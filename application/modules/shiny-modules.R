#' UI module function to include buttons to run/retrieve qPCR report from Shiny app
#' @param id module identifier
#' @param labelReport string, label for the report
#' @return tagList with:
#' \itemize{
#' \item{action button to run report execution}
#' \item{progress message output}
#' \item{download button to retrieve report}
#' }
#' @author Laure Cougnaud
runReportShinyUI <- function(id, labelReport){
	
	ns <- NS(id)
	
	tagList(
		actionButton(
			ns("createReport"), 
			label = paste("Create", labelReport, "report"), 
			style = "background-color: #d6f5f5"
		),
		uiOutput(ns("progressMessageReport")),
		br(),
		conditionalPanel(
			condition = paste0("output['", ns("reportCreated"), "'] == true "), 
			downloadButton(
				ns("getReport"), 
				label = paste("Get", labelReport, "Report"),
				style = "background-color: #b3e6ff"
			)
		)
	)

}

#' server module function to include buttons to run/retrieve qPCR report from Shiny app
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param labelReport string with label for the report
#' @param outputDir reactive expression with output directory string
#' @param params reactive expression with list of parameters filled in the interface
#' @param dataFiles reactive expression with \code{fileInput} for data files
#' @param sampleAnnotationFile reactive expression with \code{fileInput} for sample annotation file
#' @param outputSubdirectoryToKeep string with subdirectory to keep
#' for cleaning of \code{outputDir}
#' @return no returned value
#' @author Laure Cougnaud
runReportShiny <- function(input, output, session, 
	labelReport,
	outputDir, paramsUI, dataset, variables, covariate
	){
		
	outputReportName <- reactive(
		paste0(outputDir, "analysisDataset-", gsub(" ", "", paramsUI()$dataset), ".html")
	)

	output$reportCreated <- reactive(FALSE)
	
	observeEvent(input$createReport, {
				
		print(paste("report button clicked: output directory is", outputDir))		
		
		# in case previous execution
		output$reportCreated <- reactive(FALSE)
		outputOptions(output, "reportCreated", suspendWhenHidden = FALSE)
		
		
		# use if statement for error because validate doesn't work within observeEvent
		if(!is.null(dataset())){
			
			params <- paramsUI()
			params$datasetName <- params$dataset
			# output directory used in report
			params$dataset <- dataset()
			params$variables <- variables()
			params$covariate <- covariate()
			params$outputPath <- outputDir
			
#				# clean results previous execution
#				filesAlreadyPresent <- list.files(outputDir(), full.names = TRUE)
#				if(!is.null(outputSubdirectoryToKeep))
#					filesAlreadyPresent <- filesAlreadyPresent[
#						filesAlreadyPresent != file.path(outputDir(), outputSubdirectoryToKeep)
#					]
#				unlink(filesAlreadyPresent, recursive = TRUE) 
			
			# create output directory
#				dir.create(params$dataDir, recursive = TRUE)
			
			# for testing/debugging
			save(params, file = file.path(outputDir, "params.RData"))
#				print(outputDir())
			
			nameReport <- "masterTemplate.Rmd"
			pathQC <- exampleRTemplatePackage::getPathTemplate(nameReport)
			
			withProgress(
					
				message = paste("Creation of the", labelReport, "report in progress"),
				
				detail = "This may take a few minutes", {
					
					pathOutput <- file.path(outputDir, nameReport)
					
					# copy start template
					file.copy(from = pathQC, to = pathOutput, overwrite = TRUE)
					
					# run template
					potentialErrorMessage <- try(
						res <- rmarkdown::render(
							input = pathOutput, 
							output_file = outputReportName(), 
							params = params,
							envir = new.env()
						)
						, silent = TRUE)
					
				}
		
			)
			
			if(inherits(potentialErrorMessage, "try-error")){
				
				output$progressMessageReport <- renderUI(
					div(strong(paste("The", labelReport, "report didn't run:", potentialErrorMessage)), 
						style = "color:red"))
				
			}else{
				
				output$progressMessageReport <- renderUI(div(
					paste("Creation of the", labelReport, "report is successful."), 
					style = "color:green"))
				
				output$reportCreated <- reactive(TRUE)
				outputOptions(output, "reportCreated", suspendWhenHidden = FALSE)
				
			}
			
		}else{
			
			output$progressMessageReport <- renderUI(
					div("Data file(s) should be provided", style = "color:red"))
			
		}
		
	})
	
	reportResults <- reactive({
		file.path(outputDir, outputReportName())
	})
	
	
	observe({
				
		output$getReport <- downloadHandler(
				
			filename = function() outputReportName(),
				
			content = function(file){
				file.copy(from = reportResults(), to = file)
			}#, contentType = "application/pdf"
		
		)
		
	})
	
}
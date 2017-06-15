#' wrapper to generate pairwise comparison plot
#' @param dataset data.frame with data
#' @param variables columns of \code{dataset} with variable for x and y
#' @param description text description of the pairwise comparison (used for the title)
#' @param colorVariable column of \code{dataset} with color variable
#' @param colorPalette string with color palette, if \code{colorVariable}
#' \itemize{
#' 	\item{is specified: }{named string with color for each level (name) of \code{colorVariable}}
#'  \item{is not specified: }{unique element}
#' }
#' @param typePlot string with type of the plot, either 'baseR' (\code{plot}),
#' 'ggplot2', 'plotly', 'rbokeh'
#' @import ggplot2
#' @return no returned value, a plot is drawn in the current window
#' @author Laure Cougnaud
#' @export
pairwiseComparisonPlot <- function(
	dataset, variables,
	description,
	colorVariable = NULL, colorPalette = "black",
	typePlot = c("baseR", "ggplot2", "plotly", "rbokeh")){
	 
	typePlot <- match.arg(typePlot)

	titlePlot <- paste("Pairwise comparison of", description)
	 
	if(typePlot == 'baseR'){
		 
		plot(dataset[, variables], pch = 19, 
			col = if(!is.null(colorVariable))	colorPalette[dataset[, colorVariable]]	else colorPalette,
			main = titlePlot)
		 
	}else if(typePlot %in% c("ggplot2", "plotly")){
		
		if(!requireNamespace("ggplot2", quietly = TRUE))
			stop(paste("The package 'ggplot2' need to be loaded to create the plot."))
		 
		 library(ggplot2)
		 aesStringsArgs <- c(
			list(
				x = variables[1],
				y = variables[2]
			),
			if(!is.null(colorVariable))
				list(col = colorVariable)	 
		)
		 
		 gg <- ggplot(data = dataset) + # dataset[, c(variables, colorVariable)]
			 geom_point(do.call("aes_string", aesStringsArgs)) +
			 scale_colour_manual(values = colorPalette) +
			 ggtitle(titlePlot)
		 
		 if(typePlot == "ggplot2")	print(gg)
		 
		 if(typePlot == "plotly"){
			 
			 if(!requireNamespace("plotly", quietly = TRUE))
				 stop(paste("The package 'plotly' need to be loaded to create the plot."))
			 
			 plotly::ggplotly(p = gg)
			 
		 }
		 
	 }else if(typePlot == "rbokeh"){
		 
		if(!requireNamespace("rbokeh", quietly = TRUE))
			stop(paste("The package 'rbokeh' need to be loaded to create the plot."))
		
		color <- if(!is.null(colorVariable))	colorVariable	else colorPalette
		
		f <- rbokeh::figure(title = titlePlot)
				
#		rownames(dataset) <- paste0("sample", 1:nrow(dataset))
		
		rbokeh::ly_points(
			 fig = f,
			 data = dataset, #dataset[, c(variables, colorVariable)]
			 x = variables[1],  
			 y = variables[2],
			 # currently cannot specify an aesthetic and palette in rbokeh
			 color = color
#			 hoover = dataset
		)
	 
 	}
	
}



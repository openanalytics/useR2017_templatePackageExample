shinyUI(
		
	fluidPage(
			
		titlePanel("Example of R template package"),
			
		theme = "custom.css",
		
		h2("Data"),
		selectInput("dataset", "Please select dataset",
			choices = c("iris", 'warpbreaks', 
				'Pima Indians Diabetes', 'US states'
			)
		),
		
		h2("Input parameters"),
		
		h3("Boxplot of variables"),
		checkboxInput(inputId = "boxPlot", "Visualize variable distribution with a boxplot", value = TRUE),
		
		h3("Histogram of each variable by covariate"),
		checkboxInput(inputId = "histogram", "Visualize the distribution of each variable by covariate", value = TRUE),
		
		
		conditionalPanel("output.moreThanOneVariable",
				h3("Pairwise comparison plot"),
			checkboxInput(inputId = "pairwiseComparison", "Visualize pairs scatterplot of the variables", value = TRUE),
			selectInput("typePlot", "Library used for the visualization",
				c("static with base R" = "baseR", 'static with ggplot2' = "ggplot2", 
					'interactive with rbokeh' = "rbokeh", 'interactive with plotly' = "plotly"))
		),
		
		conditionalPanel("output.moreThanTwoVariables",
			h3("Linear discriminant analysis"),
			checkboxInput(inputId = "includeLda", "Include linear discriminant analysis", value = TRUE)
		),
		
		h2("Reporting"),
		# call module
		runReportShinyUI(id = "analysis", labelReport = "analysis")

	)
	
)

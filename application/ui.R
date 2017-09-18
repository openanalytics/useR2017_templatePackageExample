shinyUI(
		
	navbarPage(
			
		"Example of R template package",
			
		theme = "custom.css",
		
		tabPanel(
				
			title = "Data reporting",
			
			helpText(paste("This application demonstrates the use of a R template package to",
				"create semi-automate template analysis report.")),
		
			h2("Data"),
			helpText(paste("Please select dataset for the demonstration.")),
			selectInput("dataset", "Dataset of interest",
				choices = c("iris", 'warpbreaks', 
					'Pima Indians Diabetes', 'US states'
				)
			),
			
			h2("Input parameters"),
			helpText(paste("Please select input parameters for the choice of visualizations.",
				"These parameters will be passed to the implemented template.")),
			
			h3("Boxplot of variables"),
			checkboxInput(inputId = "boxPlot", 
				"Visualize variable distribution with a boxplot", 
				value = TRUE, width = '100%'),
			
			h3("Histogram of each variable by covariate"),
			checkboxInput(inputId = "histogram", 
				"Visualize the distribution of each variable by covariate", 
				value = TRUE, width = '100%'),
			
			
			conditionalPanel("output.moreThanOneVariable",
					h3("Pairwise comparison plot"),
				checkboxInput(inputId = "pairwiseComparison", 
					"Visualize pairs scatterplot of the variables", 
					value = TRUE, width = '100%'),
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
	
)

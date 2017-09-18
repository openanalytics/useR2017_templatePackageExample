shinyServer(function(input, output, session) {
			
	includeCSS("./www/custom.css")
	
	# load dataset
	dataset <- reactive({
		switch(input$dataset,
			'iris' = {
				data(iris)
				iris
			},
			'warpbreaks' = {
				data(warpbreaks)
				warpbreaks$warp <- factor(with(warpbreaks, paste0(wool, tension)))
				warpbreaks
			},
			'Pima Indians Diabetes' = {
				library(mlbench)
				data(PimaIndiansDiabetes)
				PimaIndiansDiabetes
			},
			'US states' = {
				
				data(state)
				
				# input parameters - data
				dataset <- data.frame(state.x77, 
					region = state.region, 
					division = state.division
				)

			}
			
			
		)			
				
	})

	# specify variables
	variables <- eventReactive(
		dataset(), {
		switch(input$dataset,	
			'iris' = grep("Species", colnames(dataset()), invert = TRUE, value = TRUE),
			'warpbreaks' = "breaks",
			'Pima Indians Diabetes' = grep("diabetes", colnames(dataset()), invert = TRUE, value = TRUE),
			'US states' = grep("region|division", colnames(dataset()), invert = TRUE, value = TRUE)
		)	
	})

	output$moreThanOneVariable <- reactive(print(length(variables()) > 1))
	outputOptions(output, "moreThanOneVariable", suspendWhenHidden = FALSE)
	
	output$moreThanTwoVariables <- reactive(print(length(variables()) > 2))
	outputOptions(output, "moreThanTwoVariables", suspendWhenHidden = FALSE)
	
	
	# specify covariate
	covariate <- eventReactive(
		dataset(), {
			switch(input$dataset,	
				'iris' = "Species",
				'warpbreaks' = "warp",
				'Pima Indians Diabetes' = "diabetes",
				'US states' = "region"
			)	
		})


	callModule(
		# module specific
		module = runReportShiny, id = "analysis",
		labelReport = "analysis",
		# parameters not in modules
		outputDir = outputDir,
		paramsUI = reactive(reactiveValuesToList(input)),
		dataset = dataset,
		variables = variables,
		covariate = covariate
	)	

})


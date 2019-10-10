optimizeLogitStochasticGradient <- 
  function(dataset, gradientStepSize, batchSize, numberOfSteps,
      initialParameters = NA, preserveHistory = TRUE, historyFrequency = 500) {
  
    # Identifying number of variables.
    numberOfVariables <- (dataset %>% ncol()) - 1
    
    # Setting initial parameters vector.
    if(is.na(initialParameters)){
      parameters <- rnorm(5)
    } else {
      parameters <- initialParameters 
    }
    
    # Creating object for parameters history if needed.
    if(preserveHistory == TRUE){
      historicalParameters      <- 
        matrix(NA, numberOfSteps / historyFrequency + 1, 5) %>%
        as_tibble()
      
      historicalParameters[1, ] <- parameters
    } else {
      historicalParameters <- NA
    }
    
    # Optimizing using stochastic gradient.
    for (i in 2:numberOfSteps) {
      parameters <- parameters + (gradientStepSize / batchSize) *
        getGradientLogLikelihood(sample_n(dataset, batchSize), parameters)
      
      if(i %% historyFrequency == 0){
        historicalParameters[ i / historyFrequency + 1, ] <- parameters
      }
    }
    
    list(parameters = parameters,
         historicalParameters = historicalParameters)
}
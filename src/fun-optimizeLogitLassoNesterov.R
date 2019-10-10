optimizeLogitLassoNesterov <- 
  function(dataset, gradientStepSize, regularizationParameter, numberOfSteps,
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
    
    # Optimizing using Nesterov acceleration.
    for (i in 2:numberOfSteps) {
      parameters <- getProximalGradient(parameters, regularizationParameter, gradientStepSize)

      x <- getProximalGradient(parametersPrevious + gradientStepSize *
                                 getGradientLogLikelihood(dataset, parametersPrevious),
                               regularizationParameter,
                               gradientStepSize)
      
      t          <- (1 + sqrt(1 + 4 * tPrevious^2)) / 2
      parameters <- x + ((tPrevious - 1) / t) * (x - xPrevious)

      xPrevious          <- x
      tPrevious          <- t
      parametersPrevious <- parameters
      
      if(i %% historyFrequency == 0){
        historicalParameters[ i / historyFrequency + 1, ] <- parameters
      }
    }
    
    list(parameters = parameters,
         historicalParameters = historicalParameters)
}
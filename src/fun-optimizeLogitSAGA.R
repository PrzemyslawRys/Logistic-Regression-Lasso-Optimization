optimizeLogitSAGA <- 
  function(dataset, gradientStepSize, numberOfSteps,
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
    
    # Creating current partial derivatives vector.
    derivatives <- getGradientLogLikelihood(dataset, parameters) %>% as.numeric()
    
    # Optimizing using SAGA algorithm.
    for (i in 2:numberOfSteps) {
      
      # Drawing coordinate from the uniform distribution.
      j <- sample(numberOfVariables, 1)
      
      # Calculating partial derivative.
      temporaryDerivatives    <- derivative
      temporaryDerivatives[j] <- getGradientLogLikelihood(dataset, parameters)[j] %>% as.numeric()
      
      # Making SAGA step.
      parameters <- parameters + gradientStepSize * (temporaryDerivatives - derivatives +
                                derivatives / M)
      derivatives <- temporaryDerivatives
      
      if(i %% historyFrequency == 0){
        historicalParameters[ i / historyFrequency + 1, ] <- parameters
      }
    }
    
    list(parameters = parameters,
         historicalParameters = historicalParameters)
  }
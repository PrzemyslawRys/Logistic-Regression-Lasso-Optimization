generateRandomLogitDataset <- 
  function(numberOfVariables, numberOfObservations,
           parameters = NA, covarianceMatrix = NA){
    # Generating correlation matrix if needed.
    if(any(is.na(covarianceMatrix))){
      covarianceMatrix <-
        matrix(rnorm(numberOfVariables * 10),
                     10, numberOfVariables) %>%
        cov()
    }
    
    # Generating random parameters vector if needed.
    if(any(is.na(parameters))){
      parameters <- (rnorm(numberOfVariables) * 10) %>%
        floor()
    }
    
    # Generating random dataset from logit model.
    dataset <- MASS::mvrnorm(numberOfObservations,
                       2 * runif(numberOfVariables) - 1,
                       covarianceMatrix)
    
    dataset %>%
      as_tibble() %>%
      mutate(p =  1 / (1 + exp(-t(parameters * t(dataset[, -ncol(dataset)])) %>%
                                 rowSums())),
             randomUniform = runif(nrow(dataset)),
             Y         = ifelse(randomUniform < p, 1, 0)) %>% 
      dplyr::select(-p, -randomUniform)
  }

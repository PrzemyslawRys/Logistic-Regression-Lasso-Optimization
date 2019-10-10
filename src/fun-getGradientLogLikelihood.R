getGradientLogLikelihood <- function(dataset, parameters) {
  parameters <- parameters %>%
    as.numeric()
  
  names(dataset)[ncol(dataset)] <- "Y"
  
  dataset <-
    dataset %>%
    mutate(p =  1 / (1 + exp(-t(parameters * t(dataset[, -ncol(dataset)])) %>%
                               rowSums())),
           diff = Y - p)
  
  (dataset$diff * dataset[1:(ncol(dataset) - 3)]) %>%
    colSums() 
}


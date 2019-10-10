getProximalGradient <- function(y, lambda, gamma) {
  sign(y) * (abs(y) - lambda * gamma)
}
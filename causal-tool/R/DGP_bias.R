# this is the data generating functions for the bias page
means_bias_DGP <- function(means_bias_select_n, means_bias_slider_smoker, means_bias_slider_error, means_bias_select_slope, means_bias_select_tau, means_bias_slider_conditional){
  # calculates the DGP for the means bias illustration
  
  N <- means_bias_select_n
  smoker <- rbinom(N, size = 1, prob = means_bias_slider_smoker)
  y_0 <- 10 + 0 + rnorm(N, 0, means_bias_slider_error)
  y_1 <- 10 + (means_bias_select_slope * smoker) + means_bias_select_tau + rnorm(N, 0, means_bias_slider_error)
  
  # randomly assign treatment but make it conditional on smoker status
  probs <- ifelse(smoker == 1, means_bias_slider_conditional, 1 - means_bias_slider_conditional)
  z <- rbinom(n = N, 1, p = probs)
  
  # add observed Y
  Y <- (y_0 * -(z - 1)) + (y_1 * z)
  
  return(tibble(smoker, y_0, y_1, z, Y, index = 1:N))
}
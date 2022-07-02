#' Simulating Gaussian Mixture
#'
#' A simulating algorithm which simulates points for a given
#' two-component gaussian mixture.
#'
#' @param mixratio The mixing ratio of the gaussian mixture.
#' @param mu1 The first mean.
#' @param sigma1 The square of the standard deviation belonging to the first cluster.
#' @param mu2 The second mean.
#' @param sigma2 The square of the standard deviation of the second cluster.
#' @param npoints the number of points for each cluster that should be generated.
#'
#' @return An atomic vector filled with the simulated data.
#'
#' @export

gaussian_mixture_sim <- function(mixratio = 0.5, mu1 = 0, sigma1 = 0.5, mu2 = 1, sigma2 = 0.5, npoints = 10){
  cluster1 <- stats::rnorm(npoints, mu1, sigma1)
  cluster2 <- stats::rnorm(npoints, mu2, sigma2)
  delta <- stats::rbinom(npoints, 1, mixratio)
  cluster <- (1-delta)*cluster1 + delta*cluster2
  return(sort(cluster))
}



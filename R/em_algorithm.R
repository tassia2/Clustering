#' EM-Algorithm
#'
#' \code{em_algorithm} takes a one dimensional dataset and gives back the variance
#'   an the expected value for a two component gaussian mixture
#'
#' @param vec The data to be clustered.
#' @param epsilon A value depicting the accuracy of the algorithm.
#'
#' @return a dataframe of length five which hast the mixing ration at the first place.
#' Second and Third place holds the first expeted value and its variance.
#' Fourth and fifth place shows the second expeted value and its variance.
#'
#'
#' @section Warning:
#' The Input needs to be one-dimensional.
#'
#'
#' @examples testdata0<-c(-0.39,0.12,0.94,1.67,1.76,2.44,3.72,4.28,4.92,5.53,
#'   0.06,0.48,1.01,1.68,1.8,3.25,4.12,4.6,5.28,6.22)
#' em_algorithm(testdata0)
#'
#' @examples testdata1<-c(0.39,0.12,0.14,0.48,0.44,1.01,1,1.59,1.6,1.67,2.44,
#'   3.2,3.22,3.25,4.12,4.28,4.52,4.6)
#' em_algorithm(testdata1)
#'
#' @examples testdata2 <- gaussian_mixture_sim(mixratio = 0.3, mu1 = 0, sigma1 = 0.5,
#'   mu2 = 3, sigma2 = 0.8, npoints = 200)
#' theta2 <- em_algorithm(testdata2)
#'
#' @export


em_algorithm <- function(vec, epsilon = 0.0001){

  if (is.list(vec)){
    if (all(sapply(vec, length) == 1)) {
      if (all(sapply(vec, is.atomic))) {
        vec <- unname(unlist(vec))
      }
    } else {
      stop("The input needs to be one dimensional.")
    }
  } else if (1 %in% dim(vec) && length(dim(vec)) < 3) {
    vec <- as.vector(vec)
  } else if (!is.vector(vec)) {
    stop("The input needs to be one dimensional.")
  }


  N <- length(vec)
  be<-sample(vec,2)
  mu1 <- be[1]

  mu2 <- be[2]

  sigma_q1 <- sum((vec-mean(vec))^2)/length(vec)
  sigma_q2 <- sum((vec-mean(vec))^2)/length(vec)
  pi1_alt <- 0
  pi1 <- 0.5
  pi2 <- 1-pi1
  theta_alt <- rep(0, 5)
  theta_neu <- c(pi1, mu1, sigma_q1, mu2, sigma_q2)

  while (eucl_dist(theta_neu, theta_alt) >= epsilon){

    theta_alt <- theta_neu

    gamma <- pi1*stats::dnorm(vec,mean=mu2,sd=sqrt(sigma_q2))/
      (pi2*stats::dnorm(vec,mean=mu1,sd=sqrt(sigma_q1))+pi1*
         stats::dnorm(vec,mean=mu2,sd=sqrt(sigma_q2)))

    mu1 <- sum((1-gamma)*vec)/sum(1-gamma)
    sigma_q1 <- sum((1-gamma)*(vec-mu1)^2)/sum(1-gamma)

    mu2 <- sum(gamma*vec)/sum(gamma)
    sigma_q2 <- sum(gamma*(vec-mu2)^2)/sum(gamma)

    pi1 <- mean(gamma)
    theta_neu <- c(pi1, mu1, sigma_q1, mu2, sigma_q2)
    df<- data.frame("Pi1"= pi1, "mu1"=mu1)

  }
  df<- data.frame("Pi1"= pi1, "mu1"=mu1, "sigma1" = sigma_q1, "mu2"= mu2, "sigma2"= sigma_q2)
  print(df)
  return(theta_neu)

}

#' Datasimulation
#'
#' A simulating algorithm which simulates normal distributed points
#' along a set of given midpoints.
#'
#' @param ncluster_points The number of points simulated in each cluster.
#'   Either a single value or a vector of values.
#' @param midpoints The points the cluster should be grouped around.
#' @param sd The standard deviations for the clusters.
#' @param dim The dimension of the simulated points.
#'
#' @return A dataframe filled with the simulated points.
#'
#' @export

data_sim <- function (ncluster_points = 10, midpoints = c(1,1),  sd = 0.5, dim = 2) {

  for (k in as.matrix(as.data.frame(midpoints))) {
    if (is.character(k)) {
        stop("Strings cannot be midpoints.")
    }
  }
  if (is.list(midpoints)){
    midpoints <- as.matrix(as.data.frame(midpoints))
  }
  else if (is.vector(midpoints)){
    if (length(midpoints) %% dim != 0){
      stop("Midpoints does not have the correct dimension.")
    }
    midpoints <- matrix(midpoints, nrow = dim)
  }
  else if (! is.matrix(midpoints)) {
    stop ("The given datatype is not supported.")
  }

  if (nrow(midpoints) != dim) {
    stop("Midpoints does not have the correct dimension.")
  }

  if (length(sd) == 1) {
    sd <- rep(sd, ncol(midpoints))
  }
  else if (ncol(midpoints) %% length(sd) == 0) {
    sd <- rep(sd, ncol(midpoints)/length(sd))
  }
  else {
    stop("Amount of standard deviations and amount of midpoints doesn't fit together.")
  }

  if (length(ncluster_points) < ncol(midpoints)){
    ncluster_points <- rep(ncluster_points, (ncol(midpoints)%/%length(ncluster_points))+1)
  }

  data_list <- list()
  for (k in 1:ncol(midpoints)) {
    data_list[[k]] <- stats::rnorm(ncluster_points[k], midpoints[1, k], sd[k])
    for (i in 2:dim) {
      data_list[[k]] <- rbind(data_list[[k]], stats::rnorm(ncluster_points, midpoints[i, k], sd[k]))
    }
  }

  data <- as.data.frame(data_list)
  return(data)

}

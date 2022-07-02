#' K-Medoids Algorithm
#'
#' @param data The data to be clustered. It needs to be a Dataframe (or matrix or list), where each column represents a point.
#' @param K The number of clusters.
#' @param medoids Initial guesses for the medoids of the clusters. Default: random guess.
#' @param tol The tolerance with respect to the matrixnorm defined by the chosen distance until the algorithm terminates.
#' @param maxIter The maximum number of iterations until the algorithm stops.
#' @param d A function depicting the distance between to points.
#'
#' @return A list of the clustering and the computed medoids. Both entries of the output are dataframes where a column represents a point and the colname depicts the cluster the point belongs to.
#'
#' @export


k_medoids <- function(data, K, medoids = gsCenters(K,data), tol = 0.01, maxIter = 10, d = eucl_dist){

  for (k in data) {
    if (is.character(k)) {
      stop("Strings cannot be clustered.")
    }
  }
  if (is.list(data)){
    data <- as.data.frame(data)
  }
  else if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  else if (is.vector(data)) {
    data <- as.data.frame(t(as.matrix(data)))
  }
  else if (!is.data.frame(data)){
    stop("The given datatype is not supported.")
  }

  test <- as.data.frame(t(data))

  if( nrow(unique(test)) < K ){
    stop("More clusters required than given different datapoints. Every datapoint is his own cluster.")
  }

  len <- length(data)
  dm <- nrow(data)
  m <- medoids

  if(ncol(m) != dm | nrow(m) != K){
    stop("Wrong dimension or quantity of assigned medoids. Points are read by column here.")
  }

  dis <- numeric(length = K)
  clusterof <- numeric(length = len)

  for(i in 1:len){
    for(j in 1:K){
      dis[j] <- d(data[i], m[j,])
    }
    index <- which.min(dis)
    clusterof[i] <- index
  }

  iter <- 0
  m_old <- numeric(length = K)

  while(maNorm(m - m_old, d) >= tol && iter < maxIter){
    m_old <- m

    for(j in 1:K){
      clusterj <- list()
      k <- 1
      for(i in 1:len){
        if(clusterof[i] == j){
          clusterj[[k]] <- data[i]
          k <- k + 1
        }
      }
      if(length(clusterj) != 0){
        cj2 <- matrix(unlist(clusterj), ncol = dm, byrow = TRUE)
      }
      dism <- matrix(nrow = nrow(cj2), ncol = nrow(cj2))
      sum <- numeric(length = nrow(cj2))
      for(i in 1:nrow(cj2)){
        for(i2 in 1:nrow(cj2)){
          dism[i,i2] <- d(cj2[i,],cj2[i2,])
        }
        sum[i] <- sum(dism[i,])
      }
      m[j,] <- cj2[which.min(sum),]
    }

    for(i in 1:len){
      for(j in 1:K){
        dis[j] <- d(data[i], m[j,])
      }
      index <- which.min(dis)
      clusterof[i] <- index
    }
    iter <- iter + 1
  }

  data <- data[, order(clusterof)]
  Cs <- paste0(rep("C",length(clusterof)),sort(clusterof))
  names(data) <- Cs

  m <- as.data.frame(t(m))
  colnames(m) <- paste("C", 1:length(m), sep = "")

  return(list("cluster" = data, "medoids" = m))
}


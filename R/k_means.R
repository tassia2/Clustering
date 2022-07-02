#' K-Means Algorithm
#'
#' @param data The data to be clustered. It needs to be a Dataframe (or matrix or list), where each column represents a point.
#' @param K The number of clusters.
#' @param means Initial guesses for the means of the clusters.
#' @param tol The tolerance to which to be clustered to with respect to the matrixnorm induced by squared euclidean distance.
#' @param maxIter The maximum number of iterations until the algorithm stops.
#'
#' @return A list list("cluster" = ..., "means" = ...) of the clustering and the computed means. Both entries of the output are dataframes
#'   where a column represents a point and the colname depicts the cluster the point belongs to.
#'
#' @export


k_means <- function(data, K, means = gsCenters(K,data), tol = 0.001, maxIter = 10){
  
  d <- ed_sq
  
  for (p in data) {
    if (is.character(p)) {
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
  m <- means
  
  if(ncol(m) != dm | nrow(m) != K){
    stop("Wrong dimension or quantity of assigned means. Points are read by column here.")
  }
  
  clusterof <- numeric(length = len)
  
  m_old <- matrix(0, nrow = K, ncol = dm)
  
  iter <- 0
  
  while(maNorm((m-m_old), d) >= tol && iter <= maxIter) {
    iter <- iter + 1
    m_old <- m
    dism <- matrix(nrow=K, ncol = len)
    
    for(i in 1:len){
      for(j in 1:K){
        dis <- d(data[i], m[j,])
        dism[j,i] <- dis
        clusterof[i] <- which.min(dism[,i])
      }
    }
    
    for(k in 1:K){
      size_k <- 0
      sum_k <- numeric(length = length(m[1]))
      for(i in 1:len){
        if(clusterof[i] == k){
          data_i <- t(data[i])
          size_k <- size_k + 1
          sum_k <- sum_k + data_i
        }
        if(size_k != 0){
          m[k,] <- ((1/size_k)*sum_k)
        }
        else{
          m[k,] <- m_old[k,]
        }
      }
    }
  }
  data <- data[, order(clusterof)]
  Cs <- paste0(rep("C",length(clusterof)),sort(clusterof))
  names(data) <- Cs
  
  m <- as.data.frame(t(m))
  colnames(m) <- paste("C", 1:length(m), sep = "")
  
  return(list("cluster" = data, "means" = m))
}
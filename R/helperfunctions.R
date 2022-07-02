#`function to calculate the euclidean distance

eucl_dist <- function (x, y) return(sqrt(sum((x - y) ^ 2)))


#`squared eulidean distance

ed_sq <- function(x,y) return(sum((x-y)^2))


#`function to randomly guess centers (k_means, k_medoids)

gsCenters <- function(k,data) {
  s <- sample(data, size = k, replace = TRUE)
  m <- matrix(0, nrow=k, ncol = nrow(data))
  for(i in 1:k) {
    m[i,] = s[,i]
  }
  return(m)
  }


#`calculating the matrixnorm of the distance d 

maNorm <- function(x,d){
  vec <- numeric(length = nrow(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      vec[j] <- d(x[,j],0)
    }
    s <- sqrt(sum(vec))
  }
  return(s)
}


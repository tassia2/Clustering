#' Agglomerative Clustering.
#'
#' \code{agglomerative_clustering} returns a clustered dataframe,
#'    where the cluster of a column is depicted in the name of the column.
#'
#' @param df The data to be clustered. Either a list, a vector,
#'   a matrix or a dataframe.
#' @param clus_type A string establishing the used clustering type.
#'   "SL" for single linkage, "CL" for complete linkage and "AL" for average linkage.
#' @param d A function computing the distance between two points.
#'
#' @return If the input is of one of the supported types and if there are
#'   not any Strings in the input the output is a dataframe where the colname
#'   of a column depicts the cluster the column belongs to.
#'
#' @export

agglomerative_clustering <- function(df, clus_type = "SL", d = eucl_dist){

  #we need a dataframe for this algorithm to work.
  #if we cant convert the data to a dataframe, we get an error
  for (k in df) {
    if (is.character(k)) {
      stop("Strings cannot be clustered.")
    }
  }
  if (is.list(df)){
    df <- as.data.frame(df)
  }
  else if (is.matrix(df)) {
    df <- as.data.frame(df)
  }
  else if (is.vector(df) ) {
    df <- as.data.frame(t(as.matrix(df)))
  }
  else if (!is.data.frame(df)){
    stop("The given datatype is not supported.")
  }

  #we cannot allow a repitition of colnames, because this could lead to errors while merging clusters.
  if (length(unique(colnames(df))) < ncol(df)) {
    new_colnames <- c()
    for (k in 1:ncol(df)) {
      new_colnames <- append(new_colnames, paste("V", k, sep = ""))
    }
    colnames(df) <- new_colnames
  }

  #single linkage
  if (identical(clus_type, "SL")) {
    dsim <- function(G, H) {
      dsim_matrix <- matrix(rep(NA, length(G)*length(H)),length(G))
      #building dissimilarity matrix for two clusters
      for (i in 1:length(G)) {
        for (j in 1:length(H)) {
          dsim_matrix[i,j] <- d(G[colnames(G)[i]], H[colnames(H)[j]])
        }
      }
      #print(dsim_matrix)
      return(min(dsim_matrix))
    }
  }
  #complete linkage
  else if (identical(clus_type, "CL")) {
    dsim <- function(G, H){
      dsim_matrix <- matrix(rep(NA, length(G)*length(H)),length(G))
      #building dissimilarity matrix for two clusters
      for (i in 1:length(G)) {
        for (j in 1:length(H)) {
          dsim_matrix[i,j] <- d(G[colnames(G)[i]], H[colnames(H)[j]])
        }
      }
      return(max(dsim_matrix))
    }
  }
  #average linkage
  else if (identical(clus_type, "AL")) {
    dsim <- function(G, H){
      dsim_matrix <- matrix(rep(NA, length(G)*length(H)),length(G))
      #building dissimilarity matrix for two clusters
      for (i in 1:length(G)) {
        for (j in 1:length(H)) {
          dsim_matrix[i,j] <- d(G[colnames(G)[i]], H[colnames(H)[j]])
        }
      }
      return(1/(length(G)*length(H))*sum(dsim_matrix))
    }
  }
  else{
    stop("The given clustering technique is not supported.")
  }

  for (k in 1:(length(df) - 2)) { #k-2 iterations to get two clusters

    #building dissimilarity matrix for all current clusters
    cluster_dsim_matrix <- matrix(rep(NA, length(unique(colnames(df)))^2),
                                  length(unique(colnames(df))))
    for (l in 1:length(unique(colnames(df)))) {
      for (m in 1:l) {
        cluster_dsim_matrix[l,m] <- dsim(df[which(colnames(df) == unique(colnames(df))[l])],
                                      df[which(colnames(df) == unique(colnames(df))[m])])
      }
    }
    #we dont need to merge one cluster with itself
    diag(cluster_dsim_matrix) <- -1
    cluster_dsim_matrix[is.na(cluster_dsim_matrix)] <- -1
    #which clusters are least dissimilar?
    pos <- which(cluster_dsim_matrix == min(cluster_dsim_matrix[cluster_dsim_matrix >= 0]),
                 arr.ind = TRUE)[1,]
    #merge those clusters
    colnames(df)[colnames(df) == unique(colnames(df))[max(pos)]] <- unique(colnames(df))[min(pos)]
  }

  cluster <- df[which(colnames(df) == unique(colnames(df))[1])]
  colnames(cluster) <- rep(c("C1"),length(colnames(df[which(colnames(df) == unique(colnames(df))[1])])))
  cluster <- cbind(cluster, df[which(colnames(df) == unique(colnames(df))[2])])
  colnames(cluster)[colnames(cluster) != "C1"] <- rep(c("C2"),length(colnames(cluster))-length(colnames(cluster)[colnames(cluster) == "C1"]))
  return(cluster)

}

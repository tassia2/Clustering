% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agglomerative_clustering.R
\name{agglomerative_clustering}
\alias{agglomerative_clustering}
\title{Agglomerative Clustering.}
\usage{
agglomerative_clustering(df, clus_type = "SL", d = eucl_dist)
}
\arguments{
\item{df}{The data to be clustered. Either a list, a vector,
a matrix or a dataframe.}

\item{clus_type}{A string establishing the used clustering type.
"SL" for single linkage, "CL" for complete linkage and "AL" for average linkage.}

\item{d}{A function computing the distance between two points.}
}
\value{
If the input is of one of the supported types and if there are
  not any Strings in the input the output is a dataframe where the colname
  of a column depicts the cluster the column belongs to.
}
\description{
\code{agglomerative_clustering} returns a clustered dataframe,
   where the cluster of a column is depicted in the name of the column.
}

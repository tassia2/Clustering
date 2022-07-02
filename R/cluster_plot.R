#' Plotfunction for Clusters
#'
#' This plot function supports one-dimensional gaussian mixture clustering
#' as well as multidimensional k-means, k-medoids and agglomerative clustering.
#' If multidimensional data is given as input the plot projects onto the first
#' two dimensions.
#'
#' @param df The daraframe containing the Clusters. Each column is
#'   interpreted as one point an the colname is the name of the
#'   Cluster the column belongs to.
#' @param cluster_center A dataframe of the same configuration as
#'   the data depicting the compute clustercenters.
#' @param nclusters The number of clusters to be plotted.
#' @param g_mixture A logical vlaue. "TRUE" only used for two
#'   component gaussian mixture.
#' @param theta_comp The values of the computed gaussian mixture
#'   in the following order: Mixing ratio, expected value of the
#'   first cluster, the belonging standard deviation, the expected
#'   value pf the second cluster and the belonging standard deviation.
#' @param theta_cor The correct gaussian mixture in the same order as
#'   \code{theta_comp}.
#' @param x_label A custom label for the x axis.
#' @param y_label A custom label for the y axis.
#'
#' @details If g_mixture is FALSE you are able to input custom cluster_centers.
#'    If you decide not to, the cluster means are computed and plotted.
#'    If you want to plot a previously computed gaussian mixture g_mixture needs
#'    to be TRUE. In the plot you will get the responsibiliity curve of your two
#'    clusters as well as the computed density. Should you know the correct
#'    means and standard deviations of your data (e.g. if you did simulate them
#'    yourself) you can plot the correct gaussian mixture as well by setting
#'    theta_cor on a vector of your computed values.
#'
#' @import ggplot2
#'
#' @export

cluster_plot <- function (df, cluster_center = NULL, nclusters = 2, g_mixture = FALSE,
                         theta_comp = c(0.5, 0, 0.1, 1, 0.1), theta_cor = NULL,
                         x_label = "X-Achse", y_label = "Y-Achse") {
  if (isTRUE(g_mixture)) {

   g_mix_fun <- function (x, theta) {
     return((1-theta[1])*stats::dnorm(x, theta[2], theta[3]) +
              theta[1]*stats::dnorm(x, theta[4], theta[5]))
   }
   responsibility <- function (x, theta) {
     theta[1]*stats::dnorm(x,mean=theta[4],sd=sqrt(theta[5]))/
       ((1-theta[1])*stats::dnorm(x,mean=theta[2],sd=sqrt(theta[3]))+theta[1]*stats::dnorm(x,mean=theta[4],sd=sqrt(theta[5])))
   }

   df <- as.data.frame(df)
   df <- cbind(df, as.data.frame(sapply(df, function(x) responsibility(x, theta_comp))))
   df <- cbind(df, as.data.frame(rep(0, nrow(df))))
   colnames(df) <- list("V1", "V2", "V3")
   g_mix_data <- as.data.frame(seq(min(df["V1"], max(df["V1"]), length.out = 1000)))
   g_mix_data <- rbind(rep(0, 1000))
   g_mix_data <- as.data.frame(g_mix_data)
   colnames(g_mix_data)[1] <- "V1"
   colnames(g_mix_data)[2] <- "V2"
   theta_comp <- as.vector(theta_comp)

   c_plot <- ggplot(df)

   if (!is.null(theta_cor)) {
     theta_cor <- as.vector(theta_cor)
     c_plot <- c_plot + stat_function(g_mix_data, mapping =
                                        aes(colour = "Correct Mixture"),
                                      fun = g_mix_fun, args = list(theta_cor),
                                      show.legend = TRUE)
   }

   c_plot <- c_plot + stat_function(g_mix_data, mapping =
                                    aes(color = "Computed Mixture"),
                                    fun = g_mix_fun, args = list(theta_comp),
                                    show.legend = TRUE) +
     stat_function(g_mix_data, mapping = aes(color = "Responsibility"),
                   fun = responsibility, args = list(theta_comp), linetype = "dotdash",
                   show.legend = TRUE) +
     geom_point(mapping = aes(V1, V2, color = "Data"), color = "blue", show.legend = FALSE) +
     geom_point(mapping = aes(V1, V3, color = "Data"), color = "blue", show.legend = FALSE) +
     xlab(x_label) + ylab(y_label)

   if (is.null(theta_cor)) {
     c_plot <- c_plot + scale_colour_manual(name = element_blank(), values =c("red", "blue"),
                           labels = c("Computed Mixture", "Responsibility"),
                           guide = guide_legend(override.aes = list(
                             linetype = c("solid", "dotdash"),
                             shape = c(NA, NA))))
   } else {
     c_plot <- c_plot + scale_colour_manual(name = element_blank(), values =c("red", "green", "blue"),
                           labels = c("Computed Mixture", "Simulated Mixture", "Responsibility"),
                           guide = guide_legend(override.aes = list(
                             linetype = c("solid", "solid", "dotdash"),
                             shape = c(NA, NA, NA))))
   }

   print(c_plot)

  } else {
    if (length(unique(colnames(df))) < nclusters){
      nclusters <- length(unique(colnames(df)))
    }
    cluster_list <- list()
    for (k in 1:nclusters) {
      cluster_list[[k]] <- as.data.frame(t(df[which(colnames(df) == unique(colnames(df))[k])]))
      colnames(cluster_list[[k]])[1] <- "V1"
      colnames(cluster_list[[k]])[2] <- "V2"
    }

    if (nclusters < 5) {
      plot_colors <- grDevices::rainbow(4)
    } else {
      plot_colors <- grDevices::rainbow(nclusters)
    }

    c_plot <- ggplot() + scale_fill_gradient(low = "grey90", high = "grey35") +
      theme(panel.background = element_rect(fill = "grey95"))

    for (k in 1:nclusters) {
      if (nrow(cluster_list[[k]]) > 2) {
        c_plot <- c_plot + stat_density_2d(cluster_list[[k]], mapping =
                                            aes(V1, V2, fill = ..level..), geom =
                                             "polygon", show.legend = FALSE)
      }
    }
    if (is.null(cluster_center)) {
      cluster_center <- list()
      for (k in 1:nclusters) {
        cluster_center[[k]] <- as.data.frame(t(as.data.frame(colMeans(cluster_list[[k]]))))
        colnames(cluster_center[[k]])[1] <- "V1"
        colnames(cluster_center[[k]])[2] <- "V2"
        c_plot <- c_plot + geom_point(cluster_center[[k]], mapping = aes(V1, V2), color = plot_colors[k],
                                     shape = 18, size = 4)
      }
    } else {
      cluster_center_list <- list()
      for (k in 1:nclusters) {
        cluster_center_list[[k]] <- as.data.frame(t(cluster_center[colnames(cluster_center)[k]]))
        colnames(cluster_center_list[[k]])[1] <- "V1"
        colnames(cluster_center_list[[k]])[2] <- "V2"
        c_plot <- c_plot + geom_point(cluster_center_list[[k]], mapping = aes(V1, V2), color = plot_colors[k],
                                     shape = 18, size = 4)
      }
    }

    for (k in 1:nclusters) {
      c_plot <- c_plot + geom_point(cluster_list[[k]], mapping =
                                    aes(V1, V2), color = plot_colors[k])
    }

    c_plot <- c_plot + xlab(x_label) + ylab(y_label)
    print(c_plot)
  }
}

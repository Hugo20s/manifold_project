

library(Rtsne)
library(factoextra)
library(Rdimtools)
library(stats)
library(vegan)

#--------------------------- SIMULATED DATAS -------------------

#' Function to create the SwissRoll Dataset in 3D
#'
#' @param n : number of points in the dataset
#'
#' @return list: a list with the values of the coordinates points in swissroll and their class
#' @export
#'
#' @examples
generateSwissRoll <- function(n) {
  #inspired by sklearn.datasets.make_swiss_roll function
  t <- 0.75 * pi * (3 * runif(n))
  x <- t  * cos(t)
  y <- 21 * runif(n)
  z <- t  * sin(t) * sample(c(-1, 1), 1)

  swissroll <- cbind(x, y, z)

  noise <- cbind(rnorm(n, 0, 0.05),
                 rnorm(n, 0, 0.05),
                 rnorm(n, 0, 0.05))

  swissroll <- swissroll + noise

  return(list(swissroll = swissroll, t = t ))

}
#' Function to create the Borromean rings Dataset in 3D
#'
#' @param n : number of points in the dataset
#'
#' @return list: a list with the values of the coordinates points and their color class
#' @export
#'
#' @examples
genrateBorromeanRing <- function(n) {

  t <- seq(0, 2*pi, length.out = n )

  a <- 1.5
  b <- 1
  #first ring
  x1 <- a * cos(t)
  y1 <- b * sin(t)
  z1 <- rep(0, n)

  #second ring
  x2 <- b * sin(t)
  y2 <- rep(0, n)
  z2 <- a * cos(t)

  #third ring
  x3 <- rep(0, n)
  y3 <- a * cos(t)
  z3 <- b * sin(t)

  x <- c(x1, x2, x3)
  y <- c(y1, y2, y3)
  z <- c(z1, z2, z3)

  X <- cbind(x, y, z)

  color <- c(rep.int(1,n), rep.int(2,n), rep.int(3,n))

  return(list(data = X, color = color))
}

#' Function to create the sphere Dataset in 3D
#'
#' @param n : number of points in the dataset
#' @param r: size of the radius of the sphere
#'
#' @return the 3d coordinates of the dataset
#' @export
#'
#' @examples
generateSphere <- function(n, r){
  teta <- runif(n, 0, 2*pi)
  phi <- runif(n, 0 , pi)

  r <- r * runif(n, 0.0, 1.0)^(1.0/3.0)
  
  x <- r*cos(teta) * sin(phi)
  y <- r*sin(teta) * sin(phi)
  z <- r* cos(phi)

  return(cbind(x, y, z))
}

#' Funtion to increase the dimension of a volume to a 10-dimensional volume
#'
#' @param n : number of points for the data
#' @param generateSphere : function generateSphere that creates the sphere volume
#' that will have the dimension increased
#'
#' @return the coordinates of the new volume in a 10-d space
#' @export
#'
#' @examples
generateintraSphere <- function(n, generateSphere) {
  #copy paste from course
  require(pdist)

  # these sensors where selected randomly
  sensors <- matrix(ncol = 3, data =
                      c(0.026, 0.236, -0.653, 0.310, 0.507, -0.270, -0.466,  -0.140, 0.353, -0.473,
                        0.241, 0.193, 0.969, 0.094, 0.756, -0.978, -0.574, -0.502, -0.281, 0.993,
                        0.026, -0.913, -0.700, 0.876, 0.216, -0.739, 0.556, -0.155, 0.431, 0.411))

  # draw random points in the sphere unit
  unitsphere <- generateSphere(n, 1)

  # We ode each point as the distance to sensors : intrinsic dimension = 3
  # while extrinsic dimension = 10
  X <- as.matrix(pdist(unitsphere, sensors))
  noise <- matrix(rnorm(ncol(X) * nrow(X), sd = .01), ncol = ncol(X))
  return(X + noise)
}



#--------------------------- MDS -------------------

#' Function to apply the mds method in the dataset
#'
#' @param data : coordinates of the data
#' @param s : dimension size that will be applied
#'
#' @return the mds result
#' @export
#'
#' @examples
reduce_dimension_mds <- function(data, s){
  dist_data <- dist(data)
  mds <- cmdscale(dist_data, s, add = TRUE)
  return(mds)
}
#--------------------------- IsoMap -------------------


#' Function to apply the Isomap method in the dataset
#'
#' @param data : coordinates of the data
#' @param s : dimension size that will be applied
#' @param n_neighboords : parameter to define the number of neighbors for the isomap algorithm
#'
#' @return the isomap reduction results
#' @export
#'
#' @examples
reduce_dimension_isomap <- function(data, s, n_neighbors){
  dist_data <- dist(data)
  iso <- isomap(dist_data, ndim=s, k= n_neighbors)
  return(iso)
}

#--------------------------- TSNE -------------------

#' Function to apply the t-SNE method in the dataset
#'
#' @param data : coordinates of the data
#' @param s :  dimension size that will be applied
#' @param p : perplexity parameter
#' @param i : number of maximum iterations
#'
#' @return result of the t-sne operation
#' @export
#'
#' @examples
reduce_dimension_tsne <- function(data, s, p=30, i = 1000){
  tsne <- Rtsne(data, dims = s, perplexity = p, verbose=FALSE, max_iter = i, check_duplicates = FALSE)
  return (tsne)
}

#--------------------------- GET DIMENSION -------------------


#Get dimension from PCA
#' Get the intrisic dimension of a manifold by doing the PCA
#'
#' @param data: data to perform the PCA
#' @param plot : boolean value to plot or not the graph
#'
#' @return : returns the number of intrisic dimensions found in PCA
#' @export
#'
#' @examples
get_dimension_pca <- function(data, plot=TRUE){
  res.pca <- prcomp(data)

  eig.val <- get_eigenvalue(res.pca)
  percentage <- eig.val$cumulative.variance.percent

  plot(percentage, xlab = "Number of Principal Components",
       ylab = "Culmulative Proportion of Variance",
       type = "b")

  #get the number of components that represent more than 80 % of the variance
  number_dimension <- min(which(percentage > 80))

  if (plot){
    plot(res.pca)
  }
  return(number_dimension)

}

#Get dimension from Correlation Dimension Estimator ####

#' Get the intrisic dimension of a manifold by doing the Correlation Estimation
#'
#' @param data: data to perform the method
#' @param plot : boolean value to plot or not the graph
#'
#' @return : returns the number of intrisic dimensions found
#' @export
#'
#' @examples
get_dimension_correlation_estimator <- function(data, plot=TRUE){
  #estimate_correlation
  result <- est.correlation(data, method = "cut")
  if (plot){
    plot(log10(result$r), log10(result$Cr),
         type = 'l', main="Correlation Estimator Plot")

    }

  return(round(result$estdim))
}




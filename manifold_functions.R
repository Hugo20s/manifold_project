#install.packages('lle')
#install.packages("Rtsne")
#install.packages('KRLS')

library(Rtsne)
library(lle)
library(KRLS)
library(factoextra)
library(Rdimtools)

options(rgl.printRglwidget = TRUE)

#--------------------------- SIMULATED DATAS -------------------

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

genrateBolloreoRing <- function(n) {
  
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

generateAnneaux <- function(n){
  
  interne <- runif(n, 78, 83)
  externe <- runif(n, 203, 220)
  angle <- runif(2*n, 0, 2*pi)
  height <- runif(n, 78, 83)
    
  x1 <- cbind(externe*cos(angle), 
              externe*sin(angle), height)
  x2 <- cbind(interne*cos(angle), 
              interne*sin(angle), height)
  x <- rbind(x1,x2)
  return(x)
  
}

generateSphere <- function(n, r, surface_only = TRUE){
  teta <- runif(n, 0, 2*pi)
  phi <- runif(n, 0 , pi)
  
  if (surface_only == FALSE) {
    radius <- r * runif(n, 0.0, 1.0)^(1.0/3.0)
  }
  
  x <- r*cos(teta) * sin(phi)
  y <- r*sin(teta) * sin(phi)
  z <- r* cos(phi)
  
  return(cbind(x, y, z))
}

generateintraSphere <- function(n, generateSphere) {
  #copy paste from course
  require(pdist)
  
  # these sensors where selected randomly
  sensors <- matrix(ncol = 3, data = 
                      c(0.026, 0.236, -0.653, 0.310, 0.507, -0.270, -0.466,  -0.140, 0.353, -0.473,
                        0.241, 0.193, 0.969, 0.094, 0.756, -0.978, -0.574, -0.502, -0.281, 0.993,
                        0.026, -0.913, -0.700, 0.876, 0.216, -0.739, 0.556, -0.155, 0.431, 0.411))
  
  # draw random points in the sphere unit
  unitsphere <- generateSphere(n, 1, surface_only=FALSE)
  
  # We ode each point as the distance to sensors : intrinsic dimension = 3
  # while extrinsic dimension = 10
  X <- as.matrix(pdist(unitsphere, sensors))
  noise <- matrix(rnorm(ncol(X) * nrow(X), sd = .01), ncol = ncol(X))
  return(X + noise)
}


#--------------------------- LLE -------------------

reduce_dimesion_lle <- function(data, s){
  all_k <- calc_k(data, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
  best_k <- which.min(unlist(all_k[2]))
  lle_res <- lle(data, s, k = best_k)
  return(lle_res)
}

#--------------------------- PCA KERNEL -------------------



reduce_dimension_pca_kernel <- function(X, s, sigma){
  A <- gausskernel(X , sigma = sigma)
  A <- -0.5*(A  - rowMeans(A) - colMeans(A) + mean(A))
  decomposition <- svd(A/nrow(A))
  
  vector <- decomposition$u[1:s,] / sqrt(decomposition$d[1:s])
  result <- A%*%t(vector)
  
  return(result)
}


#--------------------------- TSNE -------------------
reduce_dimension_tsne <- function(data, s){
  tsne <- Rtsne(data, dims = s, perplexity=30, verbose=FALSE, max_iter = 1000)
  return (tsne)
}

#--------------------------- GET DIMENSION -------------------


#Get dimension from PCA
get_dimension_pca <- function(data, plot=TRUE){
  res.pca <- prcomp(data)
  
  eig.val <- get_eigenvalue(res.pca)
  percentage <- eig.val$cumulative.variance.percent
  
  #get the number of components that represent more than 70 % of the variance
  number_dimension <- min(which(percentage > 70))
  
  if (plot){
    plot(res.pca)
  }
  return(number_dimension)
  
}

#Get dimension from Correlation Dimension Estimator ####

get_dimension_correlation_estimator <- function(data, plot=TRUE){
  #estimate_correlation
  result <- est.correlation(data, method = "cut")
  if (plot){
    plot(log(result$r), log(result$Cr), main="Correlation Estimator Plot")}
  
  return(round(result$estdim))
}



  
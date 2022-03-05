library(Rtsne)
install.packages("Rtsne")
options(rgl.printRglwidget = TRUE)

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
generateEsphere <- function(n, surface_only = TRUE){
  r <- 100
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
plot3d(esphere(1000))
plot3d(generateAnneaux(1000))

esphere_1000 <- esphere(1000)
anneaux_1000 <- generateAnneaux(1000)
#--------------------------- LLE -------------------

s <- 2
best_k <- calc_k(esphere_1000, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
a <- which.min(unlist(best_k[2]))
lle_res <- lle(esphere_1000, s, k = 20)
plot(lle_res$Y)


#--------------------------- PCA KERNEL -------------------


pca_kernel <- function(X, s, sigma){
  A <- gausskernel(X , sigma = sigma)
  A <- -0.5*(A  - rowMeans(A) - colMeans(A) + mean(A))
  decomposition <- svd(A/nrow(A))
  
  vector <- decomposition$u[1:s,] / sqrt(decomposition$d[1:s])
  return(A%*%t(vector))
}

pca_res <- pca_kernel(esphere_1000, s , 1000)
plot(pca_res)

#--------------------------- TSNE -------------------

tsne <- Rtsne(esphere_1000, dims = s, perplexity=30, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="tsne")


#--------------------------- GET DIMENSION -------------------


#Get dimension from PCA
res.pca <- prcomp(esphere_1000)
fviz_eig(res.pca)

#Get dimension from Correlation Dimension Estimator ####
corrDim <- function(data, epsilon = 10^seq(-2, 1, lengtsssh.out = 100)){
  matrix_distance <- dist(data)
  proportion <- numeric(length(epsilon))
  for (i in seq(epsilon)){
    proportion[i] <- mean(matrix_distance <= epsilon[i])
    
  }
  return (list(epsilon = epsilon, proportion = proportion ))
}

derivate <- function(x, y) {
  ll     <- length(y)
  deltax <- x[2] - x[1] # assumes equally spaced grid
  deltaf <- y[3:ll] - y[1:(ll - 2)]
  return(c(NA, deltaf / 2 / deltax, NA))
}

Xdim <- corrDim(res100)

plot(log10(Xdim$epsilon), derivate(log10(Xdim$epsilon), log10(Xdim$proportion)), 
     type = 'l')



library(microbenchmark)
#Swissroll not working 
#Put the dimension 10 as a parameters

library(dreval)
library(rgl)
options(rgl.printRglwidget = TRUE)
#----------- Estimation ----------- #
sphere_1000 <- generateSphere(1000, 10)
anneaux_1000 <- generateAnneaux(1000)
sphere_1000 <- generateSwissRoll(1000)
bollring_1000 <- genrateBolloreoRing(1000)

data <- generateAnneaux(1000)
s <- get_dimension_pca(data, TRUE)
#s <- get_dimension_correlation_estimator(data, TRUE)

pca_res <- reduce_dimension_pca_kernel(data, s , 1000)
plot(pca_res)
lle_res <- reduce_dimesion_lle(data, s)
plot(lle_res$Y)
tsne_res <- reduce_dimension_tsne(data, s)
plot(tsne_res$Y)

#------------- 
#Experiment 1 (highest number of points)

number_points <- c(100, 1000, 10000)

for (n in number_points){
  data <- generateintraSphere(n)
  s <- get_dimension_pca(data, TRUE)
  
  microbenchmark(
    reduce_dimension_pca_kernel(data, s , 1000), 
    reduce_dimesion_lle(data, s), 
    reduce_dimension_tsne(data, s), times = 5
  )
  
}

#Experiment 2: Separate center ring from outer ring (Visualization:)
data <- generateAnneaux(1000)
s <- get_dimension_correlation_estimator(data, TRUE)

pca_res <- reduce_dimension_pca_kernel(data, s , 1000)
lle_res <- reduce_dimesion_lle(data, s)
tsne_res <- reduce_dimension_tsne(data, s)


attach(mtcars)
par(mfrow=c(3,1))
plot(pca_res)
plot(lle_res$Y)
plot(tsne_res$Y)


calcTrustworthinessFromDist(data,pca_res )


swisroll_1000 <- data$swissroll
t <- data$t 
X <- genrateBolloreoRing(300)

plot3d(swissroll[order(t), ], col = rainbow(n), size = 10)
plot3d(X$data, col = X$color)
plot3d(sphere_1000)
plot3d(anneaux_1000)


#------------------ Real Data ---------------

data <- read.csv("rituximab.csv")
#take out values with target -1 
data_new <- data[data$Gate != -1, ]
X <- data_new[,1:7]
y <- data_new[,9]

get_dimension_pca(X)
get_dimension_correlation_estimator(X)

tsne <- Rtsne(X, dims = 2, perplexity=30, verbose=FALSE, max_iter = 1000)
plot(tsne$Y, col= y)







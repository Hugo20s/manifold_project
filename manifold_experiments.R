
#Swissroll not working 
#Put the dimension 10 as a parameters
library(intrinsicDimension)
library(dreval)
library(rgl)
options(rgl.printRglwidget = TRUE)

#------------- 
####### Experiment 1: Swissroll from 3d to 2d 
#Goal is to find the best visualization with an roll untangled
#------------- 

#Loading data

swissroll_1000 <- generateSwissRoll(1000)
#Plot data in 3D

data_swissroll <- swissroll_1000$swissroll
t <- swissroll_1000$t 
plot3d(data_swissroll[order(t), ], col = rainbow(length(t)), size = 10)

#Reducing data to 2-D 

s <- 2
k <- 10
all_k <- calc_k(data_swissroll, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
best_k <- which.min(unlist(all_k[2]))
lle_res <- reduce_dimesion_lle(data_swissroll, s, best_k)
tsne_res <- reduce_dimension_tsne(data_swissroll, s)
mds_res <-reduce_dimension_mds(data_swissroll, s)
iso_res <-reduce_dimension_isomap(data_swissroll, s, k)

# ----------- Comparing all the methods ------------ #
attach(mtcars)
par(mfrow=c(2,2))
plot(lle_res$Y[order(t), ], col = rainbow(length(t)) , main = "LLE", xlab="Dim1", ylab="Dim2")
plot(tsne_res$Y[order(t), ], col = rainbow(length(t)) , main = "TSNE", xlab="Dim1", ylab="Dim2")
plot(mds_res$points[order(t), ], col = rainbow(length(t)) , main = "MDS", xlab="Dim1", ylab="Dim2")
plot(iso_res$points[order(t), ], col = rainbow(length(t)) ,main = "Isomap")
par(mfrow=c(1,1))

#------------ Tuning Isomap ------------- 
#Starting with 4 -> because that is when data stops being fragmented
attach(mtcars)
par(mfrow=c(3,2))
n_neigboords <- c(6,8,10,12,14,16)
for (k in n_neigboords){
  iso_res <-reduce_dimension_isomap(data_swissroll, s, k)
  plot(iso_res$points[order(t), ], col = rainbow(length(t)) , main = paste(c("K", k), collapse=" = "))
}
mtext("Isomap", side = 3, line = 0, outer = TRUE)  
par(mfrow=c(1,1))

#------------- 
####### Experiment 2: Sphere in high-dimensional 10-d space
#Goal: Find the righ intrisic dimension for this volume
#------------- 

intraSphere <- generateintraSphere(1000, generateSphere)

#get intrisic dimension

s_pca <- get_dimension_pca(intraSphere, TRUE)
print(paste("Intrisic Dimension EStimation by PCA : ", s_pca))
s_cor <- get_dimension_correlation_estimator(intraSphere, TRUE)
print(paste("Intrisic Dimension EStimation by Correlation Estimator: ", s_cor))

if (s_pca == s_cor){
  s <- s_pca
}else { s <- s_cor}

#apply dimensionality reduction 

all_k <- calc_k(intraSphere, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
best_k <- which.min(unlist(all_k[2]))
lle_res <- reduce_dimesion_lle(intraSphere, s, best_k)
tsne_res <- reduce_dimension_tsne(intraSphere, s)
mds_res <-reduce_dimension_mds(intraSphere, s)
k <- 10
iso_res <-reduce_dimension_isomap(intraSphere, s, k)

# ----------- Comparing all the methods ------------ #

mat<-matrix(c(4,3, 2, 1),2,2,byrow = TRUE)
height<- rep.int(1, nrow(mat))
width<-rep.int(1, ncol(mat))
layout3d(mat, heights = height, widths=width, sharedMouse = TRUE)
plot3d(lle_res$Y , main = "LLE", xlab="Dim1", ylab="Dim2", zlab="Dim3")
plot3d(tsne_res$Y , main = "TSNE", xlab="Dim1", ylab="Dim2", zlab="Dim3")
plot3d(mds_res$points , main = "MDS", xlab="Dim1", ylab="Dim2", zlab="Dim3")
plot3d(iso_res$points ,main = "Isomap", xlab="Dim1", ylab="Dim2", zlab="Dim3")

#------------- 
####### Experiment 3: Rings from 3-D to 2-D
#Goal: Separate the outer ring from the inner ring 
#------------- 







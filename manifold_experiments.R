
#Swissroll not working
#Put the dimension 10 as a parameters

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
rgl.clear()
plot3d(data_swissroll[order(t), ], col = rainbow(length(t)), size = 10)

#Reducing data to 2-D

s <- 2
k <- 10
tsne_res <- reduce_dimension_tsne(data_swissroll, s)
mds_res <-reduce_dimension_mds(data_swissroll, s)
iso_res <-reduce_dimension_isomap(data_swissroll, s, k)

# ----------- Comparing all the methods ------------ #
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

#plotting the results
plot(mds_res$points[order(t), ], col = rainbow(length(t)) , main = "MDS", xlab="Dim1", ylab="Dim2")
plot(iso_res$points[order(t), ], col = rainbow(length(t)) ,main = "Isomap")
plot(tsne_res$Y[order(t), ], col = rainbow(length(t)) , main = "t-SNE", xlab="Dim1", ylab="Dim2")
par(mfrow=c(1,1))

#------------ Tuning Isomap -------------
#Starting with 6 -> because that is when data stops being fragmented
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

s_pca <- get_dimension_pca(intraSphere, FALSE)
print(paste("Intrisic Dimension EStimation by PCA : ", s_pca))
s_cor <- get_dimension_correlation_estimator(intraSphere, TRUE)
print(paste("Intrisic Dimension EStimation by Correlation Estimator: ", s_cor))

if (s_pca == s_cor){
  s <- s_pca
}else { s <- s_cor}

#apply dimensionality reduction

tsne_res <- reduce_dimension_tsne(intraSphere, s)
mds_res <-reduce_dimension_mds(intraSphere, s)
k <- 10
iso_res <-reduce_dimension_isomap(intraSphere, s, k)

# ----------- Comparing all the methods ------------ #

#plotting the results
rgl.clear()
mat<-matrix(c(1,1,2,3), 2, 2, byrow = TRUE)
height<- rep.int(1, nrow(mat))
width<-rep.int(1, ncol(mat))
layout3d(mat, heights = height, widths=width, sharedMouse = TRUE)
plot3d(tsne_res$Y , main = "t-SNE", xlab="Dim1", ylab="Dim2", zlab="Dim3")
plot3d(mds_res$points , main = "MDS", xlab="Dim1", ylab="Dim2", zlab="Dim3")
plot3d(iso_res$points ,main = "Isomap", xlab="Dim1", ylab="Dim2", zlab="Dim3")

#-------------
####### Experiment 3: Borromean Ring from 3-D to 2-D
#Goal: Separate the outer ring from the inner ring
#-------------

#generate Borromean Ring
rings <- genrateBorromeanRing(1000)

bolring <- rings$data
rgl.clear()
plot3d(bolring, col = rings$color)

#apply the reduction tecnhique
s <- 2
tsne_res <- reduce_dimension_tsne(bolring, s, 100, 100)
mds_res <-reduce_dimension_mds(bolring, s)
iso_res <-reduce_dimension_isomap(bolring, s, 200)

#plot the results
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(tsne_res$Y, col = rings$color, main = "t-SNE", xlab="Dim1", ylab="Dim2")
plot(mds_res$points, col = rings$color , main = "MDS", xlab="Dim1", ylab="Dim2")
plot(iso_res$points, col = rings$color ,main = "Isomap")
par(mfrow=c(1,1))

#------------ Tuning t-SNE -------------

tsne_res1 <- reduce_dimension_tsne(bolring, s, 50, 100)
tsne_res2 <- reduce_dimension_tsne(bolring, s, 50, 500)
tsne_res3 <- reduce_dimension_tsne(bolring, s, 100, 100)
tsne_res4 <- reduce_dimension_tsne(bolring, s, 100, 500)

attach(mtcars)
par(mfrow=c(2,2))
plot(tsne_res1$Y, col = rings$color, main = "t-SNE 50 100", xlab="Dim1", ylab="Dim2")
plot(tsne_res2$Y, col = rings$color, main = "t-SNE 50 500", xlab="Dim1", ylab="Dim2")
plot(tsne_res3$Y, col = rings$color, main = "t-SNE 100 100")
plot(tsne_res4$Y, col = rings$color, main = "t-SNE 100 500")
par(mfrow=c(1,1))


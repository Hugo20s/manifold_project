

library(microbenchmark)
library(bootcluster)
library(intrinsicDimension)
library(rgl)
options(rgl.printRglwidget = TRUE)
library(mlbench)
#------------- 
####### Comparision 1: Computation Time and Memory efficiency 
#Find less costly model
#------------- 

swissroll_1000 <- generateSwissRoll(1000)
data_swissroll <- swissroll_1000$swissroll

k <- 14
s <- 2
all_k <- calc_k(data_swissroll, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
best_k <- which.min(unlist(all_k[2]))


microbenchmark(
    reduce_dimesion_lle(data_swissroll, s, best_k),
    reduce_dimension_tsne(data_swissroll, s),
    reduce_dimension_mds(data_swissroll, s),
    reduce_dimension_isomap(data_swissroll, s, k), times = 5
)

#------------- 
####### Comparision 2: Trustworthiness & Continuity
#Find less costly model
#------------- 

result.models <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Data", "Method", "Trustworthiness", "Continuity")
colnames(result.models) <- x

s <- 2
k <- 10
datasets <- list(Swissroll = generateSwissRoll(1000)$swissroll, Sphere = generateSphere(1000, 10)
                 # Ring = generateAnneaux(1000) 
                 )

for (i in 1:length(datasets)){
  data <- datasets[[i]]
  lle_res <- reduce_dimesion_lle(data, s, 20)
  tnse <- reduce_dimension_tsne(data, s)
  mds <- reduce_dimension_mds(data, s)
  iso <- reduce_dimension_isomap(data, s, k)
  methods <- list(LLE = lle_res$Y, TSNE = tnse$Y,MDS = mds$points,ISOMAP = iso$points)
  for (j in 1:length(methods)){
 
    tru <- calcTrustworthinessFromDist(dist(data),dist(methods[[j]]),3)
    con <- calcContinuityFromDist(dist(data),dist(methods[[j]]),3)
  
    result.models[nrow(result.models) + 1,] = c(names(datasets)[i], names(methods)[j], tru, con)
  }
}
print(result.models)
#------------- 
####### Comparision 3: Real Data
#Find less costly model
#------------- 

#------------------ Real Data ---------------

#The data set wine contains a data.frame of 14 variables.
#The first variable is the types of wines. The other 13 variables are quantities of the constituents.
data(wine)

X <- wine[, 2:14]
y <- wine[, 1]


s_pca <- get_dimension_pca(X, TRUE)
print(paste("Intrisic Dimension EStimation by PCA : ", s_pca))
s_cor <- get_dimension_correlation_estimator(X, TRUE)
print(paste("Intrisic Dimension EStimation by Correlation Estimator: ", s_cor))

if (s_pca == s_cor){
  s <- s_pca
}else { s <- s_cor}

k <- 10
all_k <- calc_k(X, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
best_k <- which.min(unlist(all_k[2]))
lle_res <- reduce_dimesion_lle(X, s, best_k)
tsne_res <- reduce_dimension_tsne(X, s)
mds_res <-reduce_dimension_mds(X, s)
iso_res <-reduce_dimension_isomap(X, s, k)

# ----------- Comparing all the methods ------------ #

attach(mtcars)
par(mfrow=c(3,2))
plot(lle_res$Y, col = y , main = "LLE")
plot(tsne_res$Y, col = y , main = "TSNE")
plot(mds_res$points, col = y , main = "MDS")
plot(iso_res$points, col =y ,main = "Isomap")


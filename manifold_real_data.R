
#------------------ Real Data ---------------

#The data set wine contains a data.frame of 14 variables.
#The first variable is the types of wines. The other 13 variables are quantities of the constituents.
library(bootcluster)
data(wine)


X <- wine[, 2:14]
y <- wine[, 1]

#------------ Get instrisic dimension of real data --------
s_pca <- get_dimension_pca(X, TRUE)
print(paste("Intrisic Dimension EStimation by PCA : ", s_pca))
s_cor <- get_dimension_correlation_estimator(X, TRUE)
print(paste("Intrisic Dimension EStimation by Correlation Estimator: ", s_cor))

if (s_pca == s_cor){
  s <- s_pca
}else { s <- s_cor}

#----------- Tunning t-sne 
k <- 10
n <- nrow(X)
optPerp <- 16
mat<-matrix(c(6, 5, 4,1,2,3), 3, 3, byrow = TRUE)
height<- rep.int(1, nrow(mat))
width<-rep.int(1, ncol(mat))
attach(mtcars)
par(mfrow=c(3,2))
for(i in c(20, 60, 100, 500, 1000, 1300))
{
  tsne <- Rtsne(X, initial_dims=s,
                perplexity=optPerp, max_iter=i, check_duplicates = FALSE)
  plot(tsne$Y, col=y, xlab="tSNE1", ylab="tSNE2")
  mtext(paste0("max_iter = ", i))
}
par(mfrow=c(1,1))

# ----------- Comparing all the methods ------------ #
#---------- applying all the methods with the values tunned

tsne_res <- reduce_dimension_tsne(X, s, p=17, i = 1000)
mds_res <-reduce_dimension_mds(X, s)
iso_res <-reduce_dimension_isomap(X, s, 18)


#---------- Visualiation ----------- #
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(tsne_res$Y, col = y, main = "t-SNE", xlab="Dim1", ylab="Dim2")
plot(mds_res$points, col = y , main = "MDS", xlab="Dim1", ylab="Dim2")
plot(iso_res$points, col = y ,main = "Isomap")
par(mfrow=c(1,1))


#------------ Time ------------------- #


result <- microbenchmark(
  reduce_dimension_tsne(X, s, p=15, i = 500),
  reduce_dimension_mds(X, s),
  reduce_dimension_isomap(X, s, 18), times = 20
)
print(result)

#--------------- Trustworthiness & Continuity ---------------
methods <- list(TSNE = tsne_res$Y,MDS = mds_res$points,ISOMAP = iso_res$points)
result.models <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Method", "Trustworthiness", "Continuity")
colnames(result.models) <- x

for (j in 1:length(methods)){
  
  tru <- calcTrustworthinessFromDist(dist(X),dist(methods[[j]]),3)
  con <- calcContinuityFromDist(dist(X),dist(methods[[j]]),3)
  
  result.models[nrow(result.models) + 1,] = c( names(methods)[j], tru, con)
}

print(result.models)

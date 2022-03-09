

library(microbenchmark)
library(SingleCellExperiment)


#------------- 
####### Comparision 1: Computation Time and Memory efficiency 
#Find less costly model
#------------- 

swissroll_1000 <- generateSwissRoll(1000)
data_swissroll <- swissroll_1000$swissroll

all_k <- calc_k(data_swissroll, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
best_k <- which.min(unlist(all_k[2]))
k <- 14
s <- 2

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
datasets <- list(Swissroll = generateSwissRoll(1000)$swissroll, Sphere = generateSphere(1000, 10), Ring = generateAnneaux(1000) )

for (i in 1:length(datasets)){
  
  data <- datasets[[i]]
  all_k <- calc_k(data, s, kmin=1, kmax=30, plotres=TRUE,  parallel=TRUE, cpus=4, iLLE=FALSE)
  best_k <- which.min(unlist(all_k[2]))
  lle_res <- reduce_dimesion_lle(data, s, best_k)
  tnse <- reduce_dimension_tsne(data, s)
  mds <- reduce_dimension_mds(data, s)
  iso <- reduce_dimension_isomap(data, s, k)
  methods <- list(LLE = lle_res$Y, TNSE = tnse$Y,MDS = mds$points,ISOMAP = iso$points)
  for (j in 1:length(methods)){
 
    tru <- calcTrustworthinessFromDist(dist(data),dist(methods[[j]]),3)
    con <- calcContinuityFromDist(dist(data),dist(methods[[j]]),3)
  
    result.models[nrow(result.models) + 1,] = c(names(datasets)[i], names(methods)[j], tru, con)
  }
}

#------------- 
####### Comparision 3: Real Data
#Find less costly model
#------------- 

#------------------ Real Data ---------------

data <- read.csv("rituximab.csv")
#take out values with target -1 
data_new <- data[data$Gate != -1, ]
X <- data_new[,1:7]
y <- data_new[,9]

s_pca <- get_dimension_pca(X, TRUE)
print(paste("Intrisic Dimension EStimation by PCA : ", s_pca))
s_cor <- get_dimension_correlation_estimator(X, TRUE)
print(paste("Intrisic Dimension EStimation by Correlation Estimator: ", s_cor))

if (s_pca == s_cor){
  s <- s_pca
}else { s <- s_cor}

tsne <- Rtsne(X, dims = s, perplexity=30, verbose=FALSE, max_iter = 1000)
plot(tsne$Y, col= y)

pcaLocalDimEst(X, 'FO', alphaFO = .05, verbose = TRUE)


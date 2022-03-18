

library(microbenchmark)
#-------------
####### Comparision 1: Computation Time and Memory efficiency
#Find less costly model
#-------------

datasets <- list(Swissroll = generateSwissRoll(1000)$swissroll, Sphere = generateintraSphere(1000, generateSphere),
                 Anneau = genrateBorromeanRing(1000)$data
)


k <- c(12, 10, 110)
s <- 2

for (i in 1:length(datasets)){
  data <- datasets[[i]]
  print(names(datasets)[i])
  result <- microbenchmark(
      reduce_dimension_tsne(data, s),
      reduce_dimension_mds(data, s),
      reduce_dimension_isomap(data, s, k[i]), times = 3
  )
  print(result)
}
#-------------
####### Comparision 2: Trustworthiness & Continuity
#Find less costly model
#-------------

result.models <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Data", "Method", "Trustworthiness", "Continuity")
colnames(result.models) <- x

s <- 2
k <- c(12, 10, 110)
datasets <- list(Swissroll = generateSwissRoll(1000)$swissroll, Sphere = generateintraSphere(1000, 10),
                 Anneau = genrateBorromeanRing(1000)$data
                 )

for (i in 1:length(datasets)){
  data <- datasets[[i]]
  tnse <- reduce_dimension_tsne(data, s)
  mds <- reduce_dimension_mds(data, s)
  iso <- reduce_dimension_isomap(data, s, k[i])
  methods <- list(TSNE = tnse$Y,MDS = mds$points,ISOMAP = iso$points)
  for (j in 1:length(methods)){

    tru <- calcTrustworthinessFromDist(dist(data),dist(methods[[j]]),3)
    con <- calcContinuityFromDist(dist(data),dist(methods[[j]]),3)

    result.models[nrow(result.models) + 1,] = c(names(datasets)[i], names(methods)[j], tru, con)
  }
}
print(result.models)


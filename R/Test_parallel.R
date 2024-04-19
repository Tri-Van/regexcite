# Test Parallel
rm(list = ls())

library(doParallel)

#Change number of cores to see how it works
#cl <- detectCores()-2
cl<-makeCluster(5) # More codes, processing time decrease
registerDoParallel(cl=5, cores=14) # It seems on Window it does not care about cores (only 1 cores: the multicore-like functionality is limited to a single sequential worker on Windows systems)
#Can see by changing the number of cores, it does not affect processing time, it is by chaing cluster (cl) that calcualtion is faster
#registerDoParallel(cores=2)


#Do parallel with %dopar%
x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000
ptime <- system.time({
      r <- foreach(icount(trials), .combine=cbind) %dopar% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
   coefficients(result1)
     }
   })[3]
ptime #~14s for 1 cluster

#Do sequentially with %do% only
stime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
   ind <- sample(100, 100, replace=TRUE)
   result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
   coefficients(result1)
    }
  })[3]
stime

stopCluster(cl)

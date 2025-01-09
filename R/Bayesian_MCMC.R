pd <- function(x){
  values <- c(5,10,4,4,20,20,12,5)
  ifelse (x %in% 1:length(values), values[x],0)
}
prob_dist<-data.frame(x=1:8,prob=pd(1:8))

random_walk <- function(pd, start,num_steps){
  y <- rep(0, num_steps)
  current <- start
  for (j in 1:num_steps){
    candidate <- current + sample(c(-1,1),1)
    prob <- pd(candidate)/pd(current)
    if (runif(1)<prob) current <- candidate
    y[j] <- current
  }
  return(y)
}

out <- random_walk(pd, 4, 10000)

#rm(list=ls())
source("R/bayesian/1.Model.R")
###################################################
################### LIKELIHOOD ####################
###################################################
# This function calculates the log-prior for a parameter set or matrix of parameter sets
# excluding c, the parameter for treatment cost. c is fixed at an arbitrary value (1)
# as it has no role in the calibration.
l_likelihood <- function(par_vector) {
  # par_vector: a vector (or matrix) of model parameters
  if(is.null(dim(par_vector))) par_vector <- t(par_vector)
  llik <- rep(0,nrow(par_vector))
  for(j in 1:nrow(par_vector)) {
    jj <- tryCatch( {
      res_j <- mod(c(as.numeric(par_vector[j,]),1))
#browser() #check par_vector
      llik[j] <- llik[j]+sum(dbinom(c(25,75,50),500,res_j[["prev"]], log=TRUE))
      # prevalence likelihood
      llik[j] <- llik[j]+dnorm(10,res_j[["surv"]],2/1.96, log=TRUE)
      # survival likelihood
      llik[j] <- llik[j]+dnorm(75000,res_j[["tx"]],5000/1.96, log=TRUE)
      # treatment volume likelihood
    }, error = function(e) NA)
    if(is.na(jj)) { llik[j] <- -Inf }
  }
  return(llik)
}
# Test it
#l_likelihood(rbind(rep(0.5,7),rep(0.6,7))) # works

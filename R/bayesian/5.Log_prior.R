###################################################
##################### PRIOR #######################
###################################################
# This function calculates the log-likelihood for a parameter set or matrix of
# parameter sets excluding c, the parameter for treatment cost. c is fixed at an
# arbitrary value (1) as it has no role in the calibration.
l_prior <- function(par_vector) {
  # par_vector: a vector (or matrix) of model parameters (omits c)
  if(is.null(dim(par_vector))) par_vector <- t(par_vector)
  lprior <- rep(0,nrow(par_vector))
  lprior <- lprior+dlnorm(par_vector[,1],log(0.05 )-1/2*0.5^2,0.5,log=TRUE) # mu_e
  lprior <- lprior+dlnorm(par_vector[,2],log(0.25 )-1/2*0.5^2,0.5,log=TRUE) # mu_l
  lprior <- lprior+dlnorm(par_vector[,3],log(0.025)-1/2*0.5^2,0.5,log=TRUE) # mu_t
  lprior <- lprior+dlnorm(par_vector[,4],log(0.1 )-1/2*0.5^2,0.5,log=TRUE) # p
  lprior <- lprior+dlnorm(par_vector[,5],log(0.5 )-1/2*0.5^2,0.5,log=TRUE) # r_l
  lprior <- lprior+dlnorm(par_vector[,6],log(0.5 )-1/2*0.5^2,0.5,log=TRUE) # rho
  lprior <- lprior+dbeta( par_vector[,7],2,8,log=TRUE) # b
  return(lprior)
}
# Test it
l_prior(rbind(rep(0.5,7),rep(0.6,7))) # works

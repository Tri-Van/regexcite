rm(list=ls())
source("R/bayesian/4.Log_likelihood.R")
source("R/bayesian/5.Log_prior.R")
###################################################
################## MAP ESTIMATION #################
###################################################
# This section obtains a single best-fitting parameter set via maximum a posteriori
# estimation. To do so, an optimization algorithm is used to identify the parameter set
# that maximizes the sum of log-prior plus log-likelihood, which is equal to the log# posterior (plus a constant which can be ignored).
# Different optimization routines are tried, BFGS works best for this example.
# Function for log-posterior
l_post <- function(par_vector) {
  return( l_prior(par_vector) + l_likelihood(par_vector) )
}
# Optimize with various methods in optim tool-box
optOut_nm <- optim(rep(.5,7),l_post, control=list(fnscale=-1))
optOut_nm # est max(l_post) = -43.5
optOut_cg <- optim(rep(.5,7),l_post, method="CG",control=list(fnscale=-1))
optOut_cg # est max(l_post) = -12.0
optOut_bfgs <- optim(rep(.5,7),l_post, method="BFGS",control=list(fnscale=-1))
optOut_bfgs # est max(l_post) = -6.94
# Best fitting parameter set, incl. prior mode for c
mode_c <- exp(log(1000)-1/2*0.2^2-0.2^2)
par_map <- c(optOut_bfgs$par,mode_c)
###################################################
#################### RESULTS MAP ##################
###################################################
# Results for model with best fitting parameter set identified with MAP.
res_map <- mod(par_map,project_future = T)
res_map[["inc_cost"]] # inc cost
res_map[["inc_LY"]] # inc cost
res_map[["inc_cost"]]/res_map[["inc_LY"]] # icer

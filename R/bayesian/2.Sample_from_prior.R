###################################################
############### SAMPLE FROM PRIOR #################
###################################################
# This function returns a sample from the prior parameter distribution,
# with each column a different parameter and each row a different parameter set.
# sample.prior.srs: this uses a simple random sample of the parameter space.
# sample.prior.lhs: this uses a latin-hypercube sample of the parameter space.
## Simple random sample
sample.prior.srs <- function(n) {
  # n: the number of samples desired
#browser()
  draws <- data.frame(mu_e = rlnorm(n,log(0.05)-1/2*0.5^2,0.5),
                      mu_l = rlnorm(n,log(0.25)-1/2*0.5^2,0.5),
                      mu_t = rlnorm(n,log(0.025)-1/2*0.5^2,0.5),
                      p = rlnorm(n,log(0.1)-1/2*0.5^2,0.5),
                      r_l = rlnorm(n,log(0.5)-1/2*0.5^2,0.5),
                      rho = rlnorm(n,log(0.5)-1/2*0.5^2,0.5),
                      b = rbeta(n,2,8),
                      c = rlnorm(n,log(1000)-1/2*0.2^2,0.2)
  )
  return(as.matrix(draws))
}
## Latin hypercube sample
# install.packages("lhs")
require(lhs)
sample.prior.lhs <- function(n) {
  # n: the number of samples desired
#browser()
  draws0 <- randomLHS(n=n,k=8)
  draws <- data.frame( mu_e = qlnorm(draws0[,1],log(0.05)-1/2*0.5^2,0.5),
                       mu_l = qlnorm(draws0[,2],log(0.25)-1/2*0.5^2,0.5),
                       mu_t = qlnorm(draws0[,3],log(0.025)-1/2*0.5^2,0.5),
                       p = qlnorm(draws0[,4],log(0.1)-1/2*0.5^2,0.5),
                       r_l = qlnorm(draws0[,5],log(0.5)-1/2*0.5^2,0.5),
                       rho = qlnorm(draws0[,6],log(0.5)-1/2*0.5^2,0.5),
                       b = qbeta(draws0[,7],2,8),
                       c = qlnorm(draws0[,8],log(1000)-1/2*0.2^2,0.2)
  )
  return(as.matrix(draws))
}

# Test it
sample.prior <- sample.prior.lhs # use the lhs version as this is more efficient
sample.prior(3) # n : number of sample, 3 - draw number 3 times
sample.prior.srs(1)

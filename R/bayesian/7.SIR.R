rm(list=ls())
source("R/bayesian/4.Log_likelihood.R")
require(lhs)
set.seed(1234) # Set seed to get same results for every random draw
###################################################
###################### SIR ########################
###################################################
# This section (Section 3.5.3) obtains a sample from the posterior parameter distribution via SIR.
# First, lets redefine sample.prior to only provide samples for the first seven
# parameters, excluding c.
sample.prior <- function(n) {
  draws0 <- randomLHS(n=n,k=7)
  draws <- data.frame( mu_e = qlnorm(draws0[,1],log(0.05)-1/2*0.5^2,0.5),
                       mu_l = qlnorm(draws0[,2],log(0.25)-1/2*0.5^2,0.5),
                       mu_t = qlnorm(draws0[,3],log(0.025)-1/2*0.5^2,0.5),
                       p = qlnorm(draws0[,4],log(0.1)-1/2*0.5^2,0.5),
                       r_l = qlnorm(draws0[,5],log(0.5)-1/2*0.5^2,0.5),
                       rho = qlnorm(draws0[,6],log(0.5)-1/2*0.5^2,0.5),
                       b = qbeta(draws0[,7],2,8))
  return(as.matrix(draws))
}
# Generate 100,000 samples (of parameter set) from prior
n_samp <- 1e5
samp_i <- sample.prior(n_samp)
# Calculate likelihood for each parameter set in samp_i
llik_i <- rep(NA,n_samp)
for(i in 1:n_samp) {
#browser() #check samp_i
  llik_i[i] <- l_likelihood(samp_i[i,]) #Wrong in manuscript
  if(i/100==round(i/100,0)) {
    cat('\r',paste(i/nrow(samp_i)*100,"% done calculate likelihood",sep=""))
  }
}
# Calculate weights for resample (i.e. exponentiate the log-likelihood)
wt <- exp(llik_i-max(llik_i))
# Resample calibrated paramaters from samp_i with wt as sampling weights
id_samp <- sample.int(n_samp,replace=T,prob=wt)
# Prior mode for c
mode_c <- exp(log(1000)-1/2*0.2^2-0.2^2)
post_sir <- cbind(samp_i[id_samp,],mode_c)
# Unique parameter sets
length(unique(id_samp)) # 797# Effective sample size
sum(table(id_samp))^2/sum(table(id_samp)^2) # 88.26
# Max weight
max(table(id_samp))/sum(table(id_samp)) # 0.033
# Could use the parameter sets in post_sir to caliclate results, but lets
# try a more efficient technique: IMIS.

###################################################
################## RESULTS: SIR  ##################
###################################################
# Generate inc cost and inc LY using sir_post
IncCostC <- IncLYC <- rep(NA,nrow(post_sir))
for(i in 1:nrow(post_sir)) { # i=1
  tmp <- mod(post_sir[i,],project_future=T)
  IncLYC[i] <- tmp$inc_LY
  IncCostC[i] <- tmp$inc_cost
  if(i/100==round(i/100,0)) {
    cat('\r',paste(i/nrow(samp_i)*100,"% done simulation",sep=""))
  }
}
Calib <- list(trace0=tmp$trace0, trace1=tmp$trace1, post_sir=post_sir,IncCostC=IncCostC,IncLYC=IncLYC)
save(Calib,file="Calib_results.rData")
load("Calib_results.rData")
# Distribution of inc costs and inc LY
hist(IncLYC,breaks=100,col="blue3",border=F)
hist(IncCostC,breaks=100,col="red3",border=F)# Summary results
mean(IncLYC/1e3); quantile(IncLYC/1e3,c(1,39)/40)
# 119 (58, 240) thousands
mean(IncCostC/1e6); quantile(IncCostC/1e6,c(1,39)/40)
# 123 (-4, 312) millions
mean(IncCostC)/mean(IncLYC); quantile(IncCostC/IncLYC,c(1,39)/40)
# 947 (dominant, 2010)

#Calibrated compared to target
# Prevalence at 10,20,30 years
prev = (rowSums(Calib$trace0[,3:5])/rowSums(Calib$trace0[,1:5]))[c(10,20,30)*12]
# HIV survival without treatment
mu_b <- 0.015 # background mortality rate hard-coded as 0.015
v_mu <- c(0,0,mean(Calib$post_sir[,"mu_e"]),mean(Calib$post_sir[,"mu_l"]),mean(Calib$post_sir[,"mu_t"]))+mu_b
surv = 1/(v_mu[3]+mean(Calib$post_sir[,"p"]))+ mean(Calib$post_sir[,"p"])/(v_mu[3]+mean(Calib$post_sir[,"p"]))*(1/v_mu[4])
# Treatment volume at 30 years
tx = Calib$trace0[30*12,5]
# Calibrated parameters compared to prior
mean(Calib$post_sir[,"b"])    #mean(samp_i[,"b"])
mean(Calib$post_sir[,"mu_e"]) #mean(samp_i[,"mu_e"])
mean(Calib$post_sir[,"mu_l"]) #mean(samp_i[,"mu_l"])
mean(Calib$post_sir[,"mu_t"]) #mean(samp_i[,"mu_t"])
mean(Calib$post_sir[,"rho"])  #mean(samp_i[,"rho"])
mean(Calib$post_sir[,"p"])    #mean(samp_i[,"p"])
mean(Calib$post_sir[,"r_l"])  #mean(samp_i[,"r_l"])

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-# The end. #-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

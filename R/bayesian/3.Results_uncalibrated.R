rm(list=ls())
source("R/bayesian/1.Model.R")
source("R/bayesian/2.Sample_from_prior.R")
###################################################
################### RESULTS UNCALIBRATED ##########
###################################################
# This section uses mod and sample.prior functions to calculate results for the analysis
# via Monte Carlo simulation, without making use of the calibration data.
# Draw sample from prior
set.seed(1234) # set random seed for reproducibility
samp <- sample.prior(1e5) # 100,000 draws from prior
# Generate estimates for inc cost and inc LY via MC simulation (may take a while)
IncCost <- IncLY <- rep(NA,nrow(samp)) # vectors to collect results
for(i in 1:nrow(samp)) { # i=1
  tmp <- mod(samp[i,],project_future=T)
  IncLY[i] <- tmp$inc_LY
  IncCost[i] <- tmp$inc_cost
  if(i/100==round(i/100,0)) {
    cat('\r',paste(i/nrow(samp)*100,"% done",sep=""))
  }
}
Uncalib <- list(samp=samp,IncCost=IncCost,IncLY=IncLY)
# save(Uncalib,file="Uncalib_results.rData")
# load("Uncalib_results.rData")
### Results for uncalibrated model
IncCost <- Uncalib$IncCost
IncLY <- Uncalib$IncLY
# Distribution of inc costs and inc LYhist(IncLY,breaks=100,col="blue3",border=F)
hist(IncCost,breaks=100,col="red3",border=F)
# Summary results
mean(IncLY/1e3); quantile(IncLY/1e3,c(1,39)/40)
# 213 (6, 775) thousands
mean(IncCost/1e6); quantile(IncCost/1e6,c(1,39)/40)
# 277 (-34, 1235) millions
mean(IncCost)/mean(IncLY); quantile(IncCost/IncLY,c(1,39)/40)
# 1300 (dominant, 5720)


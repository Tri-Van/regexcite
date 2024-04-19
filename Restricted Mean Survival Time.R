rm(list=ls())

library("survival")
library("surv2sampleComp")
str(veteran)
# we need a group variable to use the surv2sample function
veteran$group <- as.numeric(veteran$karno < 70) 
rmst <- surv2sample(veteran$time, veteran$status, veteran$group, npert = 500, tau_start= 100, tau = 200)
rmst$group0

pbc.sample<-pbc.sample()
rmst1 <- surv2sample(pbc.sample$time, pbc.sample$status, pbc.sample$group, tau = 12)

### ** Examples
D=pbc.sample()
x=cbind(D$group, D$covariates)
rmstreg(D$time, D$status, x, D$group, tau=8, type="difference")
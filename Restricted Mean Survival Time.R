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

### Packages ‘rstpm2’
install.packages("rstpm2")
library(rstpm2)
data(brcancer)
# an stpm2 or pstpm2 object for "object" argument
brcancer <- transform(brcancer, recyear=rectime / 365.24)
fit <- stpm2(Surv(recyear,censrec==1)~hormon, data=brcancer, df=4)
summary(fit)
## utility
eform.coxph <- function(object) exp(cbind(coef(object),confint(object)))
fit.cox <- coxph(Surv(recyear,censrec==1)~hormon, data=brcancer)
rbind(cox=eform(fit.cox),
        + eform(fit)[2,,drop=FALSE])
#We see that the hazard ratios are very similar to the coxph model (coxph from survival package)

# estimate survival and compare with the Kaplan-Meier curves
plot(fit, newdata=data.frame(hormon=0), xlab="Time since diagnosis (years)")
lines(fit, newdata=data.frame(hormon=1), lty=2)
#Original KM: plot(survfit(Surv(recyear,censrec==1)~hormon, data=brcancer), col="blue", lty=1:2)
# Put KM on the predicted survival plot
lines(survfit(Surv(recyear,censrec==1)~hormon, data=brcancer), col="blue", lty=1:2)
legend("topright", c("PH hormon=0","PH hormon=1","KM hormon=0","KM hormon=1"),lty=1:2, col=c("black","black","blue","blue"))
# We can also calculate the hazards.
plot(fit,newdata=data.frame(hormon=1), type="hazard",xlab="Time since diagnosis (years)", ylim=c(0,0.3))
lines(fit, newdata=data.frame(hormon=0), col=2, lty=2, type="hazard")
legend("topright", c("hormon=1","hormon=0"),lty=1:2,col=1:2,bty="n")
#Hazard diff
plot(fit,newdata=data.frame(hormon=0), type="hdiff",exposed=function(data) transform(data, hormon=1),
     xlab="Time since diagnosis (years)")
#RMST
plot(fit,newdata=data.frame(hormon=1), type="rmst")
lines(fit, newdata=data.frame(hormon=0), col=2, lty=2, type="rmst")
plot(fit,newdata=data.frame(hormon=0), type="rmstdiff",exposed=function(data) transform(data, hormon=1),
     xlab="Time since diagnosis (years)")



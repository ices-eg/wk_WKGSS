library(stockassessment)
#library(tidyverse)

## Get the data

##source('data.R')

## Additional input data

## Landing frequency
lf <- array(1,dim= dim(cn_s))
dimnames(lf) <- dimnames(cn_s)

## Proportion F before spawning
pf <- array(0,dim= dim(cn_s))
dimnames(pf) <- dimnames(cn_s)

## Proportion M before spawning
pm <- array(0,dim= dim(cn_s))
dimnames(pm) <- dimnames(cn_s)

## Natural mortality 

nm <- array(0.1,dim= dim(cn_s))
dimnames(nm) <- dimnames(cn_s)


## Prepare input to the SAM call
#Alternate maturity - based on length distributions but still shallow slope
#TRY:
#- using only 1 survey
#- intermediate level of F pars
#- M = 0.1, increase with ages like in Höski's
#- 
dat <- setup.sam.data(surveys=list(smh=smh_n_s), #list(smh=smh_n_s),#should this be biomass or numbers?
                      residual.fleet=cn_s, 
                      prop.mature=mat_s, 
                      stock.mean.weight=smh_sw_s, 
                      catch.mean.weight=cw_s, 
                      dis.mean.weight=cw_s, 
                      land.mean.weight=cw_s,
                      prop.f=pf, 
                      prop.m=pm, 
                      natural.mortality=nm, 
                      land.frac=lf)



conf <- defcon(dat)
conf$fbarRange <- c(10,15)
ll <- length(conf$keyVarObs[which(conf$keyVarObs==2)])

# if(Species == 9) {
#   conf$keyVarObs[which(conf$keyVarObs==2)] <- c(4,4,rep(5,ll-2)) #1st 2 age groups (age 1 - 2) different from rest in autumn survey for variances in estimate
#   conf$keyVarObs[which(conf$keyVarObs==1)] <- c(2,2,rep(5,ll-2)) #same here but for spring survey
#   conf$keyVarObs[which(conf$keyVarObs==0)] <- c(0,0,0,0,rep(1,ll-4)) # age 1 and 2 different for commercial
#   # splitting by 0,1,2,3,4,5 delimits 5 unique variances being estimated across commercial and surveys
# }


 conf$keyLogFpar[2,21:26]<- rep(4,3)
 conf$keyLogFpar[2,18:20]<- rep(4,3)
 conf$keyLogFpar[2,15:17]<- rep(4,3)
 conf$keyLogFpar[2,12:14]<- rep(4,3)
 conf$keyLogFpar[2,10:11]<- rep(3,2)
 conf$keyLogFpar[2,7:9]<- rep(2,3)
 conf$keyLogFpar[2,4:6]<- rep(1,3)
 conf$keyLogFpar[2,1:3]<- rep(0,3)
 # conf$keyLogFsta[1,22:26]<- rep(10,2)
 # conf$keyLogFsta[1,20:21]<- rep(9,2)
 # conf$keyLogFsta[1,18:19]<- rep(8,2)
 conf$keyLogFsta[1,21:26]<- rep(4,3)
 conf$keyLogFsta[1,18:20]<- rep(4,3)
 conf$keyLogFsta[1,15:17]<- rep(4,3)
 conf$keyLogFsta[1,12:14]<- rep(4,3)
 conf$keyLogFsta[1,10:11]<- rep(3,2)
 conf$keyLogFsta[1,7:9]<- rep(2,3)
 conf$keyLogFsta[1,4:6]<- rep(1,3)
 conf$keyLogFsta[1,1:3]<- rep(0,3)
# 
 conf$keyVarLogN[1:6] <- 0
# 
 conf$keyVarObs[2,1:6] <- 2
# conf$keyVarObs[3,1:6] <- 4

## define model parameters
par <- defpar(dat,conf)

## Fit a model with SAMfit
fit2 <- sam.fit(dat,conf,par,rel.tol = 1e-10, control=list(eval.max=1000, iter.max=1000)) 

ssbplot(fit2)
fbarplot(fit2)
recplot(fit2)
catchplot(fit2)
# res <- residuals(fit2)
# plot(res)
# resp <- procres(fit2)
# plot(resp)
# retros <- retro(fit2,year=5)
# plot(retros)

#save(res, resp, file = 'fit_diag.R')



low_try <- par 
low_try$logFpar <- rep(-10, length(par$logFpar)) 
low_try$logSdLogFsta <- rep(-10, length(par$logSdLogFsta))#fit2$pl$logSdLogFsta
low_try$logSdLogN <- rep(-10, length(par$logSdLogN))#fit2$pl$logSdLogN 
low_try$logSdLogObs <- rep(-10, length(par$logSdLogObs))#fit2$pl$logSdLogObs, 
low_try$itrans_rho <- rep(0.2, length(par$itrans_rho))
low_try$logN <- par$logN -10
low_try$logF <- par$logF -10

high_try <- par 
high_try$logFpar <- rep(1, length(par$logFpar)) 
high_try$logSdLogFsta <- rep(10, length(par$logSdLogFsta))#fit2$pl$logSdLogFsta
high_try$logSdLogN <- rep(10, length(par$logSdLogN))#fit2$pl$logSdLogN 
high_try$logSdLogObs <- rep(10, length(par$logSdLogObs))#fit2$pl$logSdLogObs, 
high_try$itrans_rho <- rep(3, length(par$itrans_rho))
high_try$logN <- par$logN +10
high_try$logF <- par$logF +1

fit <- sam.fit(dat,conf,parameters = par,lower = low_try, upper = high_try, rel.tol = 1e-10, control=list(eval.max=1000, iter.max=1000)) 

ssbplot(fit)
fbarplot(fit)
recplot(fit)
catchplot(fit)
#   res <- residuals(fit)
#   plot(res)
#   resp <- procres(fit)
#   plot(resp)
#   retros <- retro(fit,year=5)
#   plot(retros)
#   
# save(res, resp, file = 'fit_diag.R')

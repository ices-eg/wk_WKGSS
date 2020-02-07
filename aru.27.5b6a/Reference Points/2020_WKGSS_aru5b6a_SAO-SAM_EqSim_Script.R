##~--------------------------------------------------------------------------
# Code to take the SAM assessment results from stockassessment.org (new TMB fits), 
# and run ICES standard EqSim reference point analyses
# D.C.M.Miller
##~--------------------------------------------------------------------------
## Issues:
# Doesn't work on old SAM fits from stockassessment.org.
# Set units by default to "tonnes" (biomass, catch etc.),"thousands" (stock numbers),"kg" (W@A)

## To Do:
# add option to simply load FLStock object (i.e. make more general)
  # Done - but not doing some of the initial plots for this setting
# double check MSY Btrigger rules
# How many simulations (noSims) are needed? Do some comparison tests
# Could pull ages and years automatically from stockassessemtn.org (or from loaded FLStock object)
# Change script to get SSB05 from teh assessment results
# EQSIM simulations run over F values seq(0,1.0,len=101). Should this be finer for values less than 0.2 to match ICES rounding rules?

###-------------------------------------------------------------------------------
### Clean slate
rm(list=ls())

##~--------------------------------------------------------------------------
##        SECTION WHERE CHANGES NEED TO BE MADE   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Directory info
path <- "D:/ICES benchmarks/WKGSS 2020/Ref pts/"   # folder were the code is and where results will be saved (in a subfolder)
runName <- "GSS5b6a_5selbio_allSRRs_TEST4" # (no spaces) Results will be saved in a subfolder with this name (so make it descriptive)
## Save plots?
savePlots <- T

##~--------------------------------------------------------------------------
## Stock and assessment
stockName <- "aru.27.5b6a"                # Used only in plots (i.e. titles) and when saving data (i.e. file names)

# If using stockassessment.org:
# if using FLStock object (below), this will be ignored
SAOAssessment <- "ARU.27.5b6a_WKGSS2020_FINAL"   # = stock name in stockassesssment.org
user <- 3                           # User 3 = Guest (ALWAYS GETS THE LATEST COMMITTED VERSION); User 2 = Anders

ages <- 5:21
years <- 1995:2018
meanFages <- c(9:21)
## Uncertainty last year
sigmaF <- NA                        # Gets from last year estimated in the assessment (SAM), unless this is specified as a value i.e. !is.na()
sigmaSSB <- 0.2   #From SAM=0.12; considered too low                      # Gets from last year estimated in the assessment (SAM), unless this is specified as a value i.e. !is.na()

##~--------------------------------------------------------------------------
## Create matrix for reference points
refPts <- matrix(NA,nrow=1,ncol=9, dimnames=list("value",c("MSYBtrigger","5thPerc_SSBmsy","Bpa","Blim","Fpa","Flim", "Fp05","Fmsy_unconstr","Fmsy")))  # "Fmsy_unconstr" is the Fmsy value without any precautionary considerations (i.e. ignore 5% P(SSB<Blim))
# Note: stores teh 'uncontrained Fmsy as well (i.e. without PA considerations)

## Enter Bpa value - Taken below from the minimum median SSB before 2017
#refPts[,"Bpa"]  <- 80000                       # Insert value for Blim, code below will calculate Bpa and MSY Btrigger

##~--------------------------------------------------------------------------
## Simulation settings
# Number of sims
noSims <- 101                                # Choose a suitable number, final run should use at least 1000, test runs coudl be done with less to save time

# SR models to use
appModels <- c("SegregBlim")   # SRR models to use  

# Which years (SSB years, not recruitment years) to exclude from the SRR fits 
rmSRRYrs <- c()                               # leave as 'c()' if the full time series is to be used (default)
#rmSRRYrs <- c(2015:2016)                     # Or specify here which other years (e.g. early period) should be left out

# Autocorrelation in recruitment?
rhoRec <- T                                   # Recruitment is strongly autocorrelated for this stockæ default=F

## Weight at age and selectivity
numAvgYrsB <- 5                               # Number of recent years to use for WAA
bioConst   <- F                            # Constant/average WAA (TRUE) or resampling from the years specified (FALSE)
numAvgYrsS <- 5                               # Number of recent years to use for selectivity
selConst   <- F                            # Constant/average selectivity (TRUE) or resampling from the years specified (FALSE)

## Forecast error (see Guidance document for details on calculation of these values)
# F
cvF  <- 0.212                                 # Default = 0.212
phiF <-	0.423                                 # Default = 0.423
# SSB
cvSSB <- 0                                    # Default = 0
phiSSB <- 0                                   # Default = 0


# 5th percentile of SSB in the final year of the assessment
SSB05<-0                                      # used in MSY Btrigger calculation. If set at 0, ignored


##~--------------------------------------------------------------------------
##        NO CHANGES NEED TO BE MADE BELOW THIS POINT   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Set working directory
setwd(path)
# create subfolder
shell(paste("md", runName, sep=" "))
setwd(paste(path,runName,"/",sep=""))

# Load libraries
# try library remotes'
#require(devtools)
#devtools::install_github("fishfollower/SAM/stockassessment")  # run this once if stoassessment has not been installed before
library(stockassessment)
#install_github("ices-tools-prod/msy") # run this once if msy has not been installed before
library(msy)
require(FLCore)

##~--------------------------------------------------------------------------
## Get fit from stockassessment.org
url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user",user,"/",SAOAssessment,"/run/", sep="")  
download.file(paste(url,"model.RData",sep=""), "model.RData")
load("model.RData") # loads 'fit' to the workspace (last baserun conducted on stockassessment.org)                  

#fitfromweb("GLL_2019_antest7")

# Check model fit
if (savePlots) x11()
plot(fit)
if (savePlots) savePlot(paste("01_",stockName,"_Assessment.png"),type="png")
if (savePlots) dev.off()
## Stock-recruitment plots
df <- data.frame(summary(fit))
ds <- dim(df)
# REC AGE MAY differ
rec <- df$R.age.5.[2:ds[1]]/1000
ssb <- df$SSB[1:(ds[1]-1)]/1000
yr  <- rownames(df)[1:(ds[1]-1)]

if (savePlots) x11()
plot(ssb,rec,type='l',ylim=c(0,1.1*max(rec)),xlim=c(0,1.1*max(ssb)),main=stockName,xlab="SSB",ylab="Recruits at age 1",cex.lab=1.5); text(ssb,rec,yr,cex=.8)
if (savePlots) savePlot(paste("02_",stockName,"_SRR.png"),type="png")
if (savePlots) dev.off()
if (savePlots) x11()
plot(yr,log(rec/ssb),type='b',main=stockName,xlab="Year",ylab="ln(Recruits/SSB) ",cex.lab=1.5)
if (savePlots) savePlot(paste("03_",stockName,"_SPR.png"),type="png")
if (savePlots) dev.off()

## Get sigmaSSB and sigmaF from the assessment fit
if (is.na(sigmaSSB)) {
  idx <- names(fit$sdrep$value) == "logssb"
  sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==max(years)] # Use last year in status table
  #sigmaSSB <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)] 
}
if (is.na(sigmaF)) {
  idx <- names(fit$sdrep$value) == "logfbar"
  #sigmaF <- fit$sdrep$sd[idx][fit$data$years==max(years)]
  sigmaF <- fit$sdrep$sd[idx][fit$data$years==(max(years)-1)]  # Use last year in status table
}

## Enter Bpa value
refPts[,"Bpa"]  <- 81156  #SSB in 2015 (lowest SSB excluding last couple of years)       # Insert value for Blim, code below will calculate Bpa and MSY Btrigger

## Calculate Blim based on sigmaSSB
refPts[,"Blim"]  <- refPts[,"Bpa"]/exp(sigmaSSB*1.645) # Used as Btrigger

##~-------------------------------------------------------------------------------
## Create FLStock object  (Note: if the assessment has been done outside of stockassessment.org, the code can be adapted here to simply read in the stock object with final assessment results)
flq <- FLQuant(NA, dimnames = list(age = ages, year = years), quant='age')
stk <- FLStock(stock.n = flq,
               name = stockName,
               desc = "FLStock_from_SAM")
units(stk)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))
# Mean F range
range(stk)[c("minfbar","maxfbar")]    <- c(min(meanFages), max(meanFages))
# Last age a plusgroup
stk  <- setPlusGroup(stk,stk@range["max"])

### Read raw data from stockassessment.org
url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user",user,"/",SAOAssessment,"/data/", sep="")  
filestoget <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat", 
                "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat", 
                "survey.dat")
d <- lapply(filestoget, function(f)download.file(paste(url,f,sep=""), f))

# add catches
#catch.n(stk)[,ac(years[1]:(max(years)-1))] <- landings.n(stk)[,ac(years[1]:(max(years)-1))] <- tmpCat; rm(tmpCat)
tmpCat <- t(read.ices("cn.dat"))
tmpLF <- t(read.ices("lf.dat"))
dms <- list(intersect(ac(ages),dimnames(tmpCat)[[1]]),intersect(years,dimnames(tmpCat)[[2]]))
catch.n(stk)[dms[[1]],dms[[2]]] <- tmpCat[dms[[1]],dms[[2]]]
catch.n(stk)[is.na(catch.n(stk))] <- 0
landings.n(stk)[dms[[1]],dms[[2]]] <- tmpCat[dms[[1]],dms[[2]]] * tmpLF[dms[[1]],dms[[2]]]
landings.n(stk)[is.na(landings.n(stk))] <- 0
discards.n(stk)[] <- catch.n(stk) - landings.n(stk)
rm(tmpCat, dms)

#catch.wt(stk)[,ac(years[1]:(max(years)-1))] <- t(read.ices("cw.dat"))[-1,]
tmpCwt <- t(read.ices("cw.dat"))
dms <- list(intersect(ac(ages),dimnames(tmpCwt)[[1]]),intersect(years,dimnames(tmpCwt)[[2]]))
catch.wt(stk)[dms[[1]],dms[[2]]] <- tmpCwt[dms[[1]],dms[[2]]]; rm(tmpCwt,dms)

tmpLwt <- t(read.ices("lw.dat"))
dms <- list(intersect(ac(ages),dimnames(tmpLwt)[[1]]),intersect(years,dimnames(tmpLwt)[[2]]))
landings.wt(stk)[dms[[1]],dms[[2]]] <- tmpLwt[dms[[1]],dms[[2]]]; rm(tmpLwt,dms)

tmpDwt <- t(read.ices("cw.dat"))
dms <- list(intersect(ac(ages),dimnames(tmpDwt)[[1]]),intersect(years,dimnames(tmpDwt)[[2]]))
discards.wt(stk)[dms[[1]],dms[[2]]] <- tmpDwt[dms[[1]],dms[[2]]]; rm(tmpDwt,dms)

discards(stk) <- computeDiscards(stk)
landings(stk) <- computeLandings(stk)
catch(stk) <- computeCatch(stk)

# add bio
tmp <- t(read.ices("mo.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
mat(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices("nm.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
m(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices("pf.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
harvest.spwn(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
tmp <- t(read.ices("pm.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
m.spwn(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)

# Update stock and fisheries from SAM fit
stock.n(stk)[] <- exp(fit$pl$logN)
tmp <- t(read.ices("sw.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
stock.wt(stk)[dms[[1]],dms[[2]]] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
stock.wt(stk)[is.na(stock.wt(stk))] <- 0.001  # Replace NA weights with something low
stock.wt(stk)[stock.wt(stk)==0] <- 0.001  # Replace 0 weights with something low
stock(stk)[] <- computeStock(stk)
# harvest is unique to the set cod (i.e. depends on config)
# check conf. file for which F states are estimated
Fstates <- fit$conf$keyLogFsta[1,]
Fstates_start <- which(Fstates==0)
#Fstates_end   <- which(Fstates==max(Fstates)) # doesn't work if variances assigned out of order
# fix for GSS5b6a:
harvest(stk)[c(1:5,7,6),] <- exp(fit$pl$logF)
harvest(stk)[as.character(12:21),] <- harvest(stk)[as.character(11),]

# year range
minYear <- range(stk)["minyear"]; maxYear <- range(stk)["maxyear"]

## Selectivity curves
if (savePlots) x11()
meanF <- apply(harvest(stk)[as.character(meanFages),],2, "mean")
sel <- sweep(harvest(stk),2,meanF,"/")
plot(ages,sel[,ac(max(years)-1)], type="l", ylim=c(0,max(sel)), xlab="Age", ylab="Selectivity", main="Selectivity")
for (i in ac((maxYear-19):maxYear)) lines(ages,sel[,i], col=i)
lines(ages,apply(sel[,ac((maxYear-2):maxYear)],1,mean), col=1, lwd=5)
lines(ages,apply(sel[,ac((maxYear-4):maxYear)],1,mean), col=2, lwd=5)
lines(ages,apply(sel[,ac((maxYear-9):maxYear)],1,mean), col=3, lwd=5)
lines(ages,apply(sel[,ac((maxYear-19):maxYear)],1,mean), col=4, lwd=5)
legend("topright", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
#legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
if (savePlots) savePlot(paste("00_",stockName,"_Selectivity.png"),type="png")
if (savePlots) dev.off()
 
## Weight at age
if (savePlots) x11()
plot(ages,stock.wt(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(stock.wt(stk))), xlab="Age", ylab="Weight (kg)", main="Weight at Age")
for (i in ac((maxYear-19):maxYear)) lines(ages,stock.wt(stk)[,i], col=i)
lines(ages,apply(stock.wt(stk)[,ac((maxYear-2):maxYear)],1,mean), col=1, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((maxYear-4):maxYear)],1,mean), col=2, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((maxYear-9):maxYear)],1,mean), col=3, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((maxYear-19):maxYear)],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
#legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
if (savePlots) savePlot(paste("00_",stockName,"_WAA.png"),type="png")
if (savePlots) dev.off()

### Trim off last year of the stock object (only if incomplete data for last assessment year)
# origStk <- stk
# stk <- window(stk, start=minYear, end=(maxYear-1))



###-------------------------------------------------------------------------------
###-------------------------------------------------------------------------------
### Set SRR Models for the simulations
#Models: "segreg","ricker", "bevholt"; or specials: "SegregBlim/Bloss" (breakpt. Blim/Bloss)

## SRR years 
# Which years (SSB years) to exclude from the SRR fits
# Keep all except last 2 (poorly estimated rec/selec)
rmSRRYrs <- union(rmSRRYrs, c(maxYear-1,maxYear))  # This removes last two years
srYears <- setdiff(c(minYear:(maxYear-1)),rmSRRYrs)


## determine segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= refPts[,"Blim"], ab$a * refPts[,"Blim"], ab$a * ssb))

## determine segreg model with Bloss breakpoint and (roughly) geomean rec above this
SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= min(ssb(stk)), ab$a * min(ssb(stk)), ab$a * ssb))


###~~~~~~~~~~~~~
## autocorrelation
ACFrec <- acf(rec(stk)[,ac(srYears)])
acfRecLag1 <- round(ACFrec$acf[,,][2],2)
if (savePlots) x11()
acf(rec(stk), plot=T, main=paste("Autocor. in Rec, Lag1 =",acfRecLag1,sep=" "))
if (savePlots) savePlot(paste("04_",stockName,"_SRautocor.png"),type="png")
if (savePlots) dev.off()

# Set a max for AC?

###-------------------------------------------------------------------------------
## Fit SRRs
FIT_segregBlim <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
#FIT_segregBloss <- eqsr_fit(stk,nsamp=noSims, models = "SegregBloss", remove.years=rmSRRYrs)
FIT_segreg <- eqsr_fit(stk,nsamp=noSims, models = "Segreg", remove.years=rmSRRYrs)
#FIT_segregAR1 <- eqsr_fit(stk,nsamp=noSims, models = "segregAR1", remove.years=rmSRRYrs)
FIT_All <- eqsr_fit(stk,nsamp=noSims, models = appModels, remove.years=rmSRRYrs)

# save model proportions and parameters:
write.csv(FIT_segregBlim$sr.det, paste(stockName,"_FIT_segregBlim_SRpars.csv",sep=""))
#write.csv(FIT_segregBloss$sr.det, paste(stockName,"_FIT_segregBloss_SRpars.csv",sep=""))
write.csv(FIT_segreg$sr.det, paste(stockName,"_FIT_segreg_SRpars.csv",sep=""))
write.csv(FIT_All$sr.det, paste(stockName,"_FIT_All_SRpars.csv",sep=""))

# Plot raw SRR results
if (savePlots) x11()
eqsr_plot(FIT_segreg,n=2e4)
if (savePlots) savePlot(paste("05ai_",stockName,"_segreg.png"),type="png")
if (savePlots) dev.off()

if (savePlots) x11()
eqsr_plot(FIT_segregBlim,n=2e4)
if (savePlots) savePlot(paste("05ai_",stockName,"_segregBlim.png"),type="png")
if (savePlots) dev.off()

if (savePlots) x11()
eqsr_plot(FIT_All,n=2e4)
if (savePlots) savePlot(paste("05b_",stockName,"_SRRall.png"),type="png")
if (savePlots) dev.off()


###-------------------------------------------------------------------------------
## Run simulations
###-------------------------------------------------------------------------------

###-------------------------------------------------------------------------------
## Simuation 1 - get Flim
# Flim is derived from Blim by simulating the stock with segmented regression S-R function with the point of inflection at Blim 
# Flim = the F that, in equilibrium, gives a 50% probability of SSB > Blim. 
# Note this simulation should be conducted with:
#  fixed F (i.e. without inclusion of a Btrigger)
#  without inclusion of assessment/advice errors. 

SIM_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB+1, maxYear), bio.const = TRUE,
                            sel.years = c(maxYear-numAvgYrsB+1, maxYear), sel.const = TRUE,
                            Fcv=0, Fphi=0, SSBcv=0,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp1 <- t(SIM_segregBlim$Refs2)
write.csv(tmp1, paste("EqSim_",stockName,"_SegregBlim_eqRes.csv",sep=""))
refPts[,"Flim"] <- tmp1["F50","catF"]

# Fpa is derived from Flim in the reverse of the way Bpa is derived from Blim. i.e.: 
tmpFpa <- refPts[,"Flim"] * exp(-sigmaF * 1.645)
if (tmpFpa<0.2) refPts[,"Fpa"] <- round(tmpFpa , 3) else refPts[,"Fpa"] <- round(tmpFpa , 2)
if (refPts[,"Flim"]<0.2) refPts[,"Flim"] <- round(refPts[,"Flim"],3) else refPts[,"Flim"] <- round(refPts[,"Flim"],2)

###-------------------------------------------------------------------------------
## Simuation 2a - get initial Fmsy
# FMSY should initially be calculated based on:
#     a constant F evaluation 
#     with the inclusion of stochasticity in population and exploitation 
#     as well as assessment/advice error. 
#     Appropriate SRRs should be specified (here using all 3)

SIM_All_noTrig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB+1, maxYear), bio.const = FALSE,
                            sel.years = c(maxYear-numAvgYrsS+1, maxYear), sel.const = FALSE,
                            Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp2 <- t(SIM_All_noTrig$Refs2)
write.csv(tmp2, paste("EqSim_",stockName,"_AllnoTrig_eqRes.csv",sep=""))
Fmsy_tmp <- tmp2["medianMSY","lanF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_noTrig,catch=TRUE)  
if (savePlots) savePlot(paste("06_",stockName,"_AllnoTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

# To ensure consistency between the precautionary and MSY frameworks, FMSY is not allowed to be above Fpa
refPts[,"Fmsy_unconstr"] <- Fmsy_tmp 
if (Fmsy_tmp > refPts[,"Fpa"]) {
  print("WHOAAA, Fmsy > Fpa!") 
  refPts[,"Fmsy"] <- refPts[,"Fpa"]
} else {
  refPts[,"Fmsy"] <- Fmsy_tmp
}


###-------------------------------------------------------------------------------
## MSY Btrigger
data.05<-SIM_segregBlim$rbp
x.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$Ftarget
b.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$p05
plot(b.05~x.05, ylab="SSB", xlab="F")
b.lm <- loess(b.05 ~ x.05)
refPts[,"5thPerc_SSBmsy"] <- predict(b.lm, refPts[,"Fmsy"])
# check if F<Fmsy last 5 years
if (sum(as.numeric(fbar(stk)[,ac((maxYear-4):maxYear)])<=refPts[,"Fmsy"])<3) {
  refPts[,"MSYBtrigger"]  <- refPts[,"Bpa"]  
} else {
# Check if Bmsy_05>Bpa
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"]>refPts[,"Bpa"],refPts[,"5thPerc_SSBmsy"],refPts[,"Bpa"])
# Check if Bmsy_05 > SSBcur_05
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"] > SSB05,max(refPts[,"Bpa"],SSB05),refPts[,"5thPerc_SSBmsy"])  
  }


###-------------------------------------------------------------------------------
## Simuation 2b - get final Fmsy
# MSY Btrigger should be selected to safeguard against an undesirable or unexpected low SSB when fishing at FMSY
# The ICES MSY AR should be evaluated to check that the FMSY and MSY Btrigger combination adheres to precautionary considerations: 
#      in the long term, P(SSB<Blim)<5%
# The evaluation must include:
#      realistic assessment/advice error
#      stochasticity in population biology and fishery exploitation.
#      Appropriate SRRs should be specified (here using all 3)

SIM_All_Trig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                          sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                          Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                          rhologRec=rhoRec,
                          Btrigger = refPts[,"MSYBtrigger"], Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                          Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp3 <- t(SIM_All_Trig$Refs2)
write.csv(tmp3, paste("EqSim_",stockName,"_AllTrig_eqRes.csv",sep=""))
refPts[,"Fp05"] <- tmp3["F05","catF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_Trig,catch=TRUE)  
if (savePlots) savePlot(paste("07_",stockName,"_AllTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
if (refPts[,"Fmsy"] > refPts[,"Fp05"]) {
  print("WHOAAA, Fmsy > Fp05!") # If Fmsy > Fp05, Fmsy = Fp05
  if (refPts[,"Fp05"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fp05"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fp05"],2)
} else {  
  if (refPts[,"Fmsy"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}
if (refPts[,"Fp05"]<0.2) refPts[,"Fp05"] <- round(refPts[,"Fp05"],3) else refPts[,"Fp05"] <- round(refPts[,"Fp05"],2)  



###-------------------------------------------------------------------------------
## Save reference points
write.csv(refPts, paste(stockName,"_RefPts.csv",sep=""))

###-------------------------------------------------------------------------------
## Save run settings
SRused <- appModels[1]
if (length(appModels)>1) for (i in 2:length(appModels)) SRused <- paste(SRused,appModels[i],sep="_")
SRyears_min <- min(srYears); SRyears_max <- max(srYears)
setList <- c("stockName", "runName", "SAOAssessment", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
runSet <- matrix(NA,ncol=1, nrow=length(setList), dimnames=list(setList,c("Value")))
for (i in setList) runSet[which(setList==i),] <- eval(parse(text = i))

write.csv(runSet, paste(stockName,"_RunSettings.csv",sep=""))

###-------------------------------------------------------------------------------
## Save workspace
save.image(file=paste(stockName,"_",maxYear,"_EqSim_Workspace.Rdata",sep=""))


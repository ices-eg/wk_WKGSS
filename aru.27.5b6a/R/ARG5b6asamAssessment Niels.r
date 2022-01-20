#-------------------------------------------------------------------------------
# FLSAM script for Greater Silversmelt in 5b/6a
#
# By Niels Hintzen, Wageningen Marine Research
#-------------------------------------------------------------------------------

rm(list=ls())

# library(devtools)
#- Install relevant packages just once
#install_github("fishfollower/SAM",ref="components") #sometimes you need to re-install TMB and/or the Matrix package too, but R will tell you
#source("http://flr-project.org/R/instFLR.R") #get FLCore and FLSAM from here

#- Load the relevant packages
library(FLCore)
library(FLSAM)

#- Set path
# dataPath  <- "C:/Downloads/user274-ARU.27.5b6a_WKGSS2020_FINAL_2/ARU.27.5b6a_WKGSS2020_FINAL_2/data/"
dataPath  <- "D:/iWKGSS 2020/06. Data/aru.27.5b6a/Model/SAM/ARU.27.5b6a_WKGSS2020_FINAL_2/data/"

# Plot time series of any slot in a stock or ica object (added 18-03-2010 by NTH)
timeseries <- function(stck.,slot.){
  assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
  print(xyplot(data~year,data=slot(stck.,slot.),
               groups=age,
               auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
               type="b",
               xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
               main=paste(stck.@name,"timeseries of",slot.),
               par.settings=list(superpose.symbol=list(
                 pch=as.character(range(stck.)["min"]:range(stck.)["max"] %% 10),
                 cex=1.25))))}



#- Load data
setwd(dataPath)
cn        <- stockassessment::read.ices("cn.dat")
cw        <- stockassessment::read.ices("cw.dat")
dw        <- stockassessment::read.ices("dw.dat")
lw        <- stockassessment::read.ices("lw.dat")
mo        <- stockassessment::read.ices("mo.dat")
nm        <- stockassessment::read.ices("nm.dat")
pf        <- stockassessment::read.ices("pf.dat")
pm        <- stockassessment::read.ices("pm.dat")
sw        <- stockassessment::read.ices("sw.dat")
lf        <- stockassessment::read.ices("lf.dat")
surveys   <- stockassessment::read.ices("survey.dat")

#- Setup stock object
dmns      <- FLQuant(dimnames=list(age=an(colnames(cn)),year=an(rownames(cn)),unit="unique",season="all",area="unique",iter=1))
ARG       <- FLStock(m=dmns)
units(ARG)[1:17]<- as.list(c(rep(c("tonnes","thousands","kg"),4),
                                 rep("NA",2),"f",rep("NA",2)))
                                 
ARG@catch.n[]       <- t(cn)
ARG@catch.n@.Data[ARG@catch.n<1] <- NA
ARG@catch.wt[]      <- t(cw)
ARG@catch           <- computeCatch(ARG)
ARG@discards.wt[]   <- t(dw)
ARG@discards.n[]    <- 0
ARG@discards[]      <- computeDiscards(ARG)
ARG@landings.wt[]   <- t(lw)
ARG@landings.n[]    <- ARG@catch.n
ARG@landings.n@.Data[ARG@landings.n<1] <- NA
ARG@landings[]      <- computeLandings(ARG)
ARG@mat[]           <- t(mo)
ARG@m[]             <- t(nm)
ARG@harvest.spwn[]  <- t(pf)
ARG@m.spwn[]        <- t(pm)
ARG@stock.wt[]      <- t(sw)
ARG@name            <- "GSS5b6a"
ARG@desc            <- "Greater Silversmelt 5b-6a"

# plot(FLIndex(index=ARG@catch.n),type="internal", main = "ARU 27.5b6a Catch at age 1995-2018")

# FLCore::plot(ARG@catch.wt, type="b")
timeseries(window(ARG,1995,range(ARG)["maxyear"]),slot="catch.wt")

# plot(FLIndex(index=trim(ARG@catch.n, year=1995:2004)),type="internal", main = "ARU 27.5b6a Catch at age 1995-2004")
# plot(FLIndex(index=trim(ARG@catch.n, year=2005:2018)),type="internal", main = "ARU 27.5b6a Catch at age 2005-2018")

#- Setup survey object
surv1               <- FLIndex(index=FLQuant(dimnames=list(age=an(colnames(surveys[[1]])),year=an(rownames(surveys[[1]])),unit="unique",season="all",area="unique",iter=1)))
surv1@index[]       <- t(surveys[[1]])
surv1@range[c("startf","endf")] <- attr(surveys[[1]],"time")
surv1@type          <- "number"
surv2               <- FLIndex(index=FLQuant(dimnames=list(age=an(colnames(surveys[[2]])),year=an(rownames(surveys[[2]])),unit="unique",season="all",area="unique",iter=1)))
surv2@index[]       <- t(surveys[[2]])
surv2@range[c("startf","endf")] <- attr(surveys[[2]],"time")
surv2@type          <- "number"
surv3               <- FLIndex(index=FLQuant(dimnames=list(age=an(colnames(surveys[[3]])),year=an(rownames(surveys[[3]])),unit="unique",season="all",area="unique",iter=1)))
surv3@index[]       <- t(surveys[[3]])
surv3@range[c("startf","endf")] <- attr(surveys[[3]],"time")
surv3@type          <- "biomass"
surv4               <- FLIndex(index=FLQuant(dimnames=list(age=an(colnames(surveys[[4]])),year=an(rownames(surveys[[4]])),unit="unique",season="all",area="unique",iter=1)))
surv4@index[]       <- t(surveys[[4]])
surv4@range[c("startf","endf")] <- attr(surveys[[4]],"time")
surv4@type          <- "biomass"

ARG.tun             <- FLIndices(FaroeseSummerSurvey=surv1,FaroeseDeepwaterSurvey=surv2,ScottishDeepwaterSurvey=surv3,CPUE=surv4)

# plot of internal consistency of surveys
# plot(ARG.tun[[1]],type="internal", main = "ARU 27.5b6a Faroese summer survey")
# plot(ARG.tun[[2]],type="internal", main = "ARU 27.5b6a Faroese deepwater survey")

#- Analyse suitable plus-group age
matplot(y=t(sweep(ARG@catch.n[,drop=T],2,colSums(catch.n(ARG),na.rm=T),"/")[ac(13:21),]),type="b",pch=ac(c(3:9,0:1)),x=1995:2018,xlab="Year",ylab="Proportion")
ARGnopg             <- ARG
pg                  <- 19
ARG                 <- setPlusGroup(ARG,pg)
ARG@catch.n[ac(pg),ac(c(2003:2007,2018))]   <- colSums(ARGnopg@catch.n[ac(pg:dims(ARGnopg)$max),ac(c(2003:2007,2018))],na.rm=T)
ARG@catch.wt[ac(pg),ac(c(2003:2007,2018))]  <- colSums(ARGnopg@catch.n[ ac(pg:dims(ARGnopg)$max),ac(c(2003:2007,2018))] *
                                               ARGnopg@catch.wt[ac(pg:dims(ARGnopg)$max),ac(c(2003:2007,2018))],na.rm=T) /
                                       colSums(ARGnopg@catch.n[ ac(pg:dims(ARGnopg)$max),ac(c(2003:2007,2018))],na.rm=T)
ARG@landings.wt     <- ARG@catch.wt
ARG@landings.n      <- ARG@catch.n
ARG@stock.wt        <- ARG@catch.wt
if(pg < dims(ARG.tun[[1]]@index)$max){
  pgTun1              <- ARG.tun[[1]]@index
  ARG.tun[[1]]        <- trim(ARG.tun[[1]],age=6:pg)
  ARG.tun[[1]]@index[ac(pg),]  <- quantSums(pgTun1[ac(pg:dims(pgTun1)$max),])
}

#- Setup control object
ARG.ctrl            <- FLSAM.control(ARG,ARG.tun)
ARG.ctrl@residuals  <- FALSE
ARG.ctrl@cor.F      <- 1

#- Run initial SAM to define states
ARG.sam             <- FLSAM(ARG,ARG.tun,ARG.ctrl)
defAIC              <- AIC(ARG.sam); defAIC
xyplot(value + lbnd + ubnd ~ age | af(year),data=f(ARG.sam),type="b",col=c(1,"grey","grey"),lty=c(2,1,1),pch=19,scale=list(y="free"))

#---------------------
#- Add correlation structure for surveys
#---------------------

ARG.ctrl@cor.obs[2,1:7]           <- c(101:106,106)
ARG.ctrl@cor.obs[3,1:9]           <- c(201:208,208)
ARG.ctrl@cor.obs.Flag[2:3]        <- as.factor("AR")
ARG.ctrl                          <- update(ARG.ctrl)
ARG.samcorObs                     <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.sam)
corObsAIC                         <- AIC(ARG.samcorObs); corObsAIC
xyplot(value + lbnd + ubnd ~ age | fleet,data=cor.obs(ARG.samcorObs),type="l",col=c(1,"grey","grey"),lty=c(2,1,1),ylim=c(0,5),scales=list(y="free"))
#- Decision
ARG.ctrl@cor.obs[2,1:7]           <- 101
ARG.ctrl@cor.obs[3,1:9]           <- 201
ARG.ctrl                          <- update(ARG.ctrl)
ARG.samcorObs                     <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.sam)
corObsAIC                         <- AIC(ARG.samcorObs); corObsAIC

#---------------------
#- Configure catchabilities
#---------------------
xyplot(value + lbnd + ubnd ~ age | fleet,data=catchabilities(ARG.sam),type="b",pch=19,col=c(1,"grey","grey"),lty=c(2,1,1),scales=list(y="free"))
ARG.ctrl@catchabilities["FaroeseDeepwaterSurvey",ac(5:14)] <- c(1,2,3,4,5,rep(6,5))+101
ARG.ctrl                                        <- update(ARG.ctrl)
ARG.samcatch                                    <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samcorObs)
catchAIC                                        <- AIC(ARG.samcatch); catchAIC
xyplot(value + lbnd + ubnd ~ age | fleet,data=catchabilities(ARG.samcatch),type="b",pch=19,col=c(1,"grey","grey"),lty=c(2,1,1),scales=list(y="free"))
#- Decision
catchAIC                                        <- AIC(ARG.samcatch); catchAIC

#---------------------
#- Configure Observation variances
#---------------------
ARG.ctrl@obs.vars["catch unique",]                      <- c(5:(pg-1),(pg-1))
ARG.ctrl@obs.vars["FaroeseSummerSurvey",ac(5:12)]       <- c(105:111,111)
ARG.ctrl@obs.vars["FaroeseDeepwaterSurvey",ac(5:14)]    <- c(205:213,213)
ARG.ctrl                                      <- update(ARG.ctrl)
ARG.samobsVar                                 <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samcatch)
obsVarAIC                                     <- AIC(ARG.samobsVar); obsVarAIC
xyplot(value + lbnd + ubnd ~ age | fleet,data=obs.var(ARG.samobsVar),type="b",pch=19,col=1)
#- Decision
ARG.ctrl@obs.vars["catch unique",]                      <- c(1,2,rep(3,8),rep(4,5)) + 101
ARG.ctrl@obs.vars["FaroeseSummerSurvey",ac(5:12)]       <- c(1,1,2,2,3,4,5,5) + 201
ARG.ctrl@obs.vars["FaroeseDeepwaterSurvey",ac(5:14)]    <- c(1,2,2,3,3,3,3,3,3,3) + 301
ARG.ctrl                                      <- update(ARG.ctrl)
ARG.samobsVar                                 <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.sam)
obsVarAIC                                     <- AIC(ARG.samobsVar); obsVarAIC
xyplot(value + lbnd + ubnd ~ age | fleet,data=obs.var(ARG.samobsVar),type="b",col=c(1,"grey","grey"),lty=c(2,1,1),pch=19,scale=list(y="free"))

#---------------------
#- Configure F random walks
#---------------------

ARG.ctrl@f.vars["catch unique",]  <- c(5:(pg-1),(pg-1))
ARG.ctrl                          <- update(ARG.ctrl)
ARG.samfvar                       <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samobsVar)
fvarAIC                           <- AIC(ARG.samfvar); fvarAIC
xyplot(value + lbnd + ubnd ~ age | fleet,data=f.var(ARG.samfvar),type="b",col=c(1,"grey","grey"),lty=c(2,1,1),pch=19)
#- Decision
ARG.ctrl@f.vars["catch unique",]  <- c(1,2,3,rep(4,8),rep(4,4))
ARG.ctrl                          <- update(ARG.ctrl)
ARG.samfvar                       <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samfvar)
fvarAIC                           <- AIC(ARG.samfvar); fvarAIC



#---------------------
#- Configure F correlation structure (usually sensible to also run retrospectives)
#---------------------
ARG.ctrl@cor.F                    <- 2
ARG.samcorF2                      <- ARG.samfvar
ARG.retrocorF2                    <- retro(ARG,ARG.tun,ARG.ctrl,retro=3)
ARG.ctrl@cor.F                    <- 1
ARG.samcorF1                      <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samfvar)
ARG.retrocorF1                    <- retro(ARG,ARG.tun,ARG.ctrl,retro=3)
ARG.ctrl@cor.F                    <- 0
ARG.samcorF0                      <- FLSAM(ARG,ARG.tun,ARG.ctrl,starting.values=ARG.samfvar)
ARG.retrocorF0                    <- retro(ARG,ARG.tun,ARG.ctrl,retro=3)
corFAICs                          <- AIC(FLSAMs(corF0=ARG.samcorF0,corF1=ARG.samcorF1,corF2=ARG.samcorF2)); corFAICs
storeMohnsRho                     <- matrix(NA,nrow=3,ncol=3,dimnames=list(type=c("ssb","fbar","rec"),model=c("corF2","corF1","corF0")))
storeMohnsRho["ssb","corF2"]      <- mean(mohns.rho(ARG.retrocorF2,type="ssb",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["fbar","corF2"]     <- mean(mohns.rho(ARG.retrocorF2,type="fbar",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["rec","corF2"]      <- mean(mohns.rho(ARG.retrocorF2,type="rec",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["ssb","corF1"]      <- mean(mohns.rho(ARG.retrocorF1,type="ssb",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["fbar","corF1"]     <- mean(mohns.rho(ARG.retrocorF1,type="fbar",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["rec","corF1"]      <- mean(mohns.rho(ARG.retrocorF1,type="rec",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["ssb","corF0"]      <- mean(mohns.rho(ARG.retrocorF0,type="ssb",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["fbar","corF0"]     <- mean(mohns.rho(ARG.retrocorF0,type="fbar",ref.year=2018,span=3)[1:3,1])
storeMohnsRho["rec","corF0"]      <- mean(mohns.rho(ARG.retrocorF0,type="rec",ref.year=2018,span=3)[1:3,1])
storeMohnsRho
#- Decision
ARG.ctrl@cor.F                    <- 0




#-------------------------------------------------------------------------------
#- Run final model
#-------------------------------------------------------------------------------
ARG.ctrl@residuals                <- TRUE
ARG.sam                           <- FLSAM(ARG,ARG.tun,ARG.ctrl)
ARG.ctrl@residuals                <- FALSE
ARG.retro                         <- retro(ARG,ARG.tun,ARG.ctrl,retro=3)
ARG.looi                          <- looi(ARG,ARG.tun,ARG.ctrl,type="full")
save(ARG,ARG.tun,ARG.ctrl,ARG.sam,ARG.retro,ARG.looi,file=file.path(path,"ARG4samBenchmark.RData"))

#- Alternative to fit directly into stockassessment
data                              <- FLSAM2SAM(FLStocks(residual=ARG),ARG.tun)
conf                              <- ctrl2conf(ARG.ctrl,data)
par                               <- stockassessment::defpar(data,conf)
fit                               <- sam.fit(data,conf,par)

#-------------------------------------------------------------------------------
# Do the plotting
#-------------------------------------------------------------------------------

pdf(file.path(path,"plots_diagnosticsFT1.pdf",sep=""))
  print(plot(ARG.sam,futureYrs=F))
  residual.diagnostics(ARG.sam)

  resids <- residuals(ARG.sam)
  resids$std.res[which(is.na(resids$std.res))] <- 0
  print(xyplot(age ~ year | fleet,data=resids,main="Residuals by fleet",group=resids$fleet,cex=resids$std.res,
         panel=function(...){
           lst <- list(...)
           panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
         }))

  print(xyplot(age ~ fleet | as.factor(year),data=resids,main="Residuals by year",group=resids$fleet,cex=resids$std.res,scales=list(x=list(rot=90)),
         panel=function(...){
           lst <- list(...)
           panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
         }))

  # figure - catchabilities at age from HERAS
  catch <- catchabilities(ARG.sam)
  print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
         scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
         type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
         subset=fleet %in% names(ARG.tun),
         main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

  # figure - variance by data source
  obv <- obs.var(ARG.sam)
  obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
  obv <- obv[order(obv$value),]
  bp <- barplot(obv$value,ylab="Observation Variance",
                main="Observation variances by data source",col=factor(obv$fleet))
  axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
  legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

  # figure - variance vs uncertainty for each data source
  plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
       pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
  text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

  # figure - fishing age selectivity per year
  sel.pat <- merge(f(ARG.sam),fbar(ARG.sam),
                   by="year",suffixes=c(".f",".fbar"))
  sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
  sel.pat$age <- as.numeric(as.character(sel.pat$age))
  print(xyplot(sel ~ age|sprintf("%i's",floor((year)/5)*5),sel.pat,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

  # figure - correlation matrix of model parameters
  print(cor.plot(ARG.sam))

  #Plot uncertainties as a function of time
  CV.yrs <- ssb(ARG.sam)$year
  CV.dat <- cbind(SSB=ssb(ARG.sam)$CV,
                     Fbar=fbar(ARG.sam)$CV,Rec=rec(ARG.sam)$CV)
  print(matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
      xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters"))
  legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

  print(plot(ARG.retro))

  retroParams(ARG.retro)

  yrs <- 2010:2018
  res <- lapply(ARG.retro, f)
  res <- lapply(res, function(y) {
      y[which(y$year %in% (max(y$year) - 20):(max(y$year))), ]
  })
  res <- lapply(res, function(y) {
      cbind(y, retro = max(y$year))
  })
  res <- do.call(rbind, res)
  res <- subset(res, year %in% yrs)
  print(xyplot(value ~ an(age) | as.factor(year), data = res,
      type = "l", groups = retro, auto.key = list(space = "right",
          points = FALSE, lines = TRUE, type = "l"), main = paste("Retrospective pattern in F at age"),
      ylab = "F", xlab = "Ages", panel = panel.superpose, panel.groups = function(...) {
          panel.grid(v = -1, h = -1, lty = 3)
          panel.xyplot(...)
      }, scales = list(alternating = 1, y = list(relation = "free",
          rot = 0))))

  print(plot(ARG.looi,main="Leave one in"))

dev.off()


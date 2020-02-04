#################################
## Maturity by length - Elvar based on code from MRI Iceland
##########################


rm(list=ls())					# clear all data from workspace
library(tidyverse)
Species="vassild"

load("G:/Vassild/Data/EggaS/ind.ES.rData")
load("G:/Vassild/Data/EggaS/ca.ES.rData")#load biological data
#filter out outliers in ind file
ind.remove<-filter(ind.ES, age==2 & length>25 )%>%rbind(filter(ind.ES,age==1 & length>20))
ind.ES<-anti_join(ind.ES,ind.remove)

st_mat=2 #lowest maturity stage that is going to spwan this season (everyting less is immature)
sst_mat=2#lowest maturity speciealstage that is going to spwan this season

#put to gather station and biological data
tmp<-left_join(ind.ES,ca.ES)
tmp<-filter(tmp,noname==Species)#select species by Norwegian name
tmp<-dplyr::select(tmp,year,id,sex,lengthmeasurement,length,age,stage,specialstage)
tmp$specialstage<-as.numeric(tmp$specialstage)#for some reason specialstage is character in orginal data

# Creating 'mat' and aggregating in 5cm bin if chosen. 
j<-tmp[!is.na(tmp$sex),]
#j<-j[!is.na(j$stage),]
j$mat<-ifelse(j$stage<st_mat,0,1)   #decites which stages are immature
j$smat<-ifelse(j$specialstage<sst_mat,0,1)   #decites which specialstages are immature
j$le<-round(j$length)         # 1 cm bin
j$le<-round(j$length/5)*5     #round length into 5 cm bin

##Maturity by length###

## Creating data.frame for females. NB!! using specialstage
k<-j[j$sex==1,]
k<-k[!is.na(k$specialstage),]
k$mat<-ifelse(k$specialstage<sst_mat,0,1)
mat.fem<-tapply(k$mat,list(k$le,k$mat), length)
mat.fem<-cbind(as.numeric(dimnames(mat.fem)[[1]]),mat.fem)
mat.fem[is.na(mat.fem)]<-0
mat.fem<-data.frame(mat.fem)
dimnames(mat.fem)<-list(1:nrow(mat.fem),c("le","imat","mat"))
mat.fem$prop<-mat.fem$mat/(mat.fem$mat+mat.fem$imat)

# The data now looks something like this
#> mat.fem
#   le imat mat       prop
#1  30    4   0 0.00000000
#2  35   44   0 0.00000000
#3  40  113   0 0.00000000
#4  45  108   0 0.00000000
#5  50   96   8 0.07692308
#6  55   91  14 0.13333333
#7  60   60  37 0.38144330
#8  65   20  50 0.71428571
# .....


# #recycle some code for males. NB!! using specialstage

k<-j[j$sex==2,]
k<-k[!is.na(k$specialstage),]
k$mat<-ifelse(k$specialstage<sst_mat,0,1)
mat.mal<-tapply(k$mat,list(k$le,k$mat), length)
mat.mal<-cbind(as.numeric(dimnames(mat.mal)[[1]]),mat.mal)
mat.mal[is.na(mat.mal)]<-0
mat.mal<-data.frame(mat.mal)
dimnames(mat.mal)<-list(1:nrow(mat.mal),c("le","imat","mat"))
mat.mal$prop<-mat.mal$mat/(mat.mal$mat+mat.mal$imat)

#Performing the logistic regression
pr<-seq(0,60,0.01)
fem.glm<-glm(cbind(mat,imat)~le, data=mat.fem, family=binomial(link=probit))
mal.glm<-glm(cbind(mat,imat)~le, data=mat.mal, family=binomial(link=probit))

#Finding length where 50% are mature (p = 0.5 by defult in dose.p)
library(MASS)
femL50<-dose.p(fem.glm)
malL50<-dose.p(mal.glm)
L50<-cbind(femL50,malL50)

##some summary statistics
# summary(fem.glm)
# summary(mal.glm)
# plot(fem.glm)
# plot(mal.glm)
# confint(fem.glm)
# confint(mal.glm)
# logLik(fem.glm)
# logLik(mal.glm)

#proportion per 0-100 1cm length group if wanted. Saved as .csv
prM<-seq(0,120,1)
PMfem<-predict(fem.glm, data.frame(le=prM),type="response")
#write.csv(PMfem,"C/:data/PMatFem.csv")
PMmal<-predict(mal.glm, data.frame(le=prM),type="response")
#write.table(PMfmal,"C:data/PMatMal.csv",sep=",")

######################
## Plot
##################
#windows (width = 6.693, height = 5)  #size of graph divice (window) in inces, with =170mm = 6.6929134
par(mfcol=c(1,1),mar=c(2.2,2.2,0.1,0.3), omi=c(0.1,0.1,0.1,0.1), family="serif")
plot(c(0,60),c(0,1), type="n", yaxt="n",xaxt="n",ylab="",xlab="")
axis(1, labels=seq(0,99,5),at=seq(0,99,5),  cex=1, tck=0.02, mgp=c(0,0.2,0))
axis(1, labels=F,at=seq(1,99,1),  cex=1, tck=0.01, mgp=c(0,0.2,0))
axis(2, labels=T,at=seq(0,1,.1),  cex=1.1, tck=0.02, adj=1, mgp=c(0,0.2,0))
points(mat.fem$le, mat.fem$prop, pch=22, col="red", cex=1.1)#,lwd=2)
points(mat.mal$le, mat.mal$prop, pch=4, col="blue", cex=1.1,)#lwd=2)
lines(pr, predict(fem.glm, data.frame(le=pr),type="response"), lwd=3, col="red")
lines(pr, predict(mal.glm, data.frame(le=pr),type="response"), lwd=3, col="blue")
L50lines<-cbind(seq(1,100,1), matrix(.5,nrow=100,ncol=1))
lines(L50lines, lty=2)
L50fem<-cbind(matrix(femL50,nrow=11,ncol=1), seq(0,1,.1) )
lines(L50fem, lty=2)
L50mal<-cbind(matrix(malL50,nrow=11,ncol=1), seq(0,1,.1) )
lines(L50mal, lty=2)
mtext(side=1, "Length (cm)", cex=1.1, line=1.5)
mtext(side=2, "Proportion mature", cex=1.1, line=1.7)
legend(5,0.9,c("Female","Male"),pch=16,col=c("red","blue"), bty = "n",y.intersp=1.5)#col=c(27,254))

###Maturity by age###

## Creating data.frame for females. NB!! using specialstage
k<-j[j$sex==1,]
k<-k[!is.na(k$specialstage),]
k<-k[!is.na(k$age),]
k$mat<-ifelse(k$specialstage<sst_mat,0,1)
mat.fem<-tapply(k$mat,list(k$age,k$mat), length)
mat.fem<-cbind(as.numeric(dimnames(mat.fem)[[1]]),mat.fem)
mat.fem[is.na(mat.fem)]<-0
mat.fem<-data.frame(mat.fem)
dimnames(mat.fem)<-list(1:nrow(mat.fem),c("age","imat","mat"))
mat.fem$prop<-mat.fem$mat/(mat.fem$mat+mat.fem$imat)


# #recycle some code for males. NB!! using stage

k<-j[j$sex==2,]
k<-k[!is.na(k$specialstage),]
k<-k[!is.na(k$age),]
k$mat<-ifelse(k$specialstage<sst_mat,0,1)
mat.mal<-tapply(k$mat,list(k$age,k$mat), length)
mat.mal<-cbind(as.numeric(dimnames(mat.mal)[[1]]),mat.mal)
mat.mal[is.na(mat.mal)]<-0
mat.mal<-data.frame(mat.mal)
dimnames(mat.mal)<-list(1:nrow(mat.mal),c("age","imat","mat"))
mat.mal$prop<-mat.mal$mat/(mat.mal$mat+mat.mal$imat)

#Performing the logistic regression
pr<-seq(0,50,0.01)
fem.glm<-glm(cbind(mat,imat)~age, data=mat.fem, family=binomial(link=probit))
mal.glm<-glm(cbind(mat,imat)~age, data=mat.mal, family=binomial(link=probit))

#Finding length where 50% are mature (p = 0.5 by defult in dose.p)
library(MASS)
femA50<-dose.p(fem.glm)
malA50<-dose.p(mal.glm)
A50<-cbind(femA50,malA50)

bcA <- car::Boot(fem.glm,R=1000)
cbind(Ests=coef(fem.glm),confint(bcA))


# summary(fem.glm)
# summary(mal.glm)
# plot(fem.glm)
# plot(mal.glm)
# confint(fem.glm)
# confint(mal.glm)
# logLik(fem.glm)
# logLik(mal.glm)
# plot(0:30,-15:15)

#proportion per 0-100 1cm length group if wanted. Saved as .csv
prM<-seq(0,40,1)
PMfem<-predict(fem.glm, data.frame(age=prM),type="response")
#write.csv(PMfem,"G:/ICES/AFWG 2014/WD/PMfem.csv")
#write.csv(PMfem,"C/:data/PMatFem.csv")
PMmal<-predict(mal.glm, data.frame(age=prM),type="response")

#write.table(PMfmal,"C:data/PMatMal.csv",sep=",")

######################
## Plot
##################
#windows (width = 6.693, height = 5)  #size of graph divice (window) in inces, with =170mm = 6.6929134
par(mfcol=c(1,1),mar=c(2.2,2.2,0.1,0.3), omi=c(0.1,0.1,0.1,0.1), family="serif")
plot(c(0,50),c(0,1), type="n", yaxt="n",xaxt="n",ylab="",xlab="")
axis(1, labels=seq(0,99,5),at=seq(0,99,5),  cex=1, tck=0.02, mgp=c(0,0.2,0))
axis(1, labels=F,at=seq(1,99,1),  cex=1, tck=0.01, mgp=c(0,0.2,0))
axis(2, labels=T,at=seq(0,1,.1),  cex=1.1, tck=0.02, adj=1, mgp=c(0,0.2,0))

points(mat.fem$age, mat.fem$prop, pch=22, col="red", cex=1.1)#,lwd=2)
points(mat.mal$age, mat.mal$prop, pch=4, col="blue", cex=1.1,)#lwd=2)

lines(pr, predict(fem.glm, data.frame(age=pr),type="response"), lwd=3, col="red")
lines(pr, predict(mal.glm, data.frame(age=pr),type="response"), lwd=3, col="blue")

A50lines<-cbind(seq(1,100,1), matrix(.5,nrow=100,ncol=1))
lines(A50lines, lty=2)

A50fem<-cbind(matrix(femA50,nrow=11,ncol=1), seq(0,1,.1) )
lines(A50fem, lty=2)
A50mal<-cbind(matrix(malA50,nrow=11,ncol=1), seq(0,1,.1) )
lines(A50mal, lty=2)

mtext(side=1, "Age", cex=1.1, line=1.5)
mtext(side=2, "Proportion mature", cex=1.1, line=1.7)
legend(0,1,c("Female","Male"),pch=16,col=c("red","blue"), bty = "n",y.intersp=1.5)#col=c(27,254))


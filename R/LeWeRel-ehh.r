#Calculates a and b in the length-weight relationship W=a*L^b
#Elvar H. Hallfredsson

rm(list=ls())					# clear all data from workspace
library(tidyverse)
#load biological data
load("G:/Vassild/Data/EggaS/ind.ES.rData")#load biological data

#Select by species and year
WLdata<-filter(ind.ES,noname=="vassild")%>%filter(year==2018)

#Fit W=aL^b with lm (linear by log), find a and b and plot. All by sex
#all
AllWL<-dplyr::select(WLdata,weight,length)
fit<-lm(log(AllWL$weight)~log(AllWL$length))
loga<-summary(fit)$coef[[1]]
aA<-exp(loga)
bA<-summary(fit)$coef[[2]]
aA
bA
len_All<-seq(min(AllWL$length),max(AllWL$length),1)
plot(AllWL$length,(AllWL$weight),xlab="Length (cm)",ylab="Weight (kg)",col="grey",pch=3,cex=.5,main=paste(aA,"* L","^",bA))
points(len_All,(aA*len_All^bA),type="l",lwd=2)

#Female
FemWL<-filter(WLdata,sex==1)%>%dplyr::select(weight,length)
fit<-lm(log(FemWL$weight)~log(FemWL$length))
loga<-summary(fit)$coef[[1]]
aF<-exp(loga)
bF<-summary(fit)$coef[[2]]
aF
bF
len_Fem<-seq(min(FemWL$length),max(FemWL$length),1)
plot(FemWL$length,(FemWL$weight),xlab="Length (cm)",ylab="Weight (kg)",col="grey",pch=3,cex=.5,main=paste(aF,"* L","^",bF))
points(len_Fem,(aF*len_Fem^bF),type="l",lwd=2)

#Male
MalWL<-filter(WLdata,sex==2)%>%dplyr::select(weight,length)
fit<-lm(log(MalWL$weight)~log(MalWL$length))
loga<-summary(fit)$coef[[1]]
aM<-exp(loga)
bM<-summary(fit)$coef[[2]]
aM
bM
len_Mal<-seq(min(MalWL$length),max(MalWL$length),1)
plot(MalWL$length,(MalWL$weight),xlab="Length (cm)",ylab="Weight (kg)",col="grey",pch=3,cex=.5,main=paste(aM,"* L","^",bM))
points(len_Mal,(aM*len_Mal^bM),type="l",lwd=2)

#tabli for a and b 
WL_coeff<-data.frame(cbind(rbind(aA,aF,aM),rbind(bA,bF,bM)))
names(WL_coeff)<-c("a","b")
row.names(WL_coeff)<-c("All","Females","Males")

#Define trancparante colors for plot
Fem_col <- rgb(255, 0, 0, max = 255, alpha = 40, names = "red40")
Mal_col <- rgb(0, 0, 255, max = 255, alpha = 40, names = "blue40")


#One plot with all and by sex
plot(WLdata$length,(WLdata$weight),type="n",xlab="Length (cm)",ylab="Weight (g)")
points(FemWL$length,(FemWL$weight),xlab="Length (cm)",ylab="Weight (kg)",col=Fem_col,pch=3,cex=.5)
points(MalWL$length,(MalWL$weight),xlab="Length (cm)",ylab="Weight (kg)",col=Mal_col,pch=3,cex=.5)
points(len_All,(aA*len_All^bA),type="l",lwd=2)
points(len_Fem,(aF*len_Fem^bF),type="l",lwd=2,col="red")
points(len_Mal,(aM*len_Mal^bM),type="l",lwd=2,col="blue")


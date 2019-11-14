#Age-length calculations -  von Bertalanffy's growth curve, alternative growth curves, age-length key, 
#plus mortality and suveyval rates. 
#Elvar H. Hallfredsson (2009-)2019

rm(list=ls())					# clear all data from workspace
library(tidyverse)
library(fishmethods)#needet for "alternative growth moedels" and "age-length key" part of the code

load("G:/Vassild/Data/EggaS/ind.ES.rData")#load biological data

#Remove outliers
ind.remove<-filter(ind.ES, age==2 & length>25 )%>%rbind(filter(ind.ES,age==1 & length>20))
ind.ES<-anti_join(ind.ES,ind.remove)
#Select and clean 
ALdata<-cbind(ind.ES$age,ind.ES$length,ind.ES$sex,ind.ES$year,ind.ES$id)
ALdata<-na.omit(ALdata)
#ALdata<-subset(ALdata,ALdata[,1]<21)#filter to cut at some max age

#Sequence for predictions in VBL plots
pr=seq(0,max(ALdata[,1])+1)

##Average length at age with sd and numbers per length group
#all
LengthAv<-aggregate(ALdata[,2],list(ALdata[,1]),mean)
LengthSd<-aggregate(ALdata[,2],list(ALdata[,1]),sd)
LengthN<-aggregate(ALdata[,2],list(ALdata[,1]),length)
AgeLenAll<-cbind(LengthAv,LengthSd$x,LengthN$x)
names(AgeLenAll)<-c("Age","Length","Sd","N")

#write.csv(Species,"\\Vassild\\Benchmark 2010\\Growth and Maturity\\AgeLenEgga09All.csv")

#female
ALdata2<-subset(ALdata,ALdata[,3]==1)
LengthAv<-aggregate(ALdata2[,2],list(ALdata2[,1]),mean)
LengthSd<-aggregate(ALdata2[,2],list(ALdata2[,1]),sd)
LengthN<-aggregate(ALdata2[,2],list(ALdata2[,1]),length)
AgeLenFem<-cbind(LengthAv,LengthSd$x,LengthN$x)
names(AgeLenFem)<-c("Age","Length","Sd","N")

#write.csv(AgeLenFem,"\\Vassild\\Benchmark 2010\\Growth and Maturity\\AgeLenEgga09Fem.csv")

#male
ALdata2<-subset(ALdata,ALdata[,3]==2)
LengthAv<-aggregate(ALdata2[,2],list(ALdata2[,1]),mean)
LengthSd<-aggregate(ALdata2[,2],list(ALdata2[,1]),sd)
LengthN<-aggregate(ALdata2[,2],list(ALdata2[,1]),length)
AgeLenMal<-cbind(LengthAv,LengthSd$x,LengthN$x)
names(AgeLenMal)<-c("Age","Length","Sd","N")

#plot average length with n per age-group
LNAll<-ggplot(AgeLenAll, aes(Age,Length,label=N))+ggtitle("All")+geom_point()+geom_text(aes(label=N),size=2.5,col="darkgrey",nudge_y=0.5,nudge_x=-0.5,hjust=0, vjust=0)+theme_bw()+expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
LNFem<-ggplot(AgeLenFem, aes(Age,Length,label=N))+ggtitle("Females")+geom_point()+geom_text(aes(label=N),size=2.5,col="darkgrey",nudge_y=0.5,nudge_x=-0.5,hjust=0, vjust=0)+theme_bw()+expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
LNMal<-ggplot(AgeLenMal, aes(Age,Length,label=N))+ggtitle("Males")+geom_point()+geom_text(aes(label=N),size=2.5,col="darkgrey",nudge_y=0.5,nudge_x=-0.5,hjust=0, vjust=0)+theme_bw()+expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
gridExtra::grid.arrange(LNAll,LNFem,LNMal,bottom="Age",top="Mean length by age group")

XlimAdist<-c(0,ceiling(max(AgeLenAll$Age)/10)*10)
YlimAdist<-c(0,ceiling(max(AgeLenAll$N)/10)*10)

#Age distribution. NB! ONLY IN THE AGED DATA, NOT WEIGTHED BY CATCH 
AdistA<-ggplot(AgeLenAll, aes(Age,N))+geom_col()+ylim(YlimAdist)+xlim(XlimAdist)+ggtitle("All")+theme_bw()+geom_text(aes(label=N),size=2,col="darkgrey",nudge_y=3,nudge_x=-0.5,hjust=0, vjust=0)+theme(axis.title.x=element_blank())
AdistF<-ggplot(AgeLenFem, aes(Age,N))+geom_col(fill="red")+ylim(YlimAdist)+xlim(XlimAdist)+ggtitle("Females")+theme_bw()+geom_text(aes(label=N),size=2,col="darkgrey",nudge_y=3,nudge_x=-0.5,hjust=0, vjust=0)+theme(axis.title.x=element_blank())
AdistM<-ggplot(AgeLenMal, aes(Age,N))+geom_col(fill="blue")+ylim(YlimAdist)+xlim(XlimAdist)+ggtitle("Males")+theme_bw()+geom_text(aes(label=N),size=2,col="darkgrey",nudge_y=3,nudge_x=-0.5,hjust=0, vjust=0)+theme(axis.title.x=element_blank())
gridExtra::grid.arrange(AdistA,AdistF,AdistM,bottom="Age",top="Age distributions in samples (NB:not weightet by catch per station)")

## von Bertalanffy's growth curve (VBL)

#Estimate VBL with nonlinear (weighted) least-squares (nls) estimates of the parameters of a nonlinear model.
j<-data.frame(ALdata)
names(j)<-c("Age","Length","Sex","Year","id")
attach(j)
j<-j[!is.na(j$Age),]
vb.fem<-nls(log(Length)~log(Linf*(1-exp(-K*(Age-t0)))), data=j[j$Sex==1,], start=list(Linf=50, K=0.2, t0=0))
vb.mal<-nls(log(Length)~log(Linf*(1-exp(-K*(Age-t0)))), data=j[j$Sex==2,], start=list(Linf=50, K=0.2, t0=0))
vb.uni<-nls(log(Length)~log(Linf*(1-exp(-K*(Age-t0)))), data=j, start=list(Linf=50, K=0.2, t0=0))

#find VBL coefficients
vbl.fem<-coefficients(vb.fem)
vbl.mal<-coefficients(vb.mal)
vbl.uni<-coefficients(vb.uni)
#coefficients by sex, and all
vbl.coeff<-rbind(vbl.fem,vbl.mal,vbl.uni)

#initial test plot 
ALcheck<-data.frame(cbind(pr,vbl.coeff[1,1]*(1-exp(-vbl.coeff[1,2]*(pr-vbl.coeff[1,3])))));colnames(ALcheck)<-c("Age","Length")
plot(j$Age,j$Length,pch=3,cex=.5,col="grey",ylim = c(0,ceiling(max(ALcheck$Length)/10)*10))
points(ALcheck$Age,ALcheck$Length,type = "l")

#predicted VBL lenth valus by age per sex, and unified for both sexes (all)
pr.fem<-data.frame(cbind(pr,exp(predict(vb.fem, data.frame(Age=pr)))));names(pr.fem)<-c("Age","Length")
pr.mal<-data.frame(cbind(pr,exp(predict(vb.mal, data.frame(Age=pr)))));names(pr.mal)<-c("Age","Length")
pr.uni<-data.frame(cbind(pr,exp(predict(vb.uni, data.frame(Age=pr)))));names(pr.uni)<-c("Age","Length")

#plot separate by sex, and all
ggFem<-ggplot(data=filter(j,Sex==1),aes(Age,Length))+geom_boxplot(aes(group=cut_width(Age,0.25))) + geom_line(data=pr.fem,aes(Age,Length))+theme_bw()+theme(axis.title.x=element_blank())+labs(title = "Female",y="Length (cm)")+ expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
ggMal<-ggplot(data=filter(j,Sex==2),aes(Age,Length))+geom_boxplot(aes(group=cut_width(Age,0.25))) + geom_line(data=pr.mal,aes(Age,Length))+theme_bw()+theme(axis.title.x=element_blank())+labs(title = "Male",y="Length (cm)")+ expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
ggUni<-ggplot(j,aes(Age,Length))+geom_boxplot(aes(group=cut_width(Age,0.25))) + geom_line(data=pr.uni,aes(Age,Length))+theme_bw()+labs(title = "All",y="Length (cm)",x="Age")+ expand_limits(x = 0, y = 0)+theme(axis.title.x=element_blank())
gridExtra::grid.arrange(ggFem,ggMal,ggUni,bottom="Age")

#plot females and males in same plot
ggplot(j,aes(Age,Length)) + geom_boxplot(aes(fill=as.factor(Sex),group=interaction(as.factor(Sex),round(Age))))+
  geom_line(data=pr.fem,aes(Age,Length),col="red",size=1)+ geom_line(data=pr.mal,aes(Age,Length),col="cyan4",size=1)+
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male"))+ expand_limits(x = 0, y = 0)+
  labs(title="VBL growt function",ylab="Length (cm)")+theme_bw()

#table with the VBL coefficients
vbl.coeff


# ##VBL without t0

# vb.fem<-nls(log(Length)~log(Linf*(1-exp(-K*(Age)))), data=j[j$Sex==1,], start=list(Linf=50, K=0.2))
# vb.mal<-nls(log(Length)~log(Linf*(1-exp(-K*(Age)))), data=j[j$Sex==2,], start=list(Linf=50, K=0.2))
# vb.uni<-nls(log(Length)~log(Linf*(1-exp(-K*(Age)))), data=j, start=list(Linf=50, K=0.2))
# 
# pr.fem<-data.frame(cbind(pr,exp(predict(vb.fem, data.frame(Age=pr)))));names(pr.fem)<-c("Age","Length")
# pr.mal<-data.frame(cbind(pr,exp(predict(vb.mal, data.frame(Age=pr)))));names(pr.mal)<-c("Age","Length")
# pr.uni<-data.frame(cbind(pr,exp(predict(vb.uni, data.frame(Age=pr)))));names(pr.uni)<-c("Age","Length")
# 
# ggplot(j,aes(Age,Length)) + geom_boxplot(aes(fill=as.factor(Sex),group=interaction(as.factor(Sex),round(Age))))+
#   geom_line(data=pr.fem,aes(Age,Length),col="red",size=1)+ geom_line(data=pr.mal,aes(Age,Length),col="cyan4",size=1)+
#   scale_fill_discrete(name = "Sex", labels = c("Female", "Male"))+ expand_limits(x = 0, y = 0)+
#   labs(title="VBL growt function",ylab="Length (cm)")+theme_bw()
# 
# ggplot(j,aes(Age,Length))+geom_boxplot(aes(group=cut_width(Age,0.25))) + geom_line(data=pr.uni,aes(Age,Length))+theme_bw()+labs(title = "All",y="Length (cm)",x="Age")+ expand_limits(x = 0, y = 0)

#Alternative growth models
growth(intype=1,unit=1,size=j$Length,age=j$Age,calctype=1,wgtby=1,s2=NULL,error=1, 
       specwgt=0.0001,Sinf=50,K=0.2,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

##Age-length key
ALkey<-alk(age=j$Age,size=j$Length,binsize=1,type=1)
barplot(ALkey[,2],names.arg=ALkey[,1],xlab="Length (cm)",ylab = "Numbers")
#write.csv(ALkey,"Put in path/ALkey.csv")
 
# Survival and mortality (Z) rates,full=fylly recruitet to fisheries, last = chosen max age
Z<-agesurv(age=j$Age,full=7,last = 30)
filter(Z$results,Parameter=="Z")
#plot those includet in the agesurv function
barplot(Z$data$number,names.arg = Z$data$age, ylab = "Numbers",xlab = "Age") 








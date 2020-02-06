## Plot catch at age
##
## Before: cn.dat
## After:  cn.pdf


library(stockassessment)
library(icesTAF)

## 1  Read catch-at-age data

cn <- read.ices("data/cn.dat")
a <- as.integer(colnames(cn))

c95 <- cn["1995",]
c96 <- cn["1996",]
c97 <- cn["1997",]
c98 <- cn["1998",]
c99 <- cn["1999",]

## 2  Plot data

pdf("cn.pdf", width=14, height=8)
par(mfrow=c(2,3))
barplot(c95, main=1995)
barplot(c96, main=1996)
barplot(c97, main=1997)
barplot(c98, main=1998)
barplot(c99, main=1999)
dev.off()

## 3  Plot all years

pdf("catage-gss-faroes.pdf", width=14, height=8)
barchart(t(cn[,as.character(5:18)]), groups=rownames(cn), horizontal=FALSE, as.table=TRUE)
dev.off()

ic <- read.ices("data/cn_intercatch.dat")
pdf("catage-gss-iceland.pdf", width=14, height=8)
barchart(t(ic[,as.character(4:18)]), groups=rownames(ic), horizontal=FALSE, as.table=TRUE)
dev.off()

cod <- read.csv("COD_catch_at_age.csv", row.names=1, check.names=FALSE)[-(1:2)]
pdf("catage-cod.pdf", width=14, height=8)
barchart(t(cod), groups=rownames(cod), horizontal=FALSE, as.table=TRUE)
dev.off()

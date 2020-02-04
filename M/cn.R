## Plot catch at age
##
## Before: cn.dat
## After:  cn.pdf


library(stockassessment)

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

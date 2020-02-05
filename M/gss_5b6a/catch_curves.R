## Catch-curve analysis to estimate Z
##
## For early near-virgin stock conditions,
## M is close to Z
##
## Before: cn.dat
## After:  catch_curves.pdf


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

pdf("catch_curves.pdf", width=14, height=8)

par(mfrow=c(2,3))

plot(a, log(c95), main=1995, ylim=c(4,9)); abline(v=c(14,19), lty=2)
plot(a, log(c96), main=1996, ylim=c(4,9)); abline(v=c(14,19), lty=2)
plot(a, log(c97), main=1997, ylim=c(4,9)); abline(v=c(14,19), lty=2)
plot(a, log(c98), main=1998, ylim=c(4,9)); abline(v=c(14,19), lty=2)
plot(a, log(c99), main=1999, ylim=c(4,9)); abline(v=c(14,19), lty=2)

## 3  Fit models

fm95 <- lm(log(c95)~a, subset=a %in% 14:19)
fm96 <- lm(log(c96)~a, subset=a %in% 14:19)
fm97 <- lm(log(c97)~a, subset=a %in% 14:19)
fm98 <- lm(log(c98)~a, subset=a %in% 14:19)
fm99 <- lm(log(c99)~a, subset=a %in% 14:19)

z95 <- -coef(fm95)[[2]]
z96 <- -coef(fm96)[[2]]
z97 <- -coef(fm97)[[2]]
z98 <- -coef(fm98)[[2]]
z99 <- -coef(fm99)[[2]]

z <- c(z95, z96, z97, z98, z99)
z

## Plot model fit to data

par(mfrow=c(2,3))

plot(a, log(c95), main=1995, ylim=c(4,9)); abline(v=c(14,19), lty=2); abline(fm95); points(log(c95)~a, subset=a %in% 14:19, pch=16, cex=2.5)
plot(a, log(c96), main=1996, ylim=c(4,9)); abline(v=c(14,19), lty=2); abline(fm96); points(log(c96)~a, subset=a %in% 14:19, pch=16, cex=2.5)
plot(a, log(c97), main=1997, ylim=c(4,9)); abline(v=c(14,19), lty=2); abline(fm97); points(log(c97)~a, subset=a %in% 14:19, pch=16, cex=2.5)
plot(a, log(c98), main=1998, ylim=c(4,9)); abline(v=c(14,19), lty=2); abline(fm98); points(log(c98)~a, subset=a %in% 14:19, pch=16, cex=2.5)
plot(a, log(c99), main=1999, ylim=c(4,9)); abline(v=c(14,19), lty=2); abline(fm99); points(log(c99)~a, subset=a %in% 14:19, pch=16, cex=2.5)

frame()
labels <- paste0(1995:1999, "    Z = ", format(round(z,2)))
text(0.5, seq(0.9,0.1,-0.2), labels, cex=2)

dev.off()

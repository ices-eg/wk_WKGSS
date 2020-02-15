## Examine catch-at-age data to determine appropriate Fbar ages

library(stockassessment)  # read.ices

## 1  Import
gss.5a14 <- read.csv("catage_gss_iceland.csv", row.names=1, na.strings="-1",
                     check.names=FALSE)
user3 <- "https://stockassessment.org/datadisk/stockassessment/userdirs/user3"
cn.dat <- file.path(user3, "ARU.27.5b6a_WKGSS2020_FINAL/data/cn.dat")
gss.5b6a <- as.data.frame(read.ices(cn.dat))
gss.5b6a[gss.5b6a=="-1"] <- 0

## 2  Calculate cut-off points
cutoff <- function(cn, probs=c(0.1, 0.9), na.rm=TRUE)
{
  x <- cumsum(prop.table(colSums(cn, na.rm=na.rm)))
  y <- as.numeric(names(x))
  approx(x, y, xout=probs)$y
}
cut.5a14 <- cutoff(gss.5a14)
cut.5b6a <- cutoff(gss.5b6a)

## 3  Export
Fbar <- data.frame("5a14"=cut.5a14, "5b6a"=cut.5b6a, check.names=FALSE)
Fbar$Both <- rowMeans(Fbar)
row.names(Fbar) <- c("From", "To")
ages <- round(Fbar)
write.csv(t(ages), "Fbar_ages.csv", quote=FALSE)

## 4  Plot
agevec <- ages$Both[1]:ages$Both[2]
catage.5a14 <- prop.table(colSums(gss.5a14, na.rm=TRUE))
catage.5b6a <- prop.table(colSums(gss.5b6a, na.rm=TRUE))
pdf("Fbar_ages.pdf", width=12, height=10)
par(mfrow=c(2,1))
barplot(catage.5a14, col=8*(names(catage.5a14) %in% agevec), main="5a14")
barplot(catage.5b6a, col=8*(names(catage.5b6a) %in% agevec), main="5b6a")
dev.off()

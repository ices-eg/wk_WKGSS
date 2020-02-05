M <- c(0.1, 0.15, 0.20, 0.25, 0.30)

A <- 100

N <- matrix(NA_real_, A, length(M), dimnames=list(1:A,M))

for(i in 1:length(M))
  N[,i] <- 1000 * (1-M[i])^(0:99)
N[21,] <- colSums(N[21:A,])
N <- N[1:21,]

pdf("sim.pdf", width=14, height=8)

par(mfrow=c(2,3))
labels <- paste0("M = ", format(round(M,2)))
for(i in 1:length(M))
  barplot(N[10:21,i], main=labels[i])

dev.off()

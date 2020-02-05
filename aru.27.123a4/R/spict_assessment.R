#+ setup, include=FALSE
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, error = FALSE, cache = TRUE)

library(spict)
runretro <- FALSE

dat <-readxl::read_xlsx("data/GSS_indices270120_AK.xlsx")
## dat <- dat[dat$year >= 1988 & dat$year <= 2018,]
dat$catchTOT <- dat$catch1and2 + dat$catch3 + dat$catch4


# dat %>% filter(year >= 1988 & year <= 2018) %>% select(year, catch1and2, catch3, catch4, catchTOT) %>%
#   reshape2:::melt.data.frame(id.vars = c("year", "catchTOT")) %>%
#   mutate(perc = value / catchTOT * 100) %>% group_by(variable) %>%
#   summarise(mean(perc))
#
# dat$year


## North Sea index - does not converge ----
inp_NS <- list(timeC = dat$year,
               obsC = dat$catchTOT,
               timeI = dat$year + dat$northsea_month / 12,
               obsI = dat$northsea_SA,
               optimiser.control = list(iter.max = 1e5,
                                        eval.max = 1e5))
inp_NS <- check.inp(inp_NS)
plotspict.data(inp_NS)

fit_NS <- fit.spict(inp_NS)
fit_NS
plot(fit_NS)



## North Sea index split in two periods ----
w <- !is.na(dat$northsea_month) & dat$northsea_month == 10
v <- !is.na(dat$northsea_month) & dat$northsea_month < 5
inp_NS <- list(timeC = dat$year,
               obsC = dat$catchTOT,
               timeI = list(dat$year[w] + dat$northsea_month[w] / 12,
                            dat$year[v] + dat$northsea_month[v] / 12),
               obsI = list(dat$northsea_SA[w],
                           dat$northsea_SA[v]),
               optimiser.control = list(iter.max = 1e5,
                                        eval.max = 1e5),
               priors = list(
                 #logn = c(log(2), 0.5, 1)
                 ))
inp_NS <- check.inp(inp_NS)
dev.new()
plotspict.data(inp_NS)

fit_NS <- fit.spict(inp_NS)

## fit_NS
dev.new()
plot(fit_NS)

if(fit_NS$opt$convergence == 0) {
  fit_NS <- calc.osa.resid((fit_NS))
  plotspict.diagnostic(fit_NS)
  if(runretro) {
    fit_NS <- retro(fit_NS)
    plotspict.retro(fit_NS)
  }
}





## Norwegian Sea SA -  does not converge ----
inp_norSA <- list(timeC = dat$year,
               obsC = dat$catchTOT,
               timeI = dat$year,
               obsI = dat$norwegian_seaSA,
               optimiser.control = list(iter.max = 1e5,
                                        eval.max = 1e5),
               priors = list(
                 #logn = c(log(2), 0.5, 1)
               ))
inp_norSA <- check.inp(inp_norSA)
plotspict.data(inp_norSA)

fit_norSA <- fit.spict(inp_norSA)
fit_norSA <- calc.osa.resid((fit_norSA))
if (runretro) {
  fit_norSA <- retro(fit_norSA)
  plotspict.retro(fit_norSA)
}

fit_norSA
plot(fit_norSA)
plotspict.diagnostic(fit_norSA)



## Norwegian Sea AC - does not converge ----
inp_norAC <- list(timeC = dat$year,
                  obsC = dat$catchTOT,
                  timeI = dat$year,
                  obsI = dat$norwegian_seaAC,
                  optimiser.control = list(iter.max = 1e5,
                                           eval.max = 1e5),
                  priors = list(
                    #logn = c(log(2), 0.5, 1)
                  ))
inp_norAC <- check.inp(inp_norAC)
plotspict.data(inp_norAC)

fit_norAC <- fit.spict(inp_norAC)
fit_norAC <- calc.oAC.resid((fit_norAC))

fit_norAC
plot(fit_norAC)
plotspict.diagnostic(fit_norAC)

if(runretro) {
  fit_norAC <- retro(fit_norAC)
  plotspict.retro(fit_norAC)
}



## Norwegian Sea AC and North Sea -   ----
w <- !is.na(dat$northsea_month) & dat$northsea_month == 10
v <- !is.na(dat$northsea_month) & dat$northsea_month < 5
inp_NS_norSA <- list(timeC = dat$year,
                     obsC = dat$catchTOT,
                     timeI = list(dat$year + 3.5 / 12,
                                  dat$year[w] + dat$northsea_month[w] / 12
                                  #dat$year[v] + dat$northsea_month[v] / 12
                                  ),
                     obsI = list(dat$norwegian_seaAC,
                                 dat$northsea_SA[w]
                                 #dat$northsea_SA[v]
                                 ),
                     optimiser.control = list(iter.max = 1e5,
                                              eval.max = 1e5),
                     priors = list(
                       logn = c(log(2), 0.2, 1),
                       logbkfrac = c(log(1), 0.5, 1) ## B/K fraction in the beginning of the time series - around 1 means pristine stock
                  ))
inp_NS_norSA <- check.inp(inp_NS_norSA)
dev.new()
plotspict.data(inp_NS_norSA)

fit_NS_norSA <- fit.spict(inp_NS_norSA)
fit_NS_norSA <- calc.osa.resid((fit_NS_norSA))

fit_NS_norSA
dev.new()
plot(fit_NS_norSA)
plotspict.diagnostic(fit_NS_norSA)

if(runretro) {
  fit_NS_norSA <- retro(fit_NS_norSA)
  plotspict.retro(fit_NS_norSA)
}


## Norwegian Sea AC and only area 2 -   ----
w <- !is.na(dat$northsea_month) & dat$northsea_month == 10
inp_NS_norSA <- list(timeC = dat$year,
                     obsC = dat$catch4,
                     timeI = list(#dat$year,
                                  c(dat$year + dat$northsea_month / 12)
                                  ),
                     obsI = list(#dat$norwegian_seaAC,
                                 dat$northsea_SA
                                 ),
                     optimiser.control = list(iter.max = 1e5,
                                              eval.max = 1e5),
                     priors = list(
                       ##logn = c(log(2), 0.5, 1)
                     ))
inp_NS_norSA <- check.inp(inp_NS_norSA)
plotspict.data(inp_NS_norSA)

fit_NS_norSA <- fit.spict(inp_NS_norSA)
fit_NS_norSA <- calc.osa.resid((fit_NS_norSA))

fit_NS_norSA
plot(fit_NS_norSA)
plotspict.diagnostic(fit_NS_norSA)

## fit_NS_norSA <- retro(fit_NS_norSA)
## plotspict.retro(fit_NS_norSA)



## Norwegian Sea commercial CPUE - pelagics ----
inp_pelCPUE <- list(timeC = dat$year,
                  obsC = dat$catchTOT,
                  timeI = dat$year,
                  obsI = dat$CPUE_PT,
                  optimiser.control = list(iter.max = 1e5,
                                           eval.max = 1e5),
                  priors = list(
                    #logn = c(log(2), 0.5, 1)
                  ))
inp_pelCPUE <- check.inp(inp_pelCPUE)
plotspict.data(inp_pelCPUE)

fit_pelCPUE <- fit.spict(inp_pelCPUE)
fit_pelCPUE <- calc.oAC.resid((fit_pelCPUE))
## fit_pelCPUE <- retro(fit_pelCPUE)

fit_pelCPUE
plot(fit_pelCPUE)
plotspict.diagnostic(fit_pelCPUE)
plotspict.retro(fit_pelCPUE)


## Norwegian Sea commercial CPUE - bottom trawl ----
inp_btCPUE <- list(timeC = dat$year,
                    obsC = dat$catchTOT,
                    timeI = dat$year,
                    obsI = dat$CPUE_BT,
                    optimiser.control = list(iter.max = 1e5,
                                             eval.max = 1e5),
                    priors = list(
                      #logn = c(log(2), 0.5, 1)
                    ))
inp_btCPUE <- check.inp(inp_btCPUE)
plotspict.data(inp_btCPUE)

fit_btCPUE <- fit.spict(inp_btCPUE)
fit_btCPUE <- calc.oAC.resid((fit_btCPUE))
## fit_btCPUE <- retro(fit_btCPUE)

fit_btCPUE
plot(fit_btCPUE)
plotspict.diagnostic(fit_btCPUE)
plotspict.retro(fit_btCPUE)


## Norwegian Sea AC and North Sea and pelagic CPUe   ----
w <- !is.na(dat$northsea_month) & dat$northsea_month == 10
v <- !is.na(dat$northsea_month) & dat$northsea_month < 5
inp_NS_norSA_PT <- list(timeC = dat$year,
                     obsC = dat$catchTOT,
                     timeI = list(#dat$year,
                                  dat$year[w] + dat$northsea_month[w] / 12,
                                  ## dat$year[v] + dat$northsea_month[v] / 12,
                                  dat$year + 0.5),
                     obsI = list(#dat$norwegian_seaAC,
                                 dat$northsea_SA[w],
                                 ## dat$northsea_SA[v],
                                 dat$CPUE_PT),
                     optimiser.control = list(iter.max = 1e5,
                                              eval.max = 1e5),
                     priors = list(
                       ##logn = c(log(2), 1, 1)
                     ))
inp_NS_norSA_PT <- check.inp(inp_NS_norSA_PT)
plotspict.data(inp_NS_norSA_PT)

fit_NS_norSA_PT <- fit.spict(inp_NS_norSA_PT)
fit_NS_norSA_PT
fit_NS_norSA_PT <- calc.osa.resid((fit_NS_norSA_PT))


plot(fit_NS_norSA_PT)
plotspict.diagnostic(fit_NS_norSA_PT)

fit_NS_norSA_PT <- retro(fit_NS_norSA_PT)
plotspict.retro(fit_NS_norSA_PT)



## DATRAS
ns <- readRDS("~/Dropbox/Work/data/NS-IBTS1991_2018.Rds")
sort(unique(levels(ns[["HL"]]$Species)))
ns_ar <- subset(ns, Species == "Argentina silus")
ns_ar <- addSpectrum(ns_ar)
ns_ar <- addWeightByHaul(ns_ar)
bubblePlot(ns_ar)
plot(ns_ar)

library(surveyIndex)

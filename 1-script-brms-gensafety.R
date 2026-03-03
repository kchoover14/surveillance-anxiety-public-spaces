options(prompt="R>", "scipen"=100, "digits"=4)
library(ordinal)
library(brms)
library(readxl)
library(ggplot2)
library(scales) 
library(coda)
gensafety=read_excel("festivaldata-full.xlsx", sheet="gensafety")
gensafety$Experiencen = ordered(gensafety$Experiencen)
gensafety$Experience = ordered(gensafety$Experience)
gensafety$Sex = factor(gensafety$Sex)
gensafety$SexOrient = factor(gensafety$SexOrient)
gensafety$PerSafetyn = ordered(gensafety$PerSafetyn)
gensafety$Changesn = ordered(gensafety$Changesn)

#MODELS
#Personal Safety
safety.brm = brm(
  formula = PerSafetyn ~ Experience + Sex + SexOrient, 
  data = gensafety, 
  cores = getOption("mc.cores", 4),
  family = cumulative("logit")
)
pp_check(safety.brm, nsamples = 100)
safety.mcmc = as.mcmc(safety.brm)
plot(safety.mcmc)

#Changes to Personal Safety
changes.brm = brm(
  formula = Changesn ~ Experience + Sex + SexOrient, 
  data = gensafety, 
  cores = getOption("mc.cores", 4),
  family = cumulative("logit")
)
pp_check(changes.brm, nsamples = 100)
changes.mcmc = as.mcmc(changes.brm)
plot(changes.mcmc)

#RESULTS
sink(file = "1-results-brm gensafety-full.txt", append=FALSE)
cat("**************Personal Safety******************")
cat(sep="\n")
summary(safety.brm)
cat(sep="\n")
cat("**************Changes to Personal Safety******************")
cat(sep="\n")
summary(changes.brm)
sink()

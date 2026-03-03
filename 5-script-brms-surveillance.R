options(prompt="R>", scipen=100, digits=4)

library(ordinal)
library(brms)
library(broom)
library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
library(coda)
surveillance=read_excel("festivaldata-full.xlsx", sheet="surveillance")
surveillance$Sex = factor(surveillance$Sex)
surveillance$Experience = ordered(surveillance$Experience)
surveillance$Experiencen = ordered(surveillance$Experiencen)
surveillance$SexOrient = factor(surveillance$SexOrient)
surveillance$NoticeSurvn= ordered(surveillance$NoticeSurvn)	
surveillance$NoticeChangeSurvn= ordered(surveillance$NoticeChangeSurvn)	
surveillance$NeedMoreLessn = ordered(surveillance$NeedMoreLessn)
surveillance$DeclineUnsafen = ordered(surveillance$DeclineUnsafen)	
surveillance$IntroSurvInclineAttendn = ordered(surveillance$IntroSurvInclineAttendn)

#MODELS
nosur.brm = brm(
    formula=NoticeSurvn ~ Sex + Experience + SexOrient,
    data = surveillance,
    family = cumulative("logit"),
    cores = getOption("mc.cores", 4)
    )
pp_check(nosur.brm, nsamples = 100)
nosur.mcmc = as.mcmc(nosur.brm)
plot(nosur.mcmc)

incsur.glm = glm(NoticeChangeSurvn ~ Sex + Experience + SexOrient, 
                 family=binomial, data = surveillance)
#Check model fit
library(performance)
library(see)
binned_residuals(incsur.glm)

moreless.brm = brm(
    formula= NeedMoreLessn ~ Sex + Experience + SexOrient, 
    data = surveillance,
    family = cumulative ("logit"),
    cores = getOption("mc.cores", 4)
)
pp_check(moreless.brm, nsamples = 100)
moreless.mcmc = as.mcmc(moreless.brm)
plot(moreless.mcmc)

inclined.brm = brm(
    formula = IntroSurvInclineAttendn ~ Sex + Experience + SexOrient, 
    data = surveillance,
    family = cumulative("logit"),
    cores = getOption("mc.cores", 4)
    )
pp_check(inclined.brm, nsamples = 100)
inclined.mcmc = as.mcmc(inclined.brm)
plot(inclined.mcmc)

declined.brm = brm(
    formula = DeclineUnsafen ~ Sex + Experience + SexOrient, 
    data = surveillance,
    family = cumulative("logit"),
    cores = getOption("mc.cores", 4)
    )
pp_check(declined.brm, nsamples = 100)
declined.mcmc = as.mcmc(declined.brm)
plot(declined.mcmc)

#RESULTS
sink(file = "5-brm surveillance-full.txt", append=FALSE)
cat("**************Notice Surv******************")
cat(sep="\n")
summary(nosur.brm)
cat(sep="\n")

cat("***************NoticeIncSurv*****************")
cat(sep="\n")
car::Anova(incsur.glm)
cat(sep="\n")
tidy(incsur.glm)
cat(sep="\n")
confint(incsur.glm)
cat(sep="\n")

cat("**************NeedMoreLess******************")
cat(sep="\n")
summary(moreless.brm)
cat(sep="\n")

cat("****************Inclined****************")
cat(sep="\n")
summary(inclined.brm)
cat(sep="\n")

cat("*************Declined*******************")
cat(sep="\n")
summary(declined.brm)
cat(sep="\n")

surveillance$NoticeSurvn= as.numeric(surveillance$NoticeSurvn)	
surveillance$NoticeChangeSurvn= as.numeric(surveillance$NoticeChangeSurvn)	
#CORRELATION
cat("**************correlation between noticing questions******************")
cat(sep="\n")
notice = table(surveillance$NoticeSurvn, surveillance$NoticeChangeSurvn) #rows, columns
notice
prop.table(notice, 1) # row percentages

cor.test(surveillance$NoticeSurvn, surveillance$NoticeChangeSurvn,
         method = "kendall", alpha=.05, conf.level = TRUE)
sink()

library(PerformanceAnalytics)
notsurv.test=cbind(surveillance$NoticeSurvn, surveillance$NoticeChangeSurvn)
chart.Correlation(notsurv.test,
                  method="kendall",
                  pch=12)
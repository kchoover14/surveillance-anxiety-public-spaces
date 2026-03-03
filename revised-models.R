######################## LIBRARIES ########################

library(readxl)             # read Excel data files
library(broom)              # tidy model output
library(PerformanceAnalytics) # correlation matrix plots
library(ordinal)            # cumulative link models (diagnostic use)
library(brms)               # Bayesian multilevel models via Stan
library(coda)               # MCMC diagnostics
library(performance)        # binned residual plots for GLM fit checks
library(see)                # visualization support for performance
library(car)                # Anova() for GLM likelihood ratio tests
library(ggplot2)            # ggsave function for pp_check on brms

######################## DATA ########################

demog        = read_excel("revised-data.xlsx", sheet = "demog")
gensafety    = read_excel("revised-data.xlsx", sheet = "gensafety")
concerns     = read_excel("revised-data.xlsx", sheet = "specsafety")
measures     = read_excel("revised-data.xlsx", sheet = "measures")
feelings     = read_excel("revised-data.xlsx", sheet = "feelings")
surveillance = read_excel("revised-data.xlsx", sheet = "surveillance")
long         = read_excel("revised-data.xlsx", sheet = "long")

# demog
demog$Age2        = factor(demog$Age2)
demog$Gender      = factor(demog$Gender)
demog$Sex         = factor(demog$Sex)
demog$SexOrient   = factor(demog$SexOrient)
demog$Experience  = factor(demog$Experience)
demog$Experiencen = as.numeric(demog$Experiencen)

# gensafety
gensafety$Sex         = factor(gensafety$Sex)
gensafety$SexOrient   = factor(gensafety$SexOrient)
gensafety$PerSafetyn  = ordered(gensafety$PerSafetyn)
gensafety$Changesn    = ordered(gensafety$Changesn)
gensafety$Experience  = ordered(gensafety$Experience)
gensafety$Experiencen = ordered(gensafety$Experiencen)

# concerns
concerns$Sex         = factor(concerns$Sex)
concerns$SexOrient   = factor(concerns$SexOrient)
concerns$Experience  = ordered(concerns$Experience)
concerns$Experiencen = ordered(concerns$Experiencen)

# measures
measures$Sex         = factor(measures$Sex)
measures$SexOrient   = factor(measures$SexOrient)
measures$Experience  = ordered(measures$Experience)
measures$Experiencen = ordered(measures$Experiencen)

# feelings
feelings$Sex         = factor(feelings$Sex)
feelings$SexOrient   = factor(feelings$SexOrient)
feelings$Experience  = ordered(feelings$Experience)
feelings$Experiencen = ordered(feelings$Experiencen)

# surveillance
surveillance$Sex                     = factor(surveillance$Sex)
surveillance$SexOrient               = factor(surveillance$SexOrient)
surveillance$Experience              = ordered(surveillance$Experience)
surveillance$Experiencen             = ordered(surveillance$Experiencen)
surveillance$NoticeSurvn             = ordered(surveillance$NoticeSurvn)
surveillance$NoticeChangeSurvn       = ordered(surveillance$NoticeChangeSurvn)
surveillance$NeedMoreLessn           = ordered(surveillance$NeedMoreLessn)
surveillance$DeclineUnsafen          = ordered(surveillance$DeclineUnsafen)
surveillance$IntroSurvInclineAttendn = ordered(surveillance$IntroSurvInclineAttendn)

######################## DESCRIPTIVES AND CORRELATION ########################

summary(demog[2:6])

cor.test(demog$Age2n, demog$Experiencen,
         method = "kendall", alternative = "greater",
         alpha = .05, conf.level = TRUE)

age.test = cbind(demog$Age2n, demog$Experiencen)
chart.Correlation(age.test, method = "kendall", pch = 12)

sink(file = "revised-results-descriptives.txt", append = FALSE)
cat("************** SAMPLE SUMMARY ********************\n")
summary(demog[2:6])
cat("\n************** AGE x EXPERIENCE CORRELATION ******\n")
cor.test(demog$Age2n, demog$Experiencen,
         method = "kendall", alternative = "greater",
         alpha = .05, conf.level = TRUE)
sink()

######################## PROPORTION TABLES ########################

safety_tbl  = table(long$SafetyCat, long$SafetyData)
safer_tbl   = table(long$SaferCat, long$SaferData)
feelings_tbl = table(long$FeelingsCat, long$FeelingsData)

sink(file = "revised-results-proportion-tables.txt", append = FALSE)
cat("************** SAFETY CONCERNS ********************\n")
safety_tbl
prop.table(safety_tbl, 1)
cat("\n************** SAFETY MEASURES ********************\n")
safer_tbl
prop.table(safer_tbl, 1)
cat("\n************** FEELINGS ABOUT SURVEILLANCE ********\n")
feelings_tbl
prop.table(feelings_tbl, 1)
sink()

######################## BRM: GENERAL SAFETY ########################
#perceived safety
safety.brm = brm(
  formula = PerSafetyn ~ Experience + Sex + SexOrient,
  data    = gensafety,
  cores   = getOption("mc.cores", 4),
  family  = cumulative("logit")
)
ggsave("revised-sfig-ppcheck-safety.png",
       plot = pp_check(safety.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)
#changes
changes.brm = brm(
  formula = Changesn ~ Experience + Sex + SexOrient,
  data    = gensafety,
  cores   = getOption("mc.cores", 4),
  family  = cumulative("logit")
)
ggsave("revised-sfig-ppcheck-changes.png",
       plot = pp_check(changes.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)


#sink results
sink(file = "revised-results-general-safety.txt", append = FALSE)
cat("************** PERSONAL SAFETY ********************\n")
summary(safety.brm)
cat("\n************** CHANGES TO PERSONAL SAFETY *********\n")
summary(changes.brm)
sink()

#save R data
saveRDS(safety.brm,  "safety_brm.rds")
saveRDS(changes.brm, "changes_brm.rds")

######################## GLMs: SPECIFIC SAFETY CONCERNS ########################
Physical.glm = glm(Physical ~ Sex + Experience + SexOrient,
                   family = binomial, data = concerns)
CrowdViolence.glm = glm(CrowdViolence ~ Sex + Experience + SexOrient,
                        family = binomial, data = concerns)
Sexual.glm = glm(Sexual ~ Sex + Experience + SexOrient,
                 family = binomial, data = concerns)
Terrorism.glm = glm(Terrorism ~ Sex + Experience + SexOrient,
                    family = binomial, data = concerns)
Police.con.glm = glm(Police ~ Sex + Experience + SexOrient,
                     family = binomial, data = concerns)
UnwantedSecurity.glm = glm(UnwantedSecurity ~ Sex + Experience + SexOrient,
                           family = binomial, data = concerns)

sink(file = "revised-results-safety-concerns.txt", append = FALSE)
cat("************** ANOVA ********************\n")
cat("Physical\n")
Anova(Physical.glm)
cat("\nCrowd Violence\n")
Anova(CrowdViolence.glm)
cat("\nSexual\n")
Anova(Sexual.glm)
cat("\nTerrorism\n")
Anova(Terrorism.glm)
cat("\nPolice\n")
Anova(Police.con.glm)
cat("\nUnwanted Security\n")
Anova(UnwantedSecurity.glm)

cat("\n\n************** SUMMARIES AND CONFIDENCE INTERVALS **\n")
cat("Physical\n")
tidy(Physical.glm)
confint(Physical.glm)
cat("\nCrowd Violence\n")
tidy(CrowdViolence.glm)
confint(CrowdViolence.glm)
cat("\nSexual\n")
tidy(Sexual.glm)
confint(Sexual.glm)
cat("\nTerrorism\n")
tidy(Terrorism.glm)
confint(Terrorism.glm)
cat("\nPolice\n")
tidy(Police.con.glm)
confint(Police.con.glm)
cat("\nUnwanted Security\n")
tidy(UnwantedSecurity.glm)
confint(UnwantedSecurity.glm)

cat("\n\n************** WEIGHTED MEANS ********************\n")
cat("Physical\n")
tapply(concerns$Physical, concerns$Experience, weighted.mean)
tapply(concerns$Physical, concerns$Sex, weighted.mean)
tapply(concerns$Physical, concerns$SexOrient, weighted.mean)
cat("\nCrowd Violence\n")
tapply(concerns$CrowdViolence, concerns$Experience, weighted.mean)
tapply(concerns$CrowdViolence, concerns$Sex, weighted.mean)
tapply(concerns$CrowdViolence, concerns$SexOrient, weighted.mean)
cat("\nSexual\n")
tapply(concerns$Sexual, concerns$Experience, weighted.mean)
tapply(concerns$Sexual, concerns$Sex, weighted.mean)
tapply(concerns$Sexual, concerns$SexOrient, weighted.mean)
cat("\nTerrorism\n")
tapply(concerns$Terrorism, concerns$Experience, weighted.mean)
tapply(concerns$Terrorism, concerns$Sex, weighted.mean)
tapply(concerns$Terrorism, concerns$SexOrient, weighted.mean)
cat("\nPolice\n")
tapply(concerns$Police, concerns$Experience, weighted.mean)
tapply(concerns$Police, concerns$Sex, weighted.mean)
tapply(concerns$Police, concerns$SexOrient, weighted.mean)
cat("\nUnwanted Security\n")
tapply(concerns$UnwantedSecurity, concerns$Experience, weighted.mean)
tapply(concerns$UnwantedSecurity, concerns$Sex, weighted.mean)
tapply(concerns$UnwantedSecurity, concerns$SexOrient, weighted.mean)
sink()

saveRDS(Physical.glm,      "Physical_glm.rds")
saveRDS(CrowdViolence.glm, "CrowdViolence_glm.rds")
saveRDS(Sexual.glm,        "Sexual_glm.rds")
saveRDS(Terrorism.glm,     "Terrorism_glm.rds")

######################## GLMs: SAFETY MEASURES ########################

Location.glm = glm(Location ~ Sex + Experience + SexOrient,
                   family = binomial, data = measures)
EthosVibe.glm = glm(EthosVibe ~ Sex + Experience + SexOrient,
                    family = binomial, data = measures)
Police.meas.glm = glm(Police ~ Sex + Experience + SexOrient,
                      family = binomial, data = measures)
Surveillance.meas.glm = glm(Surveillance ~ Sex + Experience + SexOrient,
                            family = binomial, data = measures)
PvtSecurity.glm = glm(PvtSecurity ~ Sex + Experience + SexOrient,
                      family = binomial, data = measures)
CitSecurity.glm = glm(CitSecurity ~ Sex + Experience + SexOrient,
                      family = binomial, data = measures)
Friends.meas.glm = glm(Friends ~ Sex + Experience + SexOrient,
                       family = binomial, data = measures)
HealthTents.glm = glm(HealthTents ~ Sex + Experience + SexOrient,
                      family = binomial, data = measures)

# Model fit checks
binned_residuals(Location.glm)
binned_residuals(EthosVibe.glm)
binned_residuals(Police.meas.glm)
binned_residuals(Surveillance.meas.glm)
binned_residuals(PvtSecurity.glm)
binned_residuals(CitSecurity.glm)
binned_residuals(Friends.meas.glm)
binned_residuals(HealthTents.glm)

sink(file = "revised-results-safety-measures.txt", append = FALSE)
cat("************** ANOVA ********************\n")
cat("Location\n")
Anova(Location.glm)
cat("\nEthos/Vibe\n")
Anova(EthosVibe.glm)
cat("\nPolice\n")
Anova(Police.meas.glm)
cat("\nSurveillance\n")
Anova(Surveillance.meas.glm)
cat("\nPrivate Security\n")
Anova(PvtSecurity.glm)
cat("\nCitizen Security\n")
Anova(CitSecurity.glm)
cat("\nFriends\n")
Anova(Friends.meas.glm)
cat("\nHealth Tents\n")
Anova(HealthTents.glm)

cat("\n\n************** SUMMARIES AND CONFIDENCE INTERVALS **\n")
cat("Location\n")
tidy(Location.glm)
confint(Location.glm)
cat("\nEthos/Vibe\n")
tidy(EthosVibe.glm)
confint(EthosVibe.glm)
cat("\nPolice\n")
tidy(Police.meas.glm)
confint(Police.meas.glm)
cat("\nSurveillance\n")
tidy(Surveillance.meas.glm)
confint(Surveillance.meas.glm)
cat("\nPrivate Security\n")
tidy(PvtSecurity.glm)
confint(PvtSecurity.glm)
cat("\nCitizen Security\n")
tidy(CitSecurity.glm)
confint(CitSecurity.glm)
cat("\nFriends\n")
tidy(Friends.meas.glm)
confint(Friends.meas.glm)
cat("\nHealth Tents\n")
tidy(HealthTents.glm)
confint(HealthTents.glm)

cat("\n\n************** WEIGHTED MEANS ********************\n")
cat("Location\n")
tapply(measures$Location, measures$Experience, weighted.mean)
tapply(measures$Location, measures$Sex, weighted.mean)
tapply(measures$Location, measures$SexOrient, weighted.mean)
cat("\nEthos/Vibe\n")
tapply(measures$EthosVibe, measures$Experience, weighted.mean)
tapply(measures$EthosVibe, measures$Sex, weighted.mean)
tapply(measures$EthosVibe, measures$SexOrient, weighted.mean)
cat("\nPolice\n")
tapply(measures$Police, measures$Experience, weighted.mean)
tapply(measures$Police, measures$Sex, weighted.mean)
tapply(measures$Police, measures$SexOrient, weighted.mean)
cat("\nSurveillance\n")
tapply(measures$Surveillance, measures$Experience, weighted.mean)
tapply(measures$Surveillance, measures$Sex, weighted.mean)
tapply(measures$Surveillance, measures$SexOrient, weighted.mean)
cat("\nPrivate Security\n")
tapply(measures$PvtSecurity, measures$Experience, weighted.mean)
tapply(measures$PvtSecurity, measures$Sex, weighted.mean)
tapply(measures$PvtSecurity, measures$SexOrient, weighted.mean)
cat("\nCitizen Security\n")
tapply(measures$CitSecurity, measures$Experience, weighted.mean)
tapply(measures$CitSecurity, measures$Sex, weighted.mean)
tapply(measures$CitSecurity, measures$SexOrient, weighted.mean)
cat("\nFriends\n")
tapply(measures$Friends, measures$Experience, weighted.mean)
tapply(measures$Friends, measures$Sex, weighted.mean)
tapply(measures$Friends, measures$SexOrient, weighted.mean)
cat("\nHealth Tents\n")
tapply(measures$HealthTents, measures$Experience, weighted.mean)
tapply(measures$HealthTents, measures$Sex, weighted.mean)
tapply(measures$HealthTents, measures$SexOrient, weighted.mean)
sink()

saveRDS(Police.meas.glm,      "Police_meas_glm.rds")
saveRDS(Surveillance.meas.glm, "Surveillance_meas_glm.rds")
saveRDS(CitSecurity.glm,      "CitSecurity_glm.rds")
saveRDS(Friends.meas.glm,     "Friends_meas_glm.rds")

######################## GLMs: FEELINGS ABOUT SURVEILLANCE ########################
Safe.glm = glm(Safe ~ Sex + Experience + SexOrient,
               family = binomial, data = feelings)
Watched.glm = glm(Watched ~ Sex + Experience + SexOrient,
                  family = binomial, data = feelings)
Indifferent.glm = glm(Indifferent ~ Sex + Experience + SexOrient,
                      family = binomial, data = feelings)
Anxious.glm = glm(Anxious ~ Sex + Experience + SexOrient,
                  family = binomial, data = feelings)
AtRisk.glm = glm(AtRisk ~ Sex + Experience + SexOrient,
                 family = binomial, data = feelings)
ChangesVibe.glm = glm(ChangesVibe ~ Sex + Experience + SexOrient,
                      family = binomial, data = feelings)

sink(file = "revised-results-feelings.txt", append = FALSE)
cat("************** ANOVA ********************\n")
cat("Safe\n")
Anova(Safe.glm)
cat("\nWatched\n")
Anova(Watched.glm)
cat("\nIndifferent\n")
Anova(Indifferent.glm)
cat("\nAnxious\n")
Anova(Anxious.glm)
cat("\nAt Risk\n")
Anova(AtRisk.glm)
cat("\nChanges Vibe\n")
Anova(ChangesVibe.glm)

cat("\n\n************** SUMMARIES AND CONFIDENCE INTERVALS **\n")
cat("Safe\n")
tidy(Safe.glm)
confint(Safe.glm)
cat("\nWatched\n")
tidy(Watched.glm)
confint(Watched.glm)
cat("\nIndifferent\n")
tidy(Indifferent.glm)
confint(Indifferent.glm)
cat("\nAnxious\n")
tidy(Anxious.glm)
confint(Anxious.glm)
cat("\nAt Risk\n")
tidy(AtRisk.glm)
confint(AtRisk.glm)
cat("\nChanges Vibe\n")
tidy(ChangesVibe.glm)
confint(ChangesVibe.glm)

cat("\n\n************** WEIGHTED MEANS ********************\n")
cat("Safe\n")
tapply(feelings$Safe, feelings$Experience, weighted.mean)
tapply(feelings$Safe, feelings$Sex, weighted.mean)
tapply(feelings$Safe, feelings$SexOrient, weighted.mean)
cat("\nWatched\n")
tapply(feelings$Watched, feelings$Experience, weighted.mean)
tapply(feelings$Watched, feelings$Sex, weighted.mean)
tapply(feelings$Watched, feelings$SexOrient, weighted.mean)
cat("\nIndifferent\n")
tapply(feelings$Indifferent, feelings$Experience, weighted.mean)
tapply(feelings$Indifferent, feelings$Sex, weighted.mean)
tapply(feelings$Indifferent, feelings$SexOrient, weighted.mean)
cat("\nAnxious\n")
tapply(feelings$Anxious, feelings$Experience, weighted.mean)
tapply(feelings$Anxious, feelings$Sex, weighted.mean)
tapply(feelings$Anxious, feelings$SexOrient, weighted.mean)
cat("\nAt Risk\n")
tapply(feelings$AtRisk, feelings$Experience, weighted.mean)
tapply(feelings$AtRisk, feelings$Sex, weighted.mean)
tapply(feelings$AtRisk, feelings$SexOrient, weighted.mean)
cat("\nChanges Vibe\n")
tapply(feelings$ChangesVibe, feelings$Experience, weighted.mean)
tapply(feelings$ChangesVibe, feelings$Sex, weighted.mean)
tapply(feelings$ChangesVibe, feelings$SexOrient, weighted.mean)
sink()

saveRDS(Safe.glm,        "Safe_glm.rds")
saveRDS(ChangesVibe.glm, "ChangesVibe_glm.rds")

######################## BRM: SURVEILLANCE ########################
# notice surveillance
nosur.brm = brm(
  formula = NoticeSurvn ~ Sex + Experience + SexOrient,
  data    = surveillance,
  family  = cumulative("logit"),
  cores   = getOption("mc.cores", 4)
)
ggsave("revised-sfig-ppcheck-nosur.png",
       plot = pp_check(nosur.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)

#notice change surveillance
incsur.glm = glm(NoticeChangeSurvn ~ Sex + Experience + SexOrient,
                 family = binomial, data = surveillance)
moreless.brm = brm(
  formula = NeedMoreLessn ~ Sex + Experience + SexOrient,
  data    = surveillance,
  family  = cumulative("logit"),
  cores   = getOption("mc.cores", 4)
)
ggsave("revised-sfig-ppcheck-moreless.png",
       plot = pp_check(moreless.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)

#inclined to attend
inclined.brm = brm(
  formula = IntroSurvInclineAttendn ~ Sex + Experience + SexOrient,
  data    = surveillance,
  family  = cumulative("logit"),
  cores   = getOption("mc.cores", 4)
)
ggsave("revised-sfig-ppcheck-inclined.png",
       plot = pp_check(inclined.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)

#declined to attend
declined.brm = brm(
  formula = DeclineUnsafen ~ Sex + Experience + SexOrient,
  data    = surveillance,
  family  = cumulative("logit"),
  cores   = getOption("mc.cores", 4)
)
ggsave("revised-sfig-ppcheck-declined.png",
       plot = pp_check(declined.brm, ndraws = 100),
       width = 6, height = 4, dpi = 150)

# Correlation between noticing questions
surveillance$NoticeSurvn_n       = as.numeric(surveillance$NoticeSurvn)
surveillance$NoticeChangeSurvn_n = as.numeric(surveillance$NoticeChangeSurvn)
notice = table(surveillance$NoticeSurvn_n, surveillance$NoticeChangeSurvn_n)
notsurv.test = cbind(surveillance$NoticeSurvn_n, surveillance$NoticeChangeSurvn_n)
chart.Correlation(notsurv.test, method = "kendall", pch = 12)

sink(file = "revised-results-surveillance.txt", append = FALSE)
cat("************** NOTICE SURVEILLANCE ****************\n")
summary(nosur.brm)
cat("\n************** NOTICE INCREASE IN SURVEILLANCE ****\n")
Anova(incsur.glm)
tidy(incsur.glm)
confint(incsur.glm)
cat("\n************** NEED MORE OR LESS SURVEILLANCE ******\n")
summary(moreless.brm)
cat("\n************** INCLINED TO ATTEND IF SURVEILLANCE **\n")
summary(inclined.brm)
cat("\n************** DECLINED TO ATTEND -- FELT UNSAFE ***\n")
summary(declined.brm)
cat("\n************** CORRELATION -- NOTICING QUESTIONS ***\n")
notice
prop.table(notice, 1)
cor.test(surveillance$NoticeSurvn_n, surveillance$NoticeChangeSurvn_n,
         method = "kendall", alpha = .05, conf.level = TRUE)
sink()

saveRDS(nosur.brm,    "nosur_brm.rds")
saveRDS(incsur.glm,   "incsur_glm.rds")
saveRDS(moreless.brm, "moreless_brm.rds")
saveRDS(inclined.brm, "inclined_brm.rds")
saveRDS(declined.brm, "declined_brm.rds")


######################## MODEL FIT CHECKS ########################
sink(file = "revised-results-fit-checks.txt", append = FALSE)
cat("************** SPECIFIC SAFETY CONCERNS **********\n")
cat("Physical\n");        print(binned_residuals(Physical.glm))
cat("\nCrowd Violence\n"); print(binned_residuals(CrowdViolence.glm))
cat("\nSexual\n");         print(binned_residuals(Sexual.glm))
cat("\nTerrorism\n");      print(binned_residuals(Terrorism.glm))
cat("\nPolice\n");         print(binned_residuals(Police.con.glm))
cat("\nUnwanted Security\n"); print(binned_residuals(UnwantedSecurity.glm))
cat("\n************** SAFETY MEASURES *******************\n")
cat("Location\n");        print(binned_residuals(Location.glm))
cat("\nEthos/Vibe\n");    print(binned_residuals(EthosVibe.glm))
cat("\nPolice\n");        print(binned_residuals(Police.meas.glm))
cat("\nSurveillance\n");  print(binned_residuals(Surveillance.meas.glm))
cat("\nPrivate Security\n"); print(binned_residuals(PvtSecurity.glm))
cat("\nCitizen Security\n"); print(binned_residuals(CitSecurity.glm))
cat("\nFriends\n");       print(binned_residuals(Friends.meas.glm))
cat("\nHealth Tents\n");  print(binned_residuals(HealthTents.glm))
cat("\n************** FEELINGS ABOUT SURVEILLANCE *******\n")
cat("Safe\n");             print(binned_residuals(Safe.glm))
cat("\nWatched\n");       print(binned_residuals(Watched.glm))
cat("\nIndifferent\n");   print(binned_residuals(Indifferent.glm))
cat("\nAnxious\n");       print(binned_residuals(Anxious.glm))
cat("\nAt Risk\n");       print(binned_residuals(AtRisk.glm))
cat("\nChanges Vibe\n");  print(binned_residuals(ChangesVibe.glm))
cat("\n************** SURVEILLANCE BEHAVIOUR ************\n")
cat("Notice change surveillance\n"); print(binned_residuals(incsur.glm))
sink()

######################## TIDY
rm(list=ls())
gc()

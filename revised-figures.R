######################## LIBRARIES ########################

library(readxl)    # read Excel data files
library(brms)      # needed to work with saved BRM objects
library(ggplot2)   # figures
library(scales)    # percent formatting on axes
library(cowplot)   # multi-panel figure assembly
library(ggeffects) # marginal effects from GLMs and BRMs

######################## DATA ########################

gensafety    = read_excel("revised-data.xlsx", sheet = "gensafety")
concerns     = read_excel("revised-data.xlsx", sheet = "specsafety")
measures     = read_excel("revised-data.xlsx", sheet = "measures")
feelings     = read_excel("revised-data.xlsx", sheet = "feelings")
surveillance = read_excel("revised-data.xlsx", sheet = "surveillance")

# gensafety -- numeric for jitter color, ordered for model
gensafety$Sex             = factor(gensafety$Sex)
gensafety$Experiencen_num = as.numeric(gensafety$Experiencen)

# concerns
concerns$Sex             = factor(concerns$Sex)
concerns$SexOrient       = factor(concerns$SexOrient)
concerns$Experiencen_num = as.numeric(concerns$Experiencen)

# measures
measures$Sex             = factor(measures$Sex)
measures$SexOrient       = factor(measures$SexOrient)
measures$Experiencen_num = as.numeric(measures$Experiencen)

# feelings
feelings$Sex             = factor(feelings$Sex)
feelings$SexOrient       = factor(feelings$SexOrient)
feelings$Experiencen_num = as.numeric(feelings$Experiencen)

# surveillance
surveillance$Sex             = factor(surveillance$Sex)
surveillance$SexOrient       = factor(surveillance$SexOrient)
surveillance$Experiencen_num = as.numeric(surveillance$Experiencen)

######################## LOAD MODEL OBJECTS ########################

safety.brm         = readRDS("safety_brm.rds")
Physical.glm       = readRDS("Physical_glm.rds")
CrowdViolence.glm  = readRDS("CrowdViolence_glm.rds")
Sexual.glm         = readRDS("Sexual_glm.rds")
Terrorism.glm      = readRDS("Terrorism_glm.rds")
Police.meas.glm    = readRDS("Police_meas_glm.rds")
Surveillance.meas.glm = readRDS("Surveillance_meas_glm.rds")
CitSecurity.glm    = readRDS("CitSecurity_glm.rds")
Friends.meas.glm   = readRDS("Friends_meas_glm.rds")
Safe.glm           = readRDS("Safe_glm.rds")
ChangesVibe.glm    = readRDS("ChangesVibe_glm.rds")
incsur.glm         = readRDS("incsur_glm.rds")
moreless.brm       = readRDS("moreless_brm.rds")
inclined.brm       = readRDS("inclined_brm.rds")
declined.brm       = readRDS("declined_brm.rds")

######################## FIGURE 1: GENERAL SAFETY CONCERNS ########################

# Panel A -- general safety concerns by experience
safety.expjit = ggplot(gensafety, aes(Experiencen_num, PerSafetyn)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  scale_y_discrete(limits = c("None", "Minor", "Major")) +
  labs(x = "", y = "General Safety Concerns") +
  theme_classic()

safety.Experience = conditional_effects(safety.brm, "Experience", categorical = TRUE)
safety.expmarg = plot(safety.Experience, plot = FALSE)[[1]] +
  scale_color_viridis_d(breaks = c("1", "2", "3"),
                        labels = c("None", "Minor", "Major")) +
  scale_fill_discrete(breaks = c("1", "2", "3"),
                      labels = c("None", "Minor", "Major")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

# Panel B -- general safety concerns by sex
safety.sexjit = ggplot(gensafety, aes(Sexn, PerSafetyn)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  scale_y_discrete(limits = c("None", "Minor", "Major")) +
  labs(x = "", y = "General Safety Concerns") +
  theme_classic()

safety.Sex = conditional_effects(safety.brm, "Sex", categorical = TRUE)
safety.sexmarg = plot(safety.Sex, plot = FALSE)[[1]] +
  scale_color_viridis_d(breaks = c("1", "2", "3"),
                        labels = c("None", "Minor", "Major")) +
  scale_fill_discrete(breaks = c("1", "2", "3"),
                      labels = c("None", "Minor", "Major")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

fig1 = plot_grid(safety.expjit, safety.expmarg,
                 safety.sexjit, safety.sexmarg,
                 labels = c("A", "", "B", ""),
                 ncol = 2, rel_widths = c(1, 1))

ggsave("revised-fig1.png", plot = fig1, width = 6, height = 6,
       units = "in", dpi = 300)

######################## FIGURE 2: SPECIFIC SAFETY CONCERNS ########################

# Panel A -- physical safety concerns by experience
Physical.expjit = ggplot(concerns, aes(Experiencen_num, Physical)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "Physical") +
  theme_classic()

physical.Experience = ggeffect(Physical.glm, terms = "Experience")
physical.expmarg = plot(physical.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel B -- crowd violence concerns by experience
CrowdViolence.expjit = ggplot(concerns, aes(Experiencen_num, CrowdViolence)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "Crowd Violence") +
  theme_classic()

crowdviolence.Experience = ggeffect(CrowdViolence.glm, terms = "Experience")
crowdviolence.expmarg = plot(crowdviolence.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel C -- crowd violence concerns by sex
CrowdViolence.sexjit = ggplot(concerns, aes(Sexn, CrowdViolence)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "") +
  theme_classic()

crowdviolence.Sex = ggeffect(CrowdViolence.glm, terms = "Sex")
crowdviolence.sexmarg = plot(crowdviolence.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel D -- sexual safety concerns by sex
sexual.sexjit = ggplot(concerns, aes(Sexn, Sexual)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Sexual Safety") +
  theme_classic()

sexual.Sex = ggeffect(Sexual.glm, terms = "Sex")
sexual.sexmarg = plot(sexual.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel E -- terrorism concerns by sex
terrorism.sexjit = ggplot(concerns, aes(Sexn, Terrorism)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Terrorism") +
  theme_classic()

terrorism.Sex = ggeffect(Terrorism.glm, terms = "Sex")
terrorism.sexmarg = plot(terrorism.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel F -- terrorism concerns by sexual orientation
terrorism.sexorientjit = ggplot(concerns, aes(SexOrientn, Terrorism)) +
  geom_jitter(aes(color = SexOrientn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black", span = 1) +
  scale_x_discrete(limits = c("Heterosexual", "LGBQA")) +
  labs(x = "", y = "") +
  theme_classic()

terrorism.SexOrient = ggeffect(Terrorism.glm, terms = "SexOrient")
terrorism.sexorientmarg = plot(terrorism.SexOrient) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

fig2 = plot_grid(
  Physical.expjit,       physical.expmarg,      NULL,                  NULL,
  CrowdViolence.expjit,  crowdviolence.expmarg,  CrowdViolence.sexjit,  crowdviolence.sexmarg,
  sexual.sexjit,         sexual.sexmarg,         NULL,                  NULL,
  terrorism.sexjit,      terrorism.sexmarg,      terrorism.sexorientjit, terrorism.sexorientmarg,
  labels = c("A", "", "", "",
             "B", "", "C", "",
             "D", "", "", "",
             "E", "", "F", ""),
  ncol = 4, rel_widths = c(1, 1)
)

ggsave("revised-fig2.png", plot = fig2, width = 12, height = 12,
       units = "in", dpi = 300)

######################## FIGURE 3: SAFETY MEASURES ########################

# Panel A -- police as safety measure by sex
police.sexjit = ggplot(measures, aes(Sexn, Police)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Police") +
  theme_classic()

police.Sex = ggeffect(Police.meas.glm, terms = "Sex")
police.sexmarg = plot(police.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel B -- surveillance as safety measure by sex
surveillance.sexjit = ggplot(measures, aes(Sexn, Surveillance)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Surveillance") +
  theme_classic()

surveillance.Sex = ggeffect(Surveillance.meas.glm, terms = "Sex")
surveillance.sexmarg = plot(surveillance.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel C -- citizen security as safety measure by experience
citsec.expjit = ggplot(measures, aes(Experiencen_num, CitSecurity)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "Citizen Security") +
  theme_classic()

citsec.Experience = ggeffect(CitSecurity.glm, terms = "Experience")
citsec.expmarg = plot(citsec.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel D -- friends as safety measure by sex
friends.sexjit = ggplot(measures, aes(Sexn, Friends)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Friends") +
  theme_classic()

friends.Sex = ggeffect(Friends.meas.glm, terms = "Sex")
friends.sexmarg = plot(friends.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

fig3 = plot_grid(
  police.sexjit,      police.sexmarg,
  surveillance.sexjit, surveillance.sexmarg,
  citsec.expjit,      citsec.expmarg,
  friends.sexjit,     friends.sexmarg,
  labels = c("A", "", "B", "", "C", "", "D", ""),
  ncol = 2, rel_widths = c(1, 1)
)

ggsave("revised-fig3.png", plot = fig3, width = 6, height = 12,
       units = "in", dpi = 300)

######################## FIGURE 4: FEELINGS ABOUT SURVEILLANCE ########################

# Panel A -- feeling safe by sex
safe.sexjit = ggplot(feelings, aes(Sexn, Safe)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  labs(x = "", y = "Feeling Safe") +
  theme_classic()

safe.Sex = ggeffect(Safe.glm, terms = "Sex")
safe.sexmarg = plot(safe.Sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel B -- feeling safe by experience
safe.expjit = ggplot(feelings, aes(Experiencen_num, Safe)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "") +
  theme_classic()

safe.Experience = ggeffect(Safe.glm, terms = "Experience")
safe.expmarg = plot(safe.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel C -- changes vibe by sexual orientation
changesvibe.sexorientjit = ggplot(feelings, aes(SexOrientn, ChangesVibe)) +
  geom_jitter(aes(color = SexOrientn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black", span = 2) +
  scale_x_discrete(limits = c("Heterosexual", "LGBQA")) +
  labs(x = "", y = "Changes Vibe") +
  theme_classic()

changesvibe.SexOrient = ggeffect(ChangesVibe.glm, terms = "SexOrient")
changesvibe.sexorientmarg = plot(changesvibe.SexOrient) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  scale_x_discrete(limits = c("Heterosexual", "LGBQA")) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel D -- changes vibe by experience
changesvibe.expjit = ggplot(feelings, aes(Experiencen_num, ChangesVibe)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black", span = 2) +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "") +
  theme_classic()

changesvibe.Experience = ggeffect(ChangesVibe.glm, terms = "Experience")
changesvibe.expmarg = plot(changesvibe.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

fig4 = plot_grid(
  safe.sexjit,             safe.sexmarg,
  safe.expjit,             safe.expmarg,
  changesvibe.sexorientjit, changesvibe.sexorientmarg,
  changesvibe.expjit,      changesvibe.expmarg,
  labels = c("A", "", "B", "", "C", "", "D", ""),
  ncol = 4, rel_widths = c(1, 1)
)

ggsave("revised-fig4.png", plot = fig4, width = 12, height = 6,
       units = "in", dpi = 300)

######################## FIGURE 5: SURVEILLANCE BEHAVIOUR ########################

# Panel A -- noticing changes in surveillance by experience
incsur.expjit = ggplot(surveillance, aes(Experiencen_num, NoticeChangeSurvn2)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  scale_y_discrete(limits = c("No Change", "Increased")) +
  labs(x = "", y = "Changes in Surveillance") +
  theme_classic()

incsur.Experience = ggeffect(incsur.glm, terms = "Experience")
incsur.expmarg = plot(incsur.Experience) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic()

# Panel B -- need for more or less surveillance by sex
moreless.sexjit = ggplot(surveillance, aes(Sexn, NeedMoreLessn)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  scale_y_discrete(limits = c("Less", "Same", "More")) +
  labs(x = "Sex", y = "Need +/- Surveillance") +
  theme_classic()

moreless.Sex = conditional_effects(moreless.brm, "Sex", categorical = TRUE)
moreless.sexmarg = plot(moreless.Sex, plot = FALSE)[[1]] +
  scale_color_viridis_d(breaks = c("1", "2", "3"),
                        labels = c("Less", "Same", "More")) +
  scale_fill_discrete(breaks = c("1", "2", "3"),
                      labels = c("Less", "Same", "More")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

# Panel C -- inclined to attend if surveillance by experience
inclined.expjit = ggplot(surveillance, aes(Experiencen_num, IntroSurvInclineAttendn)) +
  geom_jitter(aes(color = Experiencen_num), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("1-2", "3-5", "6+")) +
  scale_y_discrete(limits = c("Less", "Neutral", "More")) +
  labs(x = "Experience", y = "Inclined to Attend if Surveillance") +
  theme_classic()

inclined.Experience = conditional_effects(inclined.brm, "Experience", categorical = TRUE)
inclined.expmarg = plot(inclined.Experience, plot = FALSE)[[1]] +
  scale_color_viridis_d(breaks = c("1", "2", "3"),
                        labels = c("Less", "Neutral", "More")) +
  scale_fill_discrete(breaks = c("1", "2", "3"),
                      labels = c("Less", "Neutral", "More")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

# Panel D -- declined to attend due to feeling unsafe by sex
declined.sexjit = ggplot(surveillance, aes(Sexn, DeclineUnsafen)) +
  geom_jitter(aes(color = Sexn), width = .3, height = .3,
              na.rm = TRUE, size = 1, show.legend = FALSE) +
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8) +
  stat_smooth(method = "loess", color = "black") +
  scale_x_discrete(limits = c("Female", "Male")) +
  scale_y_discrete(limits = c("No", "Once", "1+")) +
  labs(x = "Sex", y = "Declined b/c Unsafe") +
  theme_classic()

declined.Sex = conditional_effects(declined.brm, "Sex", categorical = TRUE)
declined.sexmarg = plot(declined.Sex, plot = FALSE)[[1]] +
  scale_color_viridis_d(breaks = c("1", "2", "3"),
                        labels = c("Never", "Once", "Several Times")) +
  scale_fill_discrete(breaks = c("1", "2", "3"),
                      labels = c("Never", "Once", "Several Times")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = seq(0, 1, .1)) +
  labs(x = "", y = "Probability of Response") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

fig5 = plot_grid(
  incsur.expjit,   incsur.expmarg,
  moreless.sexjit, moreless.sexmarg,
  inclined.expjit, inclined.expmarg,
  declined.sexjit, declined.sexmarg,
  labels = c("A", "", "B", "", "C", "", "D", ""),
  ncol = 2, rel_widths = c(1, 1)
)

ggsave("revised-fig5.png", plot = fig5, width = 6, height = 12,
       units = "in", dpi = 300)


######################## TIDY
rm(list=ls())
gc()


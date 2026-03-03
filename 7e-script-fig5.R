options(prompt="R>", scipen=100, digits=4)

library(brms)
library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
library(ggeffects)
surveillance=read_excel("festivaldata-full.xlsx", sheet="surveillance")
surveillance$Sex = factor(surveillance$Sex)
surveillance$Experiencen = as.numeric(surveillance$Experiencen)
surveillance$SexOrient = factor(surveillance$SexOrient)

#Jitters
incsur.expjit = ggplot(surveillance, aes(Experiencen, NoticeChangeSurvn2))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("1-2", "3-5", "6+")+
  ylim("No Change", "Increased")+
  labs(title = "", x="", y="Changes in Surveillance") +
  theme_classic()

moreless.sexjit = ggplot(surveillance, aes(Sexn, NeedMoreLessn))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("Female", "Male")+
  ylim("Less", "Same", "More")+
  labs(title = "", x="Sex", y="Need +/- Surveillance") +
  theme_classic()

inclined.expjit = ggplot(surveillance, aes(Experiencen, IntroSurvInclineAttendn))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("1-2", "3-5", "6+")+
  ylim("Less", "Neutral", "More")+
  labs(title = "", x="Experience", y="Inclined to Attend if Surveillance") +
  theme_classic()

declined.sexjit = ggplot(surveillance, aes(Sexn, DeclineUnsafen))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male")+
  ylim("No", "Once", "1+")+
  labs(title = "", x="Sex", y="Declined b/c Unsafe") +
  theme_classic()

#MODELS
surveillance$NeedMoreLessn = ordered(surveillance$NeedMoreLessn)
surveillance$IntroSurvInclineAttendn = ordered(surveillance$IntroSurvInclineAttendn)
surveillance$DeclineUnsafen = ordered(surveillance$DeclineUnsafen)	
surveillance$Experience = ordered(surveillance$Experience)
surveillance$Experiencen = ordered(surveillance$Experiencen)

incsur.glm = glm(NoticeChangeSurvn ~ Sex + Experience + SexOrient, 
                 family=binomial, data = surveillance)

moreless.brm = brm(
  formula= NeedMoreLessn ~ Sex + Experience + SexOrient, 
  data = surveillance,
  family = cumulative ("logit"),
  cores = getOption("mc.cores", 4)
  
)
inclined.brm = brm(
  formula = IntroSurvInclineAttendn ~ Sex + Experience + SexOrient, 
  data = surveillance,
  family = cumulative("logit"),
  cores = getOption("mc.cores", 4)
)
declined.brm = brm(
  formula = DeclineUnsafen ~ Sex + Experience + SexOrient, 
  data = surveillance,
  family = cumulative("logit"),
  cores = getOption("mc.cores", 4)
)

#Margins
incsur.Experience = ggeffect(incsur.glm, terms = "Experience")
incsur.expmarg = plot(incsur.Experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

moreless.Sex = conditional_effects(moreless.brm, "Sex", categorical = TRUE)
moreless.sexmarg = plot(moreless.Sex, plot=FALSE)[[1]]  +
  scale_color_viridis_d(name  ="",
                        breaks=c("1", "2", "3"),
                        labels=c("Less", "Same", "More")) +
  scale_fill_discrete(name  ="",
                      breaks=c("1", "2", "3"),
                      labels=c("Less", "Same", "More")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(x="", y="Probability of Response") +
  theme_classic() +
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

inclined.exp = conditional_effects(inclined.brm, "Experience", categorical = TRUE)
inclined.expmarg = plot(inclined.exp, plot=FALSE)[[1]]  +
  scale_color_viridis_d(name  ="",
                        breaks=c("1", "2", "3"),
                        labels=c("Less", "Neutral", "More")) +
  scale_fill_discrete(name  ="",
                      breaks=c("1", "2", "3"),
                      labels=c("Less", "Neutral", "More")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(x="", y="Probability of Response") +
  theme_classic() +
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

declined.Sex = conditional_effects(declined.brm, "Sex", categorical = TRUE)
declined.sexmarg = plot(declined.Sex, plot=FALSE)[[1]]  +
  scale_color_viridis_d(name  ="",
                        breaks=c("1", "2", "3"),
                        labels=c("Never", "Once", "Several Times")) +
  scale_fill_discrete(name  ="",
                      breaks=c("1", "2", "3"),
                      labels=c("Never", "Once", "Several Times")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(x="", y="Probability of Response") +
  theme_classic() +
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

#Figure
tiff(filename = "Figure 5-surveillance.tiff", width=6, height= 12, 
     units = "in",res=300, compression = "lzw", pointsize=12, type="cairo")
plot_grid(incsur.expjit, incsur.expmarg,
          moreless.sexjit, moreless.sexmarg,
          inclined.expjit, inclined.expmarg,
          declined.sexjit, declined.sexmarg,
          labels = c('A', '', 'B', '',
                     'C', '', 'D', '',
                     align = "h"),
          ncol =2, rel_widths = c(1, 1))
dev.off()


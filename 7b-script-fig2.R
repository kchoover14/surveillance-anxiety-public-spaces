options(prompt="R>", scipen=100, digits=4)

library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
library(readxl)
library(scales)
library(ggeffects)
concerns=read_excel("festivaldata-full.xlsx", sheet="specsafety")
concerns$Experiencen = ordered(concerns$Experiencen)
concerns$Sex = factor(concerns$Sex)
concerns$SexOrient = factor(concerns$SexOrient)
concerns$Sex = factor(concerns$Sex)
concerns$Experience = ordered(concerns$Experience)
concerns$SexOrient = factor(concerns$SexOrient)

#Jitters
concerns$Experiencen = as.numeric(concerns$Experiencen)
Physical.experiencejit = ggplot(concerns, aes(Experiencen, Physical))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("1-2", "3-5", "6+") +
  labs(title = "", x="", y="Physical") +
  theme_classic()

CrowdViolence.experiencejit = ggplot(concerns, aes(Experiencen, CrowdViolence))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("1-2", "3-5", "6+") +
  labs(title = "", x="", y="Crowd Violence") +
  theme_classic()

CrowdViolence.sexjit = ggplot(concerns, aes(Sexn, CrowdViolence))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male") +
  labs(title = "", x="", y="") +
  theme_classic()

sexual.sexjit = ggplot(concerns, aes(Sexn, Sexual))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male") +
  labs(title = "", x="", y="Sexual Safety") +
  theme_classic()

terrorism.sexjit = ggplot(concerns, aes(Sexn, Terrorism))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male") +
  labs(title = "", x="", y="Terrorism") +
  theme_classic()

terrorism.sexorientjit = ggplot(concerns, aes(SexOrientn, Terrorism))+
  geom_jitter(aes(color = SexOrientn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin = 0, end = 0.8)+
  stat_smooth(method = "loess", color="black", span = 1)+
  xlim("Heterosexual","LGBQA") +
  labs(title = "", x="", y="") +
  theme_classic()

#MODELS
Physical.glm = glm(Physical ~ Sex + Experience + SexOrient, 
                   family= binomial, data = concerns)
CrowdViolence.glm = glm(CrowdViolence ~ Sex + Experience + SexOrient, 
                        family= binomial, data = concerns)
Sexual.glm = glm(Sexual ~ Sex + Experience + SexOrient, 
                 family= binomial, data = concerns)
Terrorism.glm = glm(Terrorism ~ Sex + Experience + SexOrient, 
                    family= binomial, data = concerns)

#Margins
physical.Experience = ggeffect(Physical.glm, terms = "Experience")
physical.Experiencemarg = plot(physical.Experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

crowdviolence.Experience = ggeffect(CrowdViolence.glm, terms = "Experience")
crowdviolence.Experiencemarg = plot(crowdviolence.Experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

crowdviolence.Sex = ggeffect(CrowdViolence.glm, terms = "Sex")
crowdviolence.Sexmarg = plot(crowdviolence.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

sexual.Sex = ggeffect(Sexual.glm, terms = "Sex")
sexual.sexmarg = plot(sexual.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

terrorism.Sex = ggeffect(Terrorism.glm, terms = "Sex")
terrorism.sexmarg = plot(terrorism.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

terrorism.SexOrient = ggeffect(Terrorism.glm, terms = "SexOrient")
terrorism.sexorientmarg = plot(terrorism.SexOrient) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

#panel plot
tiff(filename = "Figure 2-safety.tiff", width=12, height= 12, 
     units = "in",res=300, compression = "lzw", pointsize=12, type="cairo")
plot_grid(Physical.experiencejit,physical.Experiencemarg, NULL, NULL,
          CrowdViolence.experiencejit,crowdviolence.Experiencemarg,
          CrowdViolence.sexjit, crowdviolence.Sexmarg,
          sexual.sexjit,sexual.sexmarg,NULL, NULL,
          terrorism.sexjit, terrorism.sexmarg,
          terrorism.sexorientjit, terrorism.sexorientmarg,
          labels = c('A', '', '','',
                     'B', '', 'C', '',
                     'D', '', '','',
                     'E', '','F', ''),
          ncol =4, rel_widths = c(1, 1))
dev.off()


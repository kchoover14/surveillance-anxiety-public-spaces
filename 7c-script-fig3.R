options(prompt="R>", scipen=100, digits=4)

library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
library(readxl)
library(scales)
library(ggeffects)

measures=read_excel("festivaldata-full.xlsx", sheet="measures")
measures$Sex = factor(measures$Sex)
measures$SexOrient = factor(measures$SexOrient)
measures$Experiencen = as.numeric(measures$Experiencen)

#Jitters
police.sexjit = ggplot(measures, aes(Sexn, Police))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("Female", "Male") +
  labs(title = "", x="", y="Police") +
  theme_classic()

surveillance.sexjit = ggplot(measures, aes(Sexn, Surveillance))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("Female", "Male") +
  labs(title = "", x="", y="Surveillance") +
  theme_classic()

citsec.experiencejit = ggplot(measures, aes(Experiencen, CitSecurity))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("1-1", "3-5", "6+") +
  labs(title = "", x="", y="Citizen Security") +
  theme_classic()

friends.sexjit = ggplot(measures, aes(Sexn, Friends))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("Female", "Male") +
  labs(title = "", x="", y="Friends") +
  theme_classic()

#MODELS
measures$Experience = ordered(measures$Experience)
measures$Experiencen = ordered(measures$Experiencen)
Police.glm = glm(Police ~ Sex + Experience + SexOrient, 
                  family = binomial, data = measures)
Surveillance.glm = glm(Surveillance ~ Sex + Experience + SexOrient, 
                       family = binomial, data = measures)
CitizenSecurity.glm = glm(Surveillance ~ Sex + Experience + SexOrient, 
                       family = binomial, data = measures)
Friends.glm = glm(Friends ~ Sex + Experience + SexOrient, 
                  family = binomial, data = measures)

#margins
police.Sex= ggeffect(Police.glm, terms = "Sex")
police.sexmarg = plot(police.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

surveillance.Sex= ggeffect(Surveillance.glm, terms = "Sex")
surveillance.sexmarg = plot(surveillance.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

citsec.experience = ggeffect(CitizenSecurity.glm, terms = "Experience")
citsec.expmarg = plot(citsec.experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

friends.Sex = ggeffect(Friends.glm, terms = "Sex")
friends.sexmarg = plot(friends.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

tiff(filename = "Figure 3-safer.tiff", width=6, height= 12, 
     units = "in",res=300, compression = "lzw", pointsize=12, type="cairo")
plot_grid(police.sexjit, police.sexmarg,
          surveillance.sexjit, surveillance.sexmarg, 
          citsec.experiencejit, citsec.expmarg,
          friends.sexjit, friends.sexmarg,
          labels = c('A', '',
                     'B', '',
                     'C', '',
                     'D', ''),
          ncol = 2, rel_widths = c(1, 1))
dev.off()

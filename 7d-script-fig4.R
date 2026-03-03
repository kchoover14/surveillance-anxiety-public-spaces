options(prompt="R>", scipen=100, digits=4)

library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
library(readxl)
library(ggeffects)
feelings=read_excel("festivaldata-full.xlsx", sheet="feelings")
feelings$Sex = factor(feelings$Sex)
feelings$SexOrient = factor(feelings$SexOrient)
feelings$Experiencen = as.numeric(feelings$Experiencen)

#PLOTS
safe.sexjit = ggplot(feelings, aes(Sexn, Safe))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male") +
  labs(title = "", x="", y="Feeling Safe") +
  theme_classic()

safe.expjit = ggplot(feelings, aes(Experiencen, Safe))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("1-2", "3-5", "6+") +
  labs(title = "", x="", y="") +
  theme_classic()

changesvibe.expjit = ggplot(feelings, aes(Experiencen, ChangesVibe))+
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black", span = 2)+
  xlim("1-2", "3-5", "6+") +
  labs(title = "", x="", y="") +
  theme_classic()

changesvibe.sexorientjit = ggplot(feelings, aes(SexOrientn, ChangesVibe))+
  geom_jitter(aes(color = SexOrientn), width = .3, height = .3, na.rm = TRUE, 
              size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin= 0, end = 0.8)+
  stat_smooth(method = "loess", color="black", span = 2)+
  xlim("Heterosexual", "LGBQA") +
  labs(title = "", x="", y="Changes Vibe") +
  theme_classic()

#MODELS
feelings$Experience = ordered(feelings$Experience)
feelings$Experiencen = ordered(feelings$Experiencen)
Safe.glm = glm(Safe ~ Sex + Experience + SexOrient, 
               family = binomial, data = feelings)
ChangesVibe.glm = glm(ChangesVibe ~ Sex + Experience + SexOrient, 
                      family = binomial, data = feelings)

safe.Sex = ggeffect(Safe.glm, terms = "Sex")
safe.sexmarg = plot(safe.Sex) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

safe.Experience = ggeffect(Safe.glm, terms = "Experience")
safe.expmarg = plot(safe.Experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  theme_classic()

changesvibe.experience= ggeffect(ChangesVibe.glm, terms = "Experience")
changesvibe.expmarg = plot(changesvibe.experience) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  xlim("1-2", "3-5", "6+") +
  theme_classic()

changesvibe.sexorient= ggeffect(ChangesVibe.glm, terms = "SexOrient")
changesvibe.sexorientmarg = plot(changesvibe.sexorient) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title = "", x="", y="Probability of Response") +
  xlim("Heterosexual", "LGBQA") +
  theme_classic()

tiff(filename = "Figure 4-feelings.tiff", width=12, height=6, 
     units = "in",res=300, compression = "lzw", pointsize=12, type="cairo")
plot_grid(safe.sexjit, safe.sexmarg, safe.expjit, safe.expmarg,
          changesvibe.sexorientjit, changesvibe.sexorientmarg, changesvibe.expjit, changesvibe.expmarg, 
                    labels = c(
                      'A', '', 'B', '',
                      'C', '', 'D', ''
                      ),
          ncol = 4, rel_widths = c(1, 1))
dev.off()


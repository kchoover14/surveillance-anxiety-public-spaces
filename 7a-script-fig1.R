options(prompt="R>", scipen=100, digits=4)

library(ordinal)
library(brms)
library(readxl)
library(ggplot2)
library(scales) 
library(cowplot)
gensafety=read_excel("festivaldata-full.xlsx", sheet="gensafety")
gensafety$Experiencen = as.numeric(gensafety$Experiencen)

#JITTERS
safety.expjit = ggplot(gensafety, aes(Experiencen, PerSafetyn)) +
  geom_jitter(aes(color = Experiencen), width = .3, height = .3, 
              na.rm = TRUE, size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin=0, end = 0.8)+
  stat_smooth(method = "loess", color="black") +
  xlim("1-2", "3-5", "6+") +
  ylim("None", "Minor", "Major")+
  labs(title = "", x="", y="General Safety Concerns") +
  theme_classic()

safety.sexjit = ggplot(gensafety, aes(Sexn, PerSafetyn))+
  geom_jitter(aes(color = Sexn), width = .3, height = .3, 
              na.rm = TRUE, size = 1, show.legend = FALSE)+
  scale_color_viridis_c(option = "A", begin=0, end = 0.8)+
  stat_smooth(method = "loess", color="black")+
  xlim("Female", "Male") +
  ylim("None", "Minor", "Major")+
  labs(title = "", x="", y="General Safety Concerns") +
  theme_classic()

#MARGINS
gensafety$PerSafetyn = ordered(gensafety$PerSafetyn)
gensafety$Experiencen = ordered(gensafety$Experiencen)
gensafety$Experience = ordered(gensafety$Experience)

safety.brm = brm(
  formula = PerSafetyn ~ Experience + Sex + SexOrient, 
  data = gensafety, 
  cores = getOption("mc.cores", 4),
  family = cumulative("logit")
)

safety.Experience=conditional_effects(safety.brm, "Experience", categorical = TRUE) 
safety.expmarg = plot(safety.Experience, plot=FALSE)[[1]] +
  scale_color_viridis_d(name  ="",
                        breaks=c("1", "2", "3"),
                        labels=c("None", "Minor", "Major")) +
  scale_fill_discrete(name  ="",
                      breaks=c("1", "2", "3"),
                      labels=c("None", "Minor", "Major")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(x="", y="Probability of Response") +
  theme_classic() +
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

safety.Sex = conditional_effects(safety.brm, "Sex", categorical = TRUE)
safety.sexnmarg = plot(safety.Sex, plot=FALSE)[[1]]  +
  scale_color_viridis_d(name  ="",
                        breaks=c("1", "2", "3"),
                        labels=c("None", "Minor", "Major")) +
  scale_fill_discrete(name  ="",
                      breaks=c("1", "2", "3"),
                      labels=c("None", "Minor", "Major")) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1),
                     breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(x="", y="Probability of Response") +
  theme_classic() +
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

library(cowplot)
tiff(filename = "Figure 1-gensafety.tiff", width=6, height= 6, 
     units = "in",res=300, compression = "lzw", pointsize=12, type="cairo")
plot_grid(safety.expjit, safety.expmarg,
          safety.sexjit, safety.sexnmarg,
          labels = c('A', '',
                     'B', ''),
          ncol =2, rel_widths = c(1, 1))
dev.off()


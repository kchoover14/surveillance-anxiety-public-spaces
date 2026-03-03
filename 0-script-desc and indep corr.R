options(prompt="R>", scipen=100, digits=4)

#Descriptives
library(readxl)
demog=read_excel("festivaldata-full.xlsx", sheet="demog")
demog$Age2 = factor(demog$Age2)
demog$Gender = factor(demog$Gender)
demog$Sex = factor(demog$Sex)
demog$SexOrient = factor(demog$SexOrient)
demog$Experience = factor(demog$Experience)
demog$Experiencen = as.numeric(demog$Experiencen)

library(broom)
summary(demog[2:6])

cor.test(demog$Age2n,demog$Experiencen, 
         method = "kendall", alternative = "greater",
         alpha=.05, conf.level = TRUE)
age.test=cbind(demog$Age2n, demog$Experiencen)
library(PerformanceAnalytics)
chart.Correlation(age.test,
                  method="kendall",
                  pch=12)




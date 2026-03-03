options(prompt="R>", scipen=100, digits=4)

library(readxl)
long=read_excel("festivaldata-full.xlsx", sheet="long")

library(broom)

sink(file = "6-results-prop tables-glm data.txt", append=FALSE)
safety = table(long$SafetyCat, long$SafetyData) #rows, columns
safety
prop.table(safety, 1) # row percentages

safer = table(long$SaferCat, long$SaferData) #rows, columns
safer
prop.table(safer, 1) # row percentages

feelings = table(long$FeelingsCat, long$FeelingsData) #rows, columns
feelings
prop.table(feelings, 1) # row percentages

sink()

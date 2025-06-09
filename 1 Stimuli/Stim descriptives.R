##Read in data
high = read.csv("high_typicality.csv") #ex 1
ex2 = read.csv("Ex2 Stim final.csv")


##load libraries
library(ez)
library(psychReport)

#turn off scientific notation
options(scipen = 999)

####EX 1####
##mean Typicality
tapply(high$Prototypic.Mean, list(high$EthnicitySelf, high$GenderSelf), mean)

##ANOVA
#typicality
model1 = ezANOVA(high,
                 wid = Model,
                 dv = Prototypic.Mean,
                 between = .(EthnicitySelf, GenderSelf),
                 type = 3,
                 detailed = T)
model1

tapply(high$Prototypic.Mean, high$EthnicitySelf, mean)

#attractiveness
model2 = ezANOVA(high,
                 wid = Model,
                 dv = Attractive.Mean,
                 between = .(EthnicitySelf, GenderSelf),
                 type = 3,
                 detailed = T)
model2

##Get values for table A1
tapply(high$Attractive.Mean, list(high$GenderSelf, high$EthnicitySelf), mean)
tapply(high$AgeRated, list(high$GenderSelf, high$EthnicitySelf), mean)
tapply(high$Prototypic.Mean, list(high$GenderSelf, high$EthnicitySelf), mean)

tapply(high$Attractive.Mean, list(high$GenderSelf, high$EthnicitySelf), sd)
tapply(high$AgeRated, list(high$GenderSelf, high$EthnicitySelf), sd)
tapply(high$Prototypic.Mean, list(high$GenderSelf, high$EthnicitySelf), sd)

tapply(high$Attractive.Mean, list(high$GenderSelf, high$EthnicitySelf), min)
tapply(high$AgeRated, list(high$GenderSelf, high$EthnicitySelf), min)
tapply(high$Prototypic.Mean, list(high$GenderSelf, high$EthnicitySelf), min)

tapply(high$Attractive.Mean, list(high$GenderSelf, high$EthnicitySelf), max)
tapply(high$AgeRated, list(high$GenderSelf, high$EthnicitySelf), max)
tapply(high$Prototypic.Mean, list(high$GenderSelf, high$EthnicitySelf), max)

####EX 2####
##mean typicality
#need subset by high and low
ex2_h = subset(ex2,
                ex2$Typicality == "H")
ex2_l = subset(ex2,
               ex2$Typicality == "L")

#typicality
tapply(ex2_h$Prototypic_Mean, list(ex2_h$Gender, ex2_h$Ethnicity), mean)
tapply(ex2_l$Prototypic_Mean, list(ex2_l$Gender, ex2_l$Ethnicity), mean)

tapply(ex2_h$Prototypic_Mean, list(ex2_h$Gender, ex2_h$Ethnicity), sd)
tapply(ex2_l$Prototypic_Mean, list(ex2_l$Gender, ex2_l$Ethnicity), sd)

tapply(ex2_h$Prototypic_Mean, list(ex2_h$Gender, ex2_h$Ethnicity), min)
tapply(ex2_l$Prototypic_Mean, list(ex2_l$Gender, ex2_l$Ethnicity), min)

tapply(ex2_h$Prototypic_Mean, list(ex2_h$Gender, ex2_h$Ethnicity), max)
tapply(ex2_l$Prototypic_Mean, list(ex2_l$Gender, ex2_l$Ethnicity), max)

#age
tapply(ex2_h$Age, list(ex2_h$Gender, ex2_h$Ethnicity), mean)
tapply(ex2_l$Age, list(ex2_l$Gender, ex2_l$Ethnicity), mean)

tapply(ex2_h$Age, list(ex2_h$Gender, ex2_h$Ethnicity), sd)
tapply(ex2_l$Age, list(ex2_l$Gender, ex2_l$Ethnicity), sd)

tapply(ex2_h$Age, list(ex2_h$Gender, ex2_h$Ethnicity), min)
tapply(ex2_l$Age, list(ex2_l$Gender, ex2_l$Ethnicity), min)

tapply(ex2_h$Age, list(ex2_h$Gender, ex2_h$Ethnicity), max)
tapply(ex2_l$Age, list(ex2_l$Gender, ex2_l$Ethnicity), max)

##attractiveness
tapply(ex2_h$Attractiveness, list(ex2_h$Gender, ex2_h$Ethnicity), mean)
tapply(ex2_l$Attractiveness, list(ex2_l$Gender, ex2_l$Ethnicity), mean)

tapply(ex2_h$Attractiveness, list(ex2_h$Gender, ex2_h$Ethnicity), sd)
tapply(ex2_l$Attractiveness, list(ex2_l$Gender, ex2_l$Ethnicity), sd)

tapply(ex2_h$Attractiveness, list(ex2_h$Gender, ex2_h$Ethnicity), min)
tapply(ex2_l$Attractiveness, list(ex2_l$Gender, ex2_l$Ethnicity), min)

tapply(ex2_h$Attractiveness, list(ex2_h$Gender, ex2_h$Ethnicity), max)
tapply(ex2_l$Attractiveness, list(ex2_l$Gender, ex2_l$Ethnicity), max)

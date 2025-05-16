####Set up####
##read in the data
dat = read.csv("Data/recog.csv")

##load libraries
library(ez)
library(reshape)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

##drop all participants except black/white individuals
table(dat$participant_ethnicity) / 80

dat2 = subset(dat,
              dat$participant_ethnicity == "White" | dat$participant_ethnicity == "Black" )

table(dat2$participant_ethnicity) / 80 

hits = subset(dat2, dat2$key == "OLD")
fa = subset(dat2, dat2$key == "NEW")

####Start w/ hits####
#get means
tapply(hits$scored, list(hits$participant_ethnicity, hits$Target_Ethnicity), mean) #okay, small CRE pattern!

black.wide = cast(subset(hits,
                         hits$participant_ethnicity == "Black"),
                  Username ~ Target_Ethnicity, mean)

white.wide = cast(subset(hits,
                         hits$participant_ethnicity == "White"),
                  Username ~ Target_Ethnicity, mean)

##Get CIs
(apply(black.wide, 2, sd) / sqrt(59)) * 1.96
(apply(white.wide, 2, sd) / sqrt(60)) * 1.96

##ANOVA
model1 = ezANOVA(hits,
                 between = participant_ethnicity,
                 within = Target_Ethnicity,
                 wid = Username,
                 dv = scored,
                 type = 3,
                 detailed = T)

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

##post hocs
tapply(hits$scored, hits$participant_ethnicity, mean)

##Black participants
temp = t.test(black.wide$B, black.wide$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

#White participants
temp = t.test(white.wide$B, white.wide$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

####Now do False Alarms####
tapply(fa$scored, list(fa$participant_ethnicity, fa$Target_Ethnicity), mean)

black.wide2 = cast(subset(fa,
                         fa$participant_ethnicity == "Black"),
                  Username ~ Target_Ethnicity, mean)

white.wide2 = cast(subset(fa,
                         fa$participant_ethnicity == "White"),
                  Username ~ Target_Ethnicity, mean)

##Get CIs
(apply(black.wide2, 2, sd) / sqrt(59)) * 1.96
(apply(white.wide2, 2, sd) / sqrt(60)) * 1.96

##And run the ANOVA
model2 = ezANOVA(fa,
                 between = participant_ethnicity,
                 within = Target_Ethnicity,
                 wid = Username,
                 dv = scored,
                 type = 3,
                 detailed = T)

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

##main effect of target ethnicity
tapply(fa$scored, fa$Target_Ethnicity, mean)

##post hocs
##Black participants
temp = t.test(black.wide2$B, black.wide2$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

#White participants
temp = t.test(white.wide2$B, white.wide2$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

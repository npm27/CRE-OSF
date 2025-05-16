####Set up####
##read in data
black = read.csv("Indices/black_indices.csv") #split by target ethnicity
white = read.csv("Indices/white_indices.csv")

##Load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

####get means####
tapply(black$dprime, black$e, mean) #black participants better dprime for black targets
tapply(white$dprime, white$e, mean) #white participants higher dprime for white targets

##get sds/CIs
tapply(black$dprime, black$e, sd)
tapply(white$dprime, white$e, sd)

(tapply(black$dprime, black$e, sd)[1] / sqrt(59)) * 1.96 
(tapply(black$dprime, black$e, sd)[2] / sqrt(60)) * 1.96

(tapply(white$dprime, white$e, sd)[1] / sqrt(59)) * 1.96 
(tapply(white$dprime, white$e, sd)[2] / sqrt(60)) * 1.96

####Run the ANOVA####
black$target = rep("Black")
white$target = rep("White")

sig.analyses = rbind(black, white)

model = ezANOVA(sig.analyses,
                 wid = i,
                 dv = dprime,
                 between = e,
                 within = target,
                 type = 3,
                 detailed = T)
model

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

aovEffectSize(model, effectSize = "pes")

##main effect of target ethnicitiy
tapply(sig.analyses$dprime, sig.analyses$target, mean)

####Post-hocs####
sig.analyses2 = sig.analyses[ , c(1, 2, 12, 7)]

sig.black = cast(subset(sig.analyses2,
                        sig.analyses2$e == "Black"),
                 i ~ target, mean)

sig.white = cast(subset(sig.analyses2,
                         sig.analyses2$e == "White"),
                  i ~ target, mean)

##Black participants
temp = t.test(sig.black$Black, sig.black$White, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant

##white participants
temp = t.test(sig.white$Black, sig.white$White, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

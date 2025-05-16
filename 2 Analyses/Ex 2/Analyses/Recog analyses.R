####Set up####
##read in data
dat = read.csv("Data/recog_data.csv")

##Load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

##rename columns
colnames(dat)[4] = "typicality"

####Check if everything lines up w/ the JOL dataset####
##Start with just hit rates
hits = subset(dat,
              dat$key == "OLD")

hits = subset(hits,
              hits$participant_ethnicity == "White" | hits$participant_ethnicity == "Black")

##ns
table(hits$participant_ethnicity) /40 #okay, 78 black participants; 79 white participants

##get means for hits
hits.b = subset(hits,
                hits$participant_ethnicity == "Black")
hits.w = subset(hits,
                hits$participant_ethnicity == "White")

tapply(hits.b$scored, list(hits.b$Target_Ethnicity, hits.b$typicality), mean)
tapply(hits.w$scored, list(hits.w$Target_Ethnicity, hits.w$typicality), mean)

##Get the data in the right shape
#start w/ black participants
hits.b.h = cast(subset(hits.b,
                       hits.b$typicality == "H"),
                Username ~ Target_Ethnicity, mean)
hits.b.l = cast(subset(hits.b,
                       hits.b$typicality == "L"),
                Username ~ Target_Ethnicity, mean)

#now white participants
hits.w.h = cast(subset(hits.w,
                       hits.w$typicality == "H"),
                Username ~ Target_Ethnicity, mean)
hits.w.l = cast(subset(hits.w,
                       hits.w$typicality == "L"),
                Username ~ Target_Ethnicity, mean)

##get sd/CIs for hits
#black
(apply(hits.b.h, 2, sd) / sqrt(nrow(hits.b.h))) * 1.96
(apply(hits.b.l, 2, sd) / sqrt(nrow(hits.b.l))) * 1.96

#white
(apply(hits.w.h, 2, sd) / sqrt(nrow(hits.w.h))) * 1.96
(apply(hits.w.l, 2, sd) / sqrt(nrow(hits.w.l))) * 1.96

##looks good! Now for false alarms!
FA = subset(dat,
            dat$key == "NEW")

FA = subset(FA,
            FA$participant_ethnicity == "White" | FA$participant_ethnicity == "Black")

##get FA means
FA.b = subset(FA,
                FA$participant_ethnicity == "Black")
FA.w = subset(FA,
                FA$participant_ethnicity == "White")

tapply(FA.b$scored, list(FA.b$Target_Ethnicity, FA.b$typicality), mean)
tapply(FA.w$scored, list(FA.w$Target_Ethnicity, FA.w$typicality), mean)

##Get the data in the right shape
#start w/ black participants
FA.b.h = cast(subset(FA.b,
                       FA.b$typicality == "H"),
                Username ~ Target_Ethnicity, mean)
FA.b.l = cast(subset(FA.b,
                       FA.b$typicality == "L"),
                Username ~ Target_Ethnicity, mean)

#now white participants
FA.w.h = cast(subset(FA.w,
                       FA.w$typicality == "H"),
                Username ~ Target_Ethnicity, mean)
FA.w.l = cast(subset(FA.w,
                       FA.w$typicality == "L"),
                Username ~ Target_Ethnicity, mean)

##get sd/CIs for FA
#black
(apply(FA.b.h, 2, sd) / sqrt(nrow(FA.b.h))) * 1.96
(apply(FA.b.l, 2, sd) / sqrt(nrow(FA.b.l))) * 1.96

#white
(apply(FA.w.h, 2, sd) / sqrt(nrow(FA.w.h))) * 1.96
(apply(FA.w.l, 2, sd) / sqrt(nrow(FA.w.l))) * 1.96

####And run the anovas here####
####Start w/ hits####
##Maybe a pair of 2 x 2 ANOVAs? #not sure if I care about the three-way interaction?
high = ezANOVA(subset(hits,
                      hits$typicality == "H"),
               between = participant_ethnicity,
               within = Target_Ethnicity,
               dv = scored,
               wid = Username,
               detailed = T)
high$ANOVA$MSE = high$ANOVA$SSd/high$ANOVA$DFd
high$ANOVA$MSE

aovEffectSize(high, effectSize = "pes")

low = ezANOVA(subset(hits,
                      hits$typicality == "L"),
               between = participant_ethnicity,
               within = Target_Ethnicity,
               dv = scored,
               wid = Username,
               detailed = T)
low$ANOVA$MSE = low$ANOVA$SSd/low$ANOVA$DFd
low$ANOVA$MSE

aovEffectSize(low, effectSize = "pes")

high2 = subset(hits,
               hits$typicality == 'H')
low2 = subset(hits,
              hits$typicality == "L")

tapply(high2$scored, list(high2$Target_Ethnicity, high2$participant_ethnicity), mean)

tapply(low2$scored, low2$Target_Ethnicity, mean)
tapply(low2$scored, list(low2$Target_Ethnicity, low2$participant_ethnicity), mean)

#get the correct comparisons
hits.b2.h = cast(subset(hits,
                      hits$participant_ethnicity == "Black" & hits$typicality == "H"),
               Username ~ Target_Ethnicity, mean)
hits.w2.h = cast(subset(hits,
                      hits$participant_ethnicity == "White" & hits$typicality == "H"),
               Username ~ Target_Ethnicity, mean)

hits.b2.l = cast(subset(hits,
                        hits$participant_ethnicity == "Black" & hits$typicality == "L"),
                 Username ~ Target_Ethnicity, mean)
hits.w2.l = cast(subset(hits,
                        hits$participant_ethnicity == "White" & hits$typicality == "L"),
                 Username ~ Target_Ethnicity, mean)

####Run the post-hocs####
#black high targets
temp = t.test(hits.b2.h$B, hits.b2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #marginal (this may be a discussion point)

##pbic
pbic1 = hits.b2.h[ , c(1,2)]
pbic1$target = rep("B")

colnames(pbic1)[2] = "score"

pbic2 = hits.b2.h[ , c(1,3)]
pbic2$target = rep("W")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

pbic.model = ezANOVA(pbic3,
                     wid = Username,
                     dv = score,
                     within = target,
                     type = 3,
                     detailed = T)

pbic.model

#white high targets
temp = t.test(hits.w2.h$B, hits.w2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant

sd(hits.w2.h$B)
sd(hits.w2.h$W)

#black low targets
temp = t.test(hits.b2.l$B, hits.b2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant

sd(hits.b2.l$B)
sd(hits.b2.l$W)

#white low targets
temp = t.test(hits.w2.l$B, hits.w2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-significant

##pbic
pbic1 = hits.w2.l[ , c(1,2)]
pbic1$target = rep("B")

colnames(pbic1)[2] = "score"

pbic2 = hits.w2.l[ , c(1,3)]
pbic2$target = rep("W")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

pbic.model = ezANOVA(pbic3,
                     wid = Username,
                     dv = score,
                     within = target,
                     type = 3,
                     detailed = T)
pbic.model

###Main take-aways so far -- typicality is strongly related to recall; high typicality targets generally recalled more than low typicality targets 

####False Alarm ANOVAs####
high.fa = ezANOVA(subset(FA,
                         FA$typicality == "H"),
                 between = participant_ethnicity,
                 within = Target_Ethnicity,
                 dv = scored,
                 wid = Username,
                 type = 3,
                 detailed = T)

high.fa$ANOVA$MSE = high.fa$ANOVA$SSd/high.fa$ANOVA$DFd
aovEffectSize(high.fa, effectSize = "pes")

low.fa = ezANOVA(subset(FA,
                         FA$typicality == "L"),
                  between = participant_ethnicity,
                  within = Target_Ethnicity,
                  dv = scored,
                  wid = Username,
                  type = 3,
                  detailed = T)

low.fa$ANOVA$MSE = low.fa$ANOVA$SSd/low.fa$ANOVA$DFd
aovEffectSize(low.fa, effectSize = "pes")

##Main effects
FA.high = subset(FA,
                 FA$typicality == "H")
FA.low = subset(FA,
                FA$typicality == "L")

tapply(FA.high$scored, FA.high$Target_Ethnicity, mean)
tapply(FA.high$scored, list (FA.high$Target_Ethnicity, FA.high$participant_ethnicity), mean)

tapply(FA.low$scored, FA.low$Target_Ethnicity, mean)

##Interactions
#get the correct comparisons
FA.b2.h = cast(subset(FA,
                        FA$participant_ethnicity == "Black" & FA$typicality == "H"),
                 Username ~ Target_Ethnicity, mean)
FA.w2.h = cast(subset(FA,
                        FA$participant_ethnicity == "White" & FA$typicality == "H"),
                 Username ~ Target_Ethnicity, mean)

FA.b2.l = cast(subset(FA,
                        FA$participant_ethnicity == "Black" & FA$typicality == "L"),
                 Username ~ Target_Ethnicity, mean)
FA.w2.l = cast(subset(FA,
                        FA$participant_ethnicity == "White" & FA$typicality == "L"),
                 Username ~ Target_Ethnicity, mean)

####Run the post-hocs####
#black high targets
temp = t.test(FA.b2.h$B, FA.b2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

#pbic
pbic1 = FA.b2.h[ , c(1,2)]
pbic1$target = rep("B")

colnames(pbic1)[2] = "score"

pbic2 = FA.b2.h[ , c(1,3)]
pbic2$target = rep("W")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

pbic.model = ezANOVA(pbic3,
                     wid = Username,
                     dv = score,
                     within = target,
                     type = 3,
                     detailed = T)

pbic.model

##White high targets
temp = t.test(FA.w2.h$B, FA.w2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

sd(FA.w2.h$B)
sd(FA.w2.h$W)

##Black low targets
temp = t.test(FA.b2.l$B, FA.w2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##White low targets
temp = t.test(FA.w2.l$B, FA.w2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

####signal detection here####
hb = read.csv("Indices/high_black_indices.csv")
lb = read.csv("Indices/low_black_indices.csv")
hw = read.csv("Indices/high_white_indices.csv")
lw = read.csv("Indices/low_white_indices.csv")

####get mean d prime####
tapply(hb$dprime, hb$e, mean) #correct pattern; higher discriminability for same race targets
tapply(lb$dprime, lb$e, mean) #same pattern, smaller effect for low typicality

tapply(hw$dprime, hw$e, mean) #correct pattern
tapply(lw$dprime, lw$e, mean) #correct pattern

##and get CIs
(tapply(hb$dprime, hb$e, sd)[1] / sqrt(78)) * 1.96 
(tapply(hb$dprime, hb$e, sd)[2] / sqrt(79)) * 1.96 

(tapply(lb$dprime, lb$e, sd)[1] / sqrt(78)) * 1.96 
(tapply(lb$dprime, lb$e, sd)[2] / sqrt(79)) * 1.96 

(tapply(hw$dprime, hw$e, sd)[1] / sqrt(78)) * 1.96
(tapply(hw$dprime, hw$e, sd)[2] / sqrt(79)) * 1.96 

(tapply(lw$dprime, lw$e, sd)[1] / sqrt(78)) * 1.96
(tapply(lw$dprime, lw$e, sd)[2] / sqrt(79)) * 1.96 

####And run the ANOVA####
hb$typ = rep("H")
hw$typ = rep("H")
lb$typ = rep("L")
lw$typ = rep("L")

hb$t = rep("B")
hw$t = rep("W")
lb$t = rep("B")
lw$t = rep("W")

sig.analyses = rbind(hb, hw, lb, lw)

d.high = ezANOVA(subset(sig.analyses,
                        sig.analyses$typ == "H"),
                 wid = i,
                 dv = dprime,
                 between = e,
                 within = t,
                 type = 3,
                 detailed = T)

d.high$ANOVA$MSE = d.high$ANOVA$SSd/d.high$ANOVA$DFd
aovEffectSize(d.high, effectSize = "pes")

d.low = ezANOVA(subset(sig.analyses,
                        sig.analyses$typ == "L"),
                 wid = i,
                 dv = dprime,
                 between = e,
                 within = t,
                 type = 3,
                 detailed = T)

d.low$ANOVA$MSE = d.low$ANOVA$SSd/d.low$ANOVA$DFd
aovEffectSize(d.low, effectSize = "pes")

sig.high = subset(sig.analyses, sig.analyses$typ == "H")
sig.low = subset(sig.analyses, sig.analyses$typ == "L")

tapply(sig.high$dprime, list(sig.high$t, sig.high$e), mean)

tapply(sig.low$dprime, list(sig.low$t, sig.low$e), mean)

##Interactions
#get the correct comparisons
sig.b2.h = cast(subset(sig.analyses[ , c(1:6, 8:13, 7)],
                      sig.analyses$e == "Black" & sig.analyses$typ == "H"),
               i ~ t, mean)
sig.w2.h = cast(subset(sig.analyses[ , c(1:6, 8:13, 7)],
                      sig.analyses$e == "White" & sig.analyses$typ == "H"),
               i ~ t, mean)

sig.b2.l = cast(subset(sig.analyses[ , c(1:6, 8:13, 7)],
                      sig.analyses$e == "Black" & sig.analyses$typ == "L"),
               i ~ t, mean)

sig.w2.l = cast(subset(sig.analyses[ , c(1:6, 8:13, 7)],
                      sig.analyses$e == "White" & sig.analyses$typ == "L"),
               i ~ t, mean)

####Post hocs####
#black high targets
temp = t.test(sig.b2.h$B, sig.b2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #marginal

pbic1 = sig.b2.h[ , c(1,2)]
pbic1$target = rep("B")

colnames(pbic1)[2] = "score"

pbic2 = sig.b2.h[ , c(1,3)]
pbic2$target = rep("W")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

pbic.model = ezANOVA(pbic3,
                     wid = i,
                     dv = score,
                     within = target,
                     type = 3,
                     detailed = T)
pbic.model

#white high targets
temp = t.test(sig.w2.h$B, sig.w2.h$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

sd(sig.w2.h$B)
sd(sig.w2.h$W)

#black low targets
temp = t.test(sig.b2.l$B, sig.b2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

sd(sig.b2.l$B)
sd(sig.b2.l$W)

#white low targets
temp = t.test(sig.w2.l$B, sig.w2.l$W, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

pbic1 = sig.w2.l[ , c(1,2)]
pbic1$target = rep("B")

colnames(pbic1)[2] = "score"

pbic2 = sig.w2.l[ , c(1,3)]
pbic2$target = rep("W")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

pbic.model = ezANOVA(pbic3,
                     wid = i,
                     dv = score,
                     within = target,
                     type = 3,
                     detailed = T)
pbic.model

####Set up####
##read in data
black = read.csv("Data/black_qs_final.csv")
white = read.csv("Data/white_qs_final.csv")

##Load libraries
library(psych)
library(ez)

##turn off scientific notation
options(scipen = 999)

####Differences in mean scores?
mean(black$ATW); mean(white$ATB)
(sd(black$ATW) / sqrt(length(black$ATW))) *1.96; (sd(white$ATB) / sqrt(length(white$ATB))) *1.96
min(black$ATW); min(white$ATB)
max(black$ATW); max(white$ATB)

mean(black$contact, na.rm = T); mean(white$contact, na.rm = T)
(sd(black$contact, na.rm = T) / sqrt(length(na.omit(black[ , 11])))) *1.96; (sd(white$contact, na.rm = T) / sqrt(length(na.omit(white[ , 11])))) *1.96
min(black$contact, na.rm = T); min(white$contact, na.rm = T)
max(black$contact, na.rm = T); max(white$contact, na.rm = T)

mean(black$EMS); mean(white$EMS) #right direction
(sd(black$EMS) / sqrt(length(black$EMS))) * 1.96; (sd(white$EMS) / sqrt(length(white$EMS))) * 1.96
min(black$EMS); min(white$EMS)
max(black$EMS); max(white$EMS)

mean(black$IMS); mean(white$IMS)
(sd(black$IMS) / sqrt(length(black$IMS))) * 1.96; (sd(white$IMS) / sqrt(length(white$IMS))) * 1.96
min(black$IMS); min(white$IMS)
max(black$IMS); max(white$IMS)

##run a few t.tests
#atw/atb
temp = t.test(black$ATW, white$ATB, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #super sig!

#ems
temp = t.test(black$EMS, white$EMS, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non sig but right direction

pbic1 = black[ , c(1, 12)]
pbic1$group = rep("black")

pbic2 = white[ , c(1, 12)]
pbic2$group = rep("white")

pbic3 = rbind(pbic1, pbic2)

model.pbic = ezANOVA(pbic3,
        dv = EMS,
        wid = Username,
        between = group,
        type = 3,
        detailed = T)
model.pbic

#ims
temp = t.test(black$IMS, white$IMS, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non sig but right direction

#contact
temp = t.test(black$contact, white$contact, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #super sig!

####Correlations b/w JOLs and measures?
##start w/ black participants
##correlations between JOLs for cross race and Attitude measures
cor.test(black$JOL_W_H, black$ATW) #ns
cor.test(black$JOL_W_L, black$ATW) #ns

cor.test(black$JOL_W_H, black$contact) #sig #neg (as contact goes up, JOLs go down?)
cor.test(black$JOL_W_L, black$contact) #sig, same pattern #could indicate that greater contact = greater ability to process useful features? Need a measure rating the quality of the contact! 

cor.test(black$JOL_W_H, black$EMS) #ns
cor.test(black$JOL_W_L, black$EMS) #ns

cor.test(black$JOL_W_H, black$IMS) #ns
cor.test(black$JOL_W_L, black$IMS) #ns

##and white participants
cor.test(white$JOL_B_H, white$ATB) #ns
cor.test(white$JOL_B_L, white$ATB) #ns

cor.test(white$JOL_B_H, white$contact) #ns
cor.test(white$JOL_B_L, white$contact) #ns

cor.test(white$JOL_B_H, white$EMS) #ns
cor.test(white$JOL_B_L, white$EMS) #ns

cor.test(white$JOL_B_H, white$IMS) #ns
cor.test(white$JOL_B_L, white$IMS) #marginal

##What about correlations w/ recog?
cor.test(white$recog_B_H, white$ATB) #sig!
cor.test(white$recog_B_L, white$ATB) #sig! (as ATB scores go up, recog goes down!)

cor.test(black$recog_B_H, black$ATW) #nope
cor.test(black$recog_B_L, black$ATW) #nope #pattern doesn't extend to black participants

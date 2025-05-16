####Set up####
##read in data
white = read.csv("Data/white_qs_final.csv")
black = read.csv("Data/black_qs_final.csv")

##turn off scientific notation
options(scipen = 999)

####Differences in mean scores?
##ATB/ATW
mean(black$ATW); mean(white$ATB)
(sd(black$ATW) / sqrt(length(black$ATW))) *1.96; (sd(white$ATB) / sqrt(length(white$ATB))) *1.96
min(black$ATW); min(white$ATB)
max(black$ATW); max(white$ATB)

temp = t.test(black$ATW, white$ATB, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #super sig!

##Contact
mean(black$contact, na.rm = T); mean(white$contact, na.rm = T)
(sd(black$contact, na.rm = T) / sqrt(length(na.omit(black[ , 5])))) *1.96; (sd(white$contact, na.rm = T) / sqrt(length(na.omit(white[ , 5])))) *1.96
min(black$contact, na.rm = T); min(white$contact, na.rm = T)
max(black$contact, na.rm = T); max(white$contact, na.rm = T)

temp = t.test(black$contact, white$contact, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#compute pbic
pbic1 = black[ , c(1, 5)]
pbic1$eth = rep("b")

pbic2 = white[ , c(1, 5)]
pbic2$eth = rep("w")

pbic3 = rbind(pbic1, pbic2)
pbic3 = na.omit(pbic3)

model.pbic = ezANOVA(pbic3,
                     dv = contact,
                     between = eth,
                     wid = Username,
                     type = 3,
                     detailed = T)
model.pbic

##EMS
mean(black$EMS); mean(white$EMS) #right direction
(sd(black$EMS) / sqrt(length(black$EMS))) * 1.96; (sd(white$EMS) / sqrt(length(white$EMS))) * 1.96
min(black$EMS); min(white$EMS)
max(black$EMS); max(white$EMS)

temp = t.test(black$EMS, white$EMS, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non sig but right direction

##IMS
mean(black$IMS); mean(white$IMS)
(sd(black$IMS) / sqrt(length(black$IMS))) * 1.96; (sd(white$IMS) / sqrt(length(white$IMS))) * 1.96
min(black$IMS); min(white$IMS)
max(black$IMS); max(white$IMS)

temp = t.test(black$IMS, white$IMS, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

####Correlations####
##start w/ black participants
cor.test(black$JOL_W, black$ATW) #ns

cor.test(black$JOL_W, black$contact) #nS

cor.test(black$JOL_W, black$EMS) #ns

cor.test(black$JOL_W, black$IMS) #ns

##and white participants
cor.test(white$B, white$ATB) #ns

cor.test(white$B, white$contact) #ns

cor.test(white$B, white$EMS) #ns

cor.test(white$B, white$IMS) #ns

####Set up####
##read in the data
dat = read.csv("Data/JOLs.csv")

##load libraries
library(ez)
library(reshape)
library(psychReport)

##turn off scientific notation
options(scipen = 999)

##drop all participants except black/white individuals
table(dat$participant.ethnicity) / 40

dat2 = subset(dat,
              dat$participant.ethnicity == "White" | dat$participant.ethnicity == "Black" )

##get recog on same scale as JOLs
dat2$scored = dat2$scored * 100

##get data in long form
#fix column names
colnames(dat2)[4] = "JOL"
colnames(dat2)[7] = "recog"

#remove JOLs > 100
dat2$JOL[dat2$JOL > 100] = NA

#wide -> long
dat2.long = melt(dat2[ , -3],
                 id = c("Username", "participant.ethnicity", "Target_Ethnicity", "Target_Gender"))

#and fix column names one more time
colnames(dat2.long)[5:6] = c("Task", "Score")

dat2.rt = dat2[ , c(1, 2, 5, 3)]

##White participants
white = subset(dat2.long,
               dat2.long$participant.ethnicity == "White")
black = subset(dat2.long,
               dat2.long$participant.ethnicity == "Black")

##and check for outliers
#any participants who are just making JOLs of all 100?
##white
white.wide = cast(subset(white,
                         white$Task == "JOL"),
                  Username ~ Target_Ethnicity, mean, na.rm = T)
white.wide$diff = (white.wide$B - white.wide$W)

##black
black.wide = cast(subset(black,
                        black$Task == "JOL"),
                  Username ~ Target_Ethnicity, mean, na.rm = T)
black.wide$diff = (black.wide$W - black.wide$B)

##Looks good!

##Recombine for ANOVAs
#2(participant ethnicity) x 2(target race) x 2(task type)
#or do separate anovas for each racial group? (kinda leaning towards this for parsimony...)
combined = rbind(white, black)

table(combined$participant.ethnicity) / 80 #60 white participants, 59 black participants

#general patterns
tapply(black$Score, list(black$Task, black$Target_Ethnicity), mean, na.rm = T)
tapply(white$Score, list(white$Task, white$Target_Ethnicity), mean, na.rm = T)

(apply(black.wide[, 2:3], 2, sd, na.rm = T) / sqrt(59)) * 1.96
(apply(white.wide[, 2:3], 2, sd, na.rm = T) / sqrt(60)) * 1.96

combined2 = na.omit(combined)

#full model
model1 = ezANOVA(combined2,
                 dv = Score,
                 between = participant.ethnicity,
                 within = .(Target_Ethnicity, Task),
                 wid = Username,
                 type = 3,
                 detailed = T)

model1

##JOLs
com.JOLs = subset(combined2,
                  combined2$Task == "JOL")
com.recog = subset(combined2,
                   combined2$Task == "recog")

model2 = ezANOVA(com.JOLs,
                 dv = Score,
                 between = participant.ethnicity,
                 within = Target_Ethnicity,
                 wid = Username,
                 type = 3,
                 detailed = T)

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

##main effect of target ethnicity
tapply(com.JOLs$Score, com.JOLs$Target_Ethnicity, mean)

####Post Hocs####
##recaculate participant level to remove missing
##white
white.wide = cast(subset(white,
                         white$Task == "JOL"),
                  Username ~ Target_Ethnicity, mean, na.rm = T)

##black
black.wide = cast(subset(black,
                         black$Task == "JOL"),
                  Username ~ Target_Ethnicity, mean, na.rm = T)

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
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Nope!

#pbic for white participants
pbic1 = white.wide[ , c(1:2)]
pbic1$target = rep("B")
colnames(pbic1)[2] = "score"

pbic2 = white.wide[ , c(1, 3)]
pbic2$target = rep("W")
colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

model.pbic = ezANOVA(pbic3,
                     dv = score,
                     within = target,
                     wid = Username,
                     type = 3,
                     detailed = T)
model.pbic

##So... The CRE is reflected in participants JOLs, but only for black participants. White participants JOLs are right on top of each other

####Compute Gammas####
library(vcdExtra)
library(Hmisc)

##get data in the correct format
colnames(dat)[4] = "JOL"
colnames(dat)[7] = "Recog"

dat$JOL[dat$JOL > 100] = NA
dat = na.omit(dat)

white.gamma.b = subset(dat,
                     dat$participant.ethnicity == "White" &
                     dat$Target_Ethnicity == "B")
white.gamma.w = subset(dat,
                       dat$participant.ethnicity == "White" &
                         dat$Target_Ethnicity == "W")

black.gamma.b = subset(dat,
                       dat$participant.ethnicity == "Black" &
                         dat$Target_Ethnicity == "B")
black.gamma.w = subset(dat,
                       dat$participant.ethnicity == "Black" &
                         dat$Target_Ethnicity == "W")

##Start w/ black participants
#black targets
empty = data.frame()

for (i in unique(black.gamma.b$Username)){
  
  temp = subset(black.gamma.b, black.gamma.b$Username == i)
  
  g = rcorr.cens(temp$Recog, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

black_gamma.b.2 = empty

#white targets

empty = data.frame()

for (i in unique(black.gamma.w$Username)){
  
  temp = subset(black.gamma.w, black.gamma.w$Username == i)
  
  g = rcorr.cens(temp$Recog, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

black_gamma.w.2 = empty

##Now white participants
#black targets
empty = data.frame()

for (i in unique(white.gamma.b$Username)){
  
  temp = subset(white.gamma.b, white.gamma.b$Username == i)
  
  g = rcorr.cens(temp$Recog, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

white_gamma.b.2 = empty

#white targets
empty = data.frame()

for (i in unique(white.gamma.w$Username)){
  
  temp = subset(white.gamma.w, white.gamma.w$Username == i)
  
  g = rcorr.cens(temp$Recog, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

white_gamma.w.2 = empty

####Get means####
#black participants
mean(black_gamma.b.2$g, na.rm = T) #.16
mean(black_gamma.w.2$g, na.rm = T) #.19

(sd(black_gamma.b.2$g, na.rm = T) / sqrt(nrow(na.omit(black_gamma.b.2)))) * 1.96
(sd(black_gamma.w.2$g, na.rm = T) / sqrt(nrow(na.omit(black_gamma.w.2)))) * 1.96

#white participants
mean(white_gamma.b.2$g, na.rm = T) #.26
mean(white_gamma.w.2$g, na.rm = T) #.23 #slight inverse pattern; probably ns

(sd(white_gamma.b.2$g, na.rm = T) / sqrt(nrow(na.omit(white_gamma.b.2)))) * 1.96
(sd(white_gamma.w.2$g, na.rm = T) / sqrt(nrow(na.omit(white_gamma.w.2)))) * 1.96

####Run the ANOVA####
black_gamma.b.2$participant = rep("Black")
black_gamma.b.2$target = rep("Black")

black_gamma.w.2$participant = rep("Black")
black_gamma.w.2$target = rep("White")

white_gamma.w.2$participant = rep("White")
white_gamma.w.2$target = rep("White")

white_gamma.b.2$participant = rep("White")
white_gamma.b.2$target = rep("Black")

gammas = rbind(black_gamma.b.2, black_gamma.w.2,
               white_gamma.b.2, white_gamma.w.2)

gammas$g[is.nan(gammas$g)] <- 0

tapply(gammas$g, list(gammas$participant, gammas$target), mean) #final  means

model_g = ezANOVA(gammas,
                  between = participant,
                  within = target,
                  wid = i,
                  dv = g,
                  type = 3,
                  detailed = T)

model_g #no significant differences

model_g$ANOVA$MSE = model_g$ANOVA$SSd/model_g$ANOVA$DFd
model_g$ANOVA$MSE

aovEffectSize(model_g, effectSize = "pes")

####Do JOL RTs differ between participants?####
tapply(dat2.rt$Response.RT, list(dat2.rt$participant.ethnicity, dat2.rt$Target_Ethnicity), mean)

#Drop every participants 1st trial, and do a z-transformation and trim?
temp3 = data.frame()

for(i in unique(dat2.rt$Username)){
  
  temp4 = subset(dat2.rt,
                 dat2.rt$Username == i)
  
  temp4$zRT = scale(temp4$Response.RT)
  
  temp3 = rbind(temp3, temp4)
  
}

ZRT = temp3

ZRT = subset(ZRT,
             ZRT$zRT < 3)

#let's also drop any RTs that are less than second
ZRT = subset(ZRT,
             ZRT$Response.RT > 1000)

#and finally, any RTs above 15 seconds
ZRT = subset(ZRT,
             ZRT$Response.RT < 15000)

tapply(ZRT$Response.RT, list(ZRT$participant.ethnicity, ZRT$Target_Ethnicity), mean)

ZRT.long.w = cast(subset(ZRT,
                         ZRT$participant.ethnicity == "White"),
                  Username ~ Target_Ethnicity, mean, value = "Response.RT")

ZRT.long.b = cast(subset(ZRT,
                         ZRT$participant.ethnicity == "Black"),
                  Username ~ Target_Ethnicity, mean, value = "Response.RT")

apply(ZRT.long.b, 2, sd)
apply(ZRT.long.w, 2, sd)

##run an ANOVA
model_RT = ezANOVA(ZRT,
                  between = participant.ethnicity,
                  within = Target_Ethnicity,
                  wid = Username,
                  dv = Response.RT,
                  type = 3,
                  detailed = T)

model_RT #no significant differences

model_RT$ANOVA$MSE = model_RT$ANOVA$SSd/model_RT$ANOVA$DFd
model_RT$ANOVA$MSE

aovEffectSize(model_RT, effectSize = "pes")

##Okay, no differences##
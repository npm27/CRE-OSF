####Set up####
##read in the data
black.qs = read.csv("Data/black_qs.csv")
white.qs = read.csv("Data/white_qs.csv")
JOLs = read.csv("Data/JOLs.csv")

##load libraries
library(reshape)
library(psych)

##turn off scientific notation
options(scipen = 999)

#fix out of range JOLs
JOLs$Response.JOL[JOLs$Response.JOL > 100] = NA

##fix column names
colnames(JOLs)[3] = "JOL"
colnames(JOLs)[6] = "recog"

#get recog on same scale as JOLs
JOLs$recog = JOLs$recog * 100

####Get participants' mean JOL and Recog scores for each typicality type####
###Start w/ participant race
JOLs.black = subset(JOLs,
                    JOLs$participant.ethnicity == "Black")
JOLs.white = subset(JOLs,
                    JOLs$participant.ethnicity == "White")

JOLs.black.l = melt(JOLs.black,
                    measure.vars = c("JOL", "recog"))
JOLs.white.l = melt(JOLs.white,
                    measure.vars = c("JOL", "recog"))

colnames(JOLs.black.l)[5:6] = c("Measure", "score")
colnames(JOLs.white.l)[5:6] = c("Measure", "score")

####Now get the participant level data for JOLs and recog####
###Start w/ JOLs
##black participants
JOLs.black2 = cast(subset(JOLs.black.l,
                               JOLs.black.l$Measure == "JOL"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)


##white participants
JOLs.white2 = cast(subset(JOLs.white.l,
                          JOLs.white.l$Measure == "JOL"),
                   Username ~ Target_Ethnicity, mean, na.rm = T)

####Now start combining datasets####
colnames(JOLs.black2)[2:3] = c("JOL_B", "JOL_W")

colnames(JOLs.White2)[2:3] = c("JOL_B", "JOL_W")

##merge in qs
black_final = merge(JOLs.black2, black.qs, by.x = "Username", by.y = "Username")
white_final = merge(JOLs.white2, white.qs, by.x = "Username", by.y = "Username")

####Write to file
#write.csv(black_final, file = "black_qs_final.csv", row.names = F)
#write.csv(white_final, file = "white_qs_final.csv", row.names = F)

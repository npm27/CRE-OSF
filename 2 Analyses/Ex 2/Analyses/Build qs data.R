####Set up####
##read in the data
black.qs = read.csv("Data/black_qs.csv")
white.qs = read.csv("Data/white_qs.csv")
JOLs = read.csv("Data/JOL_data.csv")

##load libraries
library(reshape)
library(psych)

##turn off scientific notation
options(scipen = 999)

#fix out of range JOLs
JOLs$Response.JOL[JOLs$Response.JOL > 100] = NA

##fix column names
colnames(JOLs)[3] = "JOL"
colnames(JOLs)[5] = "Typicality"
colnames(JOLs)[8] = "recog"

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

colnames(JOLs.black.l)[7:8] = c("Measure", "score")
colnames(JOLs.white.l)[7:8] = c("Measure", "score")

##Start separating by typicality
JOLs.black_high = subset(JOLs.black.l,
                         JOLs.black.l$Typicality == "H")
JOLs.black_low = subset(JOLs.black.l,
                        JOLs.black.l$Typicality == "L")

JOLs.white_high = subset(JOLs.white.l,
                         JOLs.white.l$Typicality == "H")
JOLs.white_low = subset(JOLs.white.l,
                        JOLs.white.l$Typicality == "L")

####Now get the participant level data for JOLs and recog####
###Start w/ JOLs
##black participants
JOLs.black_high2 = cast(subset(JOLs.black_high,
                               JOLs.black_high$Measure == "JOL"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)
JOLs.black_low2 = cast(subset(JOLs.black_low,
                               JOLs.black_low$Measure == "JOL"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)

##white participants
JOLs.white_high2 = cast(subset(JOLs.white_high,
                               JOLs.white_high$Measure == "JOL"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)
JOLs.white_low2 = cast(subset(JOLs.white_low,
                              JOLs.white_low$Measure == "JOL"),
                       Username ~ Target_Ethnicity, mean, na.rm = T)

###Now recog
recog.black_high2 = cast(subset(JOLs.black_high,
                               JOLs.black_high$Measure == "recog"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)
recog.black_low2 = cast(subset(JOLs.black_low,
                              JOLs.black_low$Measure == "recog"),
                       Username ~ Target_Ethnicity, mean, na.rm = T)

##white participants
recog.white_high2 = cast(subset(JOLs.white_high,
                               JOLs.white_high$Measure == "recog"),
                        Username ~ Target_Ethnicity, mean, na.rm = T)
recog.white_low2 = cast(subset(JOLs.white_low,
                              JOLs.white_low$Measure == "recog"),
                       Username ~ Target_Ethnicity, mean, na.rm = T)

####Now start combining datasets####
black = cbind(JOLs.black_high2, JOLs.black_low2[ , -1],
              recog.black_high2[ , -1], recog.black_low2[ , -1])

colnames(black)[2:9] = c("JOL_B_H", "JOL_W_H", "JOL_B_L", "JOL_W_L",
                         "recog_B_H", "recog_W_H", "recog_B_L", "recog_W_L")

white = cbind(JOLs.white_high2, JOLs.white_low2[ , -1],
              recog.white_high2[ , -1], recog.white_low2[ , -1])

colnames(white)[2:9] = c("JOL_B_H", "JOL_W_H", "JOL_B_L", "JOL_W_L",
                         "recog_B_H", "recog_W_H", "recog_B_L", "recog_W_L")

##merge in qs
black_final = merge(black, black.qs, by.x = "Username", by.y = "Username")
white_final = merge(white, white.qs, by.x = "Username", by.y = "Username")

####Write to file
#write.csv(black_final, file = "black_qs_final.csv", row.names = F)
#write.csv(white_final, file = "white_qs_final.csv", row.names = F)

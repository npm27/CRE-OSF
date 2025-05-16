####Set up####
##read in the data
Qs = read.csv("Data/Qs.csv")

##load libraries
library(reshape)
library(splitstackshape)

##Fix response column
Qs$response = as.numeric(Qs$response)

##keep only white and black respondents
Qs = subset(Qs,
            Qs$participant.ethnicity == "White" | Qs$participant.ethnicity == "Black")

##Okay, need to get each participant's mean scale score
#first check if anyone failed the manip check
check = subset(Qs,
               Qs$Procedure.Procedure.Notes == "Manip_check")

#drop manip check items
Qs = subset(Qs,
            Qs$Procedure.Procedure.Notes != "Manip_check")

##Now subset out each measure
IMS_EMS = subset(Qs,
                 Qs$Procedure.Shuffle == "EM_IM")
ATB = subset(Qs,
             Qs$Procedure.Shuffle == "ATB")
ATW = subset(Qs,
             Qs$Procedure.Shuffle == "ATW")
contact = subset(Qs,
                 Qs$Procedure.Shuffle == "ContactQ")

###Okay, all participants will have EMS/IMS; White participants ATB; Black participants ATW
####Start w/ EMS_IMS####
##get each participant's mean
#seperate by subscale
EMS = subset(IMS_EMS, IMS_EMS$Procedure.Procedure.Notes == "External")
IMS = subset(IMS_EMS, IMS_EMS$Procedure.Procedure.Notes == "Interal") #hooray for typos...

##Okay, EMS first
EMS2 = cast(EMS, Username ~ participant.ethnicity, mean, na.rm = T)

#Now do IMS
IMS2 = cast(IMS, Username ~ participant.ethnicity, mean, na.rm = T)

####Now ATB/ATW####
###Start w/ ATB
##First need to reverse code neg items
ATB.pos = subset(ATB,
                 ATB$Procedure.Procedure.Notes == "Pos")
ATB.neg = subset(ATB,
                 ATB$Procedure.Procedure.Notes == "Neg")

ATB.neg$response = 1 - ATB.neg$response + 5 #reverse

ATB = rbind(ATB.pos, ATB.neg)

ATB2 = cast(subset(ATB,
                   ATB$participant.ethnicity == "White"),
            Username ~ participant.ethnicity, mean, na.rm = T)

###Now do ATW
##again, reverse code neg items
ATW.pos = subset(ATW,
                 ATW$Procedure.Procedure.Notes == "Pos")
ATW.neg = subset(ATW,
                 ATW$Procedure.Procedure.Notes == "Neg")

ATW.neg$response = 1 - ATW.neg$response + 5 #reverse

ATW = rbind(ATW.pos, ATW.neg)

ATW2 = cast(subset(ATW,
                   ATW$participant.ethnicity == "Black"),
            Username ~ participant.ethnicity, mean, na.rm = T)

###And contact####
contact2 = cast(contact, Username ~ participant.ethnicity, mean, na.rm = T)

####okay, build the black and white datasets####
##Start w/ black participants
black = merge(ATW2, contact2, by.x = "Username", by.y = "Username")
colnames(black)[2:3] = c("ATW", "contact")

black = merge(black[ , 1:3], EMS2, by.x = "Username", by.y = "Username")
colnames(black)[4] = "EMS"

black = merge(black[ , 1:4], IMS2, by.x = "Username", by.y = "Username")
colnames(black)[5] = "IMS"

black = black[ , -6]

##Now white participants
white = merge(ATB2, contact2, by.x = "Username", by.y = "Username")
colnames(white)[2] = "ATB"  
colnames(white)[4] = "contact"

white = merge(white[ , c(1:2,4)], EMS2, by.x = "Username", by.y = "Username")
colnames(white)[5] = "EMS"

white = merge(white[ , c(1:3, 5)], IMS2, by.x = "Username", by.y = "Username")
colnames(white)[6] = "IMS"

white = white[ , -5]

####Write to file####
#write.csv(white, file = "white_qs.csv", row.names = F)
#write.csv(black, file = "black_qs.csv", row.names = F)

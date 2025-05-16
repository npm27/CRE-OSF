####Set up####
##read in everything
Dat = read.csv("DemographicsData.csv")
IDs = read.csv("JOL_data.csv")

##load libraries
library(memisc)
library(dplyr)

##Pull final IDs
Dat = Dat %>% 
  filter(Username %in% IDs$Username)

#final n = 157
#make sure everyone is only identifying as Black or White
Dat = subset(Dat,
             Dat$Race == "Black" | Dat$Race == "White")

#remove duplicates?
Dat = Dat[!duplicated(Dat[ , "Username"]),]

#There we go!
Prolific_b = subset(Dat,
                    Dat$Race == "Black")
prolific_w = subset(Dat,
                    Dat$Race == "White")

####Get descriptives####
mean(Prolific_b$Age); sd(Prolific_b$Age)
mean(prolific_w$Age); sd(prolific_w$Age)

percent(Prolific_b$Gender)
percent(prolific_w$Gender)

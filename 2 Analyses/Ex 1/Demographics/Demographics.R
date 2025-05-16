####Build out Table 1####
dat = rbind(read.csv("Demos/DemographicsData_EC.csv"), read.csv("Demos/DemographicsData_MSU.csv"))
dat2 = read.csv("Demos/DemographicsData_JSU.csv")

IDs = rbind(read.csv("IDs/JOL_data_EC.csv"), read.csv("IDs/JOL_data.csv"))
IDs2 = read.csv("IDs/JOL_data_JSU.csv")

length(unique(IDs$Username))
length(unique(IDs2$Username))

##Load libraries
library(dplyr)
library(memisc)

##pull IDs for black and white participants
dat_JSU = dat2 %>% 
          filter(Username %in% IDs2$Username)

dat_MSU = dat %>%
          filter(Username %in% IDs$Username)

dat_MSU = subset(dat_MSU,
            dat_MSU$Race == "White" | dat_MSU$Race == "Black")
dat_JSU = subset(dat_JSU,
            dat_JSU$Race == "White" | dat_JSU$Race == "Black")

dat_MSU = dat_MSU[!duplicated(dat_MSU[ , "Username"]),]

##Okay, cool!
MSU_b = subset(dat_MSU,
               dat_MSU$Race == "Black")
MSU_w = subset(dat_MSU,
               dat_MSU$Race == "White")

#age
mean(MSU_b$Age); sd(MSU_b$Age)
mean(MSU_w$Age); sd(MSU_w$Age)

mean(dat_JSU$Age); sd(dat_JSU$Age)

#percent female
percent(MSU_b$Gender)
percent(MSU_w$Gender)

percent(dat_JSU$Gender)

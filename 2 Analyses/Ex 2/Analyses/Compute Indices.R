####Set up####
##load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##read in data
ex1 = read.csv("Data/recog_data.csv")

####Get the data in the right shape####
##For each participant, it looks like I need to sum total for each of the four categories
##Hits, False alarms, misses, and correct rejections

##first, code each trial as being one of the categories
#not presented, 0 == correct rejection
#not presented, 1 == False alarm
#presented, 0 == miss
#presented, 1 == hit

##Start w/ hits
hits1 = subset(ex1,
               ex1$key == "OLD" & ex1$scored == 1)
hits1$sig_type = rep("hit")

miss1 = subset(ex1,
             ex1$key== "OLD" & ex1$scored == 0)
miss1$sig_type = rep("miss")

fa1 = subset(ex1,
             ex1$key == "NEW" & ex1$scored == 1)
fa1$sig_type = rep("fa")

cr1 = subset(ex1,
             ex1$key == "NEW" & ex1$scored == 0)
cr1$sig_type = rep("cr")

##put it all back together
combined1 = rbind(hits1, miss1, fa1, cr1)

combined1 = subset(combined1,
                   combined1$participant_ethnicity == "Black" | combined1$participant_ethnicity == "White")

##Now compute seperate indices for each category
high_black = subset(combined1,
                    combined1$Target_Ethnicity == "B" & combined1$Stimuli.Stimuli.Notes_3 == "H")
low_black = subset(combined1,
                   combined1$Target_Ethnicity == "B" & combined1$Stimuli.Stimuli.Notes_3 == "L")

high_white = subset(combined1,
                    combined1$Target_Ethnicity == "W" & combined1$Stimuli.Stimuli.Notes_3 == "H")
low_white = subset(combined1,
                   combined1$Target_Ethnicity == "W" & combined1$Stimuli.Stimuli.Notes_3 == "L")

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()
combined2 = high_white

for(i in unique(combined2$Username)){ 
  
  #loop through participants
  temp = subset(combined2, 
                combined2$Username == i)
  
  #sum hits
  hits = subset(temp,
                temp$sig_type == "hit")
  
  h = length(hits$sig_type)
  
  #sum misses
  misses = subset(temp,
                  temp$sig_type == "miss")
  
  m = length(misses$sig_type)
  
  #false alarms
  fas = subset(temp,
               temp$sig_type == "fa")
  
  f = length(fas$sig_type)
  
  #correct rejections
  crs = subset(temp,
               temp$sig_type == "cr")
  
  c = length(crs$sig_type)
  
  #get participant's ethnicity
  e = temp$participant_ethnicity[1]
  
  #now slap it back together
  temp2 = data.frame(i, e, h, m, f, c)
  
  sig_detect = rbind(sig_detect, temp2)
  
}

highwhite2 = sig_detect

####compute indices####
indices1 = data.frame(dprime(highblack2$h, highblack2$f, highblack2$m, highblack2$c))
indices2 = data.frame(dprime(lowblack2$h, lowblack2$f, lowblack2$m, lowblack2$c))
indices3 = data.frame(dprime(highwhite2$h, highwhite2$f, highwhite2$m, highwhite2$c))
indices4 = data.frame(dprime(lowwhite2$h, lowwhite2$f, lowwhite2$m, lowwhite2$c))

ex2_high_black = cbind(highblack2, indices1)
ex2_low_black = cbind(lowblack2, indices2)
ex2_high_white = cbind(highwhite2, indices3)
ex2_low_white = cbind(lowwhite2, indices4)

#write.csv(ex2_high_black, file = "Indices/high_black_indices.csv", row.names = F)
#write.csv(ex2_low_black, file = "Indices/low_black_indices.csv", row.names = F)
#write.csv(ex2_high_white, file = "Indices/high_white_indices.csv", row.names = F)
#write.csv(ex2_low_white, file = "Indices/low_white_indices.csv", row.names = F)

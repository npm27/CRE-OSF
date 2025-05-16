####Set up####
##load libraries
library(psycho)
library(ez)
library(reshape)
library(psychReport)

##read in data
ex1 = read.csv("Data/recog.csv")

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
black = subset(combined1,
                    combined1$Target_Ethnicity == "B")
white = subset(combined1,
                    combined1$Target_Ethnicity == "W")

####Now sum the categories for each participant####
##Loop time?
sig_detect = data.frame()
combined2 = black

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

black2 = sig_detect

####compute indices####
indices1 = data.frame(dprime(black2$h, black2$f, black2$m, black2$c))
indices2 = data.frame(dprime(white2$h, white2$f, white2$m, white2$c))

black3 = cbind(black2, indices1)
white3 = cbind(black2, indices2)

#write.csv(black3, file = "Indices/black_indices.csv", row.names = F)
#write.csv(white3, file = "Indices/white_indices.csv", row.names = F)

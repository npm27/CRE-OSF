####Combine datasets for each source###
JOL = rbind(read.csv("EC/JOL_data_EC.csv"), read.csv("JSU/JOL_data_JSU.csv"), read.csv("MSU/JOL_data.csv"))
recog = rbind(read.csv("EC/recog_data_EC.csv"), read.csv("JSU/recog_data_JSU.csv"), read.csv("MSU/recog_data.csv"))
Qs = rbind(read.csv("EC/questionnaires_final_EC.csv"), read.csv("JSU/questionnaires_final_JSU.csv"), read.csv("MSU/questionnaires.final.csv"))

#write.csv(JOL, file = "JOLs.csv", row.names = F)
#write.csv(recog, file = "recog.csv", row.names = F)
#write.csv(Qs, file = "Qs.csv", row.names = F)

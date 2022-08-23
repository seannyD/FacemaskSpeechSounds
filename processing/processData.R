library(dplyr)
library(reshape)
library(stringr)
try(setwd("~/OneDrive - Cardiff University/Supervision/UGDissertations/Havill/experiment/processing/"))

d = read.csv("../data/FM Perception Consent Form (Responses) - Form responses 1.csv",stringsAsFactors = F)

d$IDNum = paste0("P",sample(1:nrow(d)))

d2= as.data.frame(melt(d[,-(1:2)], id.vars="IDNum"))

d2$QuestionNum = sapply(d2$variable,function(X){
  substr(X,1,gregexpr(pattern ='\\.',X)[[1]][1]-1)
  })
d2$round = sapply(d2$variable,function(X){
  as.numeric(substr(X,nchar(as.character(X)),nchar(as.character(X)))=='1')+1
})
d2$response = tolower(d2$value)

# One observation is missing
d2 = d2[d2$value!="",]

q = read.csv("../data/QuestionKey.csv",stringsAsFactors = F)

d2$condition = q[match(d2$QuestionNum,q$Qnum),]$Condition
d2$target = q[match(d2$QuestionNum,q$Qnum),]$Correct
d2$correct = as.numeric(
  tolower(q[match(d2$QuestionNum,q$Qnum),]$Correct) ==
    tolower(d2$response))

d2$manner = q[match(d2$QuestionNum,q$Qnum),]$Manner
d2$voicing = q[match(d2$QuestionNum,q$Qnum),]$Voicing


d2 = d2[,c("IDNum","QuestionNum","round","condition","target","response","correct","voicing","manner")]

write.csv(d2,"../data/cleanedData.csv",row.names = F)

tx = table(paste("Target",d2$target),paste("Guess",d2$response))
write.csv(tx, "../data/confusionMatrix.csv")

txMasked = table(paste("Target",d2[d2$condition=="Masked",]$target),
           paste("Guess",d2[d2$condition=="Masked",]$response))
write.csv(txMasked, "../data/confusionMatrix_Masked.csv")

txUnMasked = table(paste("Target",d2[d2$condition=="Unmasked",]$target),
                 paste("Guess",d2[d2$condition=="Unmasked",]$response))
write.csv(txUnMasked, "../data/confusionMatrix_Unmasked.csv")


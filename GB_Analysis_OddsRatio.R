
rm(list =ls())
dataClassProb <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/Ensemble_LocVar3_DBF5_output.csv")

library(ggplot2)

  
 dataClassProb_split <- split(dataClassProb , f = dataClassProb$FailedRobot)

 cm <- list()
 misClasificError <- list()
 Accuracy <- list()
 Sensitivity <- list()
 Specificity <- list()
 cm_TN <- list()
 cm_FN <- list()
 cm_FP <- list()
 cm_TP <- list()
  
noFailedRobot <-  length(dataClassProb_split) 
for (i in 1: noFailedRobot ) { 
cm[[i]]  <- confusionMatrix(dataClassProb_split[[i]]$predictions_ensemble,  dataClassProb_split[[i]]$Class,positive = "1")
misClasificError[[i]] <- mean(dataClassProb_split[[i]]$predictions_ensemble != dataClassProb_split[[i]]$Class)
Accuracy[[i]]  <- 1 - misClasificError[[i]]
Sensitivity[[i]] <- cm[[i]]$byClass['Sensitivity'] 
Specificity[[i]] <- cm[[i]]$byClass['Specificity'] 
cm_TN[[i]] <- cm[[i]]$table[1,1] 
cm_FN[[i]] <- cm[[i]]$table[1,2] 
cm_FP[[i]] <- cm[[i]]$table[2,1]
cm_TP[[i]] <- cm[[i]]$table[2,2]
}

Accuracy_all <- unsplit(Accuracy, f=unique(dataClassProb$FailedRobot))
Sensitivity_all <- unsplit(Sensitivity,f=unique(dataClassProb$FailedRobot))
Specificity_all <- unsplit(Specificity,f=unique(dataClassProb$FailedRobot))
cm_TN_all <- unsplit(cm_TN,f=unique(dataClassProb$FailedRobot))
cm_FN_all <- unsplit(cm_FN,f=unique(dataClassProb$FailedRobot))
cm_FP_all <- unsplit(cm_FP,f=unique(dataClassProb$FailedRobot))
cm_TP_all <- unsplit(cm_TP,f=unique(dataClassProb$FailedRobot))




RobotList <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")

PerformanceData <- cbind( Accuracy_all,Sensitivity_all, Specificity_all,cm_TN_all,cm_FN_all,cm_FP_all,cm_TP_all)
PerformanceData <- as.data.frame(PerformanceData)

PerformanceData$cm_TN_all <- ifelse(PerformanceData$cm_FN_all==0,cm_TN_all+0.5,cm_TN_all)
PerformanceData$cm_FP_all <- ifelse(PerformanceData$cm_FN_all==0,cm_FP_all+0.5,cm_FP_all)
PerformanceData$cm_TP_all <- ifelse(PerformanceData$cm_FN_all==0,cm_TP_all+0.5,cm_TP_all)
PerformanceData$cm_FN_all <- ifelse(PerformanceData$cm_FN_all==0,cm_FN_all+0.5,cm_FN_all)



indx <- sapply(PerformanceData, is.factor)
PerformanceData[indx] <- lapply(PerformanceData[indx], function(x) as.numeric(as.character(x)))


is.num <- sapply(PerformanceData, is.numeric)
PerformanceData[is.num] <- lapply(PerformanceData[is.num], round,3)
 
PerformanceData <- cbind(RobotList, PerformanceData)

PerformanceData$Sensitivity_all <- ifelse(PerformanceData$Sensitivity_all==1,0.999,PerformanceData$Sensitivity_all)
PerformanceData$PLR <- PerformanceData$Sensitivity_all/(1-PerformanceData$Specificity_all)
PerformanceData$NLR <- (1-PerformanceData$Sensitivity_all)/(PerformanceData$Specificity_all)
PerformanceData$DOR <- PerformanceData$PLR /PerformanceData$NLR

library(fmsb)

myfunc <- function(var1, var2, var3,var4){
  result <- oddsratio(var1, var2, var3, var4, conf.level=0.95)
  return(result)
}
 
oddsRatio <- apply(PerformanceData[,5:8],1,function(x)myfunc(x[1],x[2],x[3],x[4]))

for (i in 1: length(oddsRatio)){
  
  PerformanceData$DOR_UL[i] <- oddsRatio[[i]]$conf.int[1]
  PerformanceData$DOR_LL[i] <- oddsRatio[[i]]$conf.int[2]
}

 
##Forest Plots
type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
PerformanceData$RobotList <- factor(PerformanceData$RobotList, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
ggplot(data=PerformanceData,aes(x=DOR,y=RobotList))+
  geom_point(aes(fill=type), colour="black",shape=21)+
  geom_errorbarh(aes(xmin=DOR_LL,xmax=DOR_UL),height=0.2,colour="blue")+
  geom_vline(xintercept=1,linetype="dashed")+
   scale_x_log10()+ xlab("Diagnoistics Odds Ratio in log scale") + ylab("Robot No")+
  theme(legend.position="none") + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )

ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/DiagnosticOddsRatio.pdf", width = 20, height = 20, units = "cm")

write.csv(PerformanceData,"C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/PerformanceData_ensemble_DOR_LocVar3_DBF5.csv")

##Forest plot for all DBFs

PerformanceData_all <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/PerformanceData_ensemble_DOR_LocVar3_DBF_all.csv")

##Forest Plots
type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
PerformanceData_all$RobotNo <- factor(PerformanceData_all$RobotNo, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
ggplot(data=PerformanceData_all,aes(x=DOR,y=RobotNo))+ facet_wrap(~DBF)+
  geom_point(aes(fill="black"), colour="black",shape=21)+
  geom_errorbarh(aes(xmin=DOR_LL,xmax=DOR_UL),height=0.2,colour="blue")+
  geom_vline(xintercept=1,linetype="dashed")+
  scale_x_log10()+ xlab("Diagnoistics Odds Ratio in log scale") + ylab("Robot No")+
  theme(legend.position="none") + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )



ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/DiagnosticOddsRatio_LocVar3_DBF_all.pdf", width = 20, height = 20, units = "cm")


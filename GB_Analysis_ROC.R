
rm(list =ls())
dataClassProb <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/Ensemble_LocVar3_DBF3_output.csv")

library(ggplot2)



 dataClassProb_split <- split(dataClassProb , f = dataClassProb$FailedRobot)


RobotList <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")

# PerformanceData <- cbind( Accuracy_all,Sensitivity_all, Specificity_all,cm_TN_all,cm_FN_all,cm_FP_all,cm_TP_all)
# PerformanceData <- as.data.frame(PerformanceData)


###RoC curve

library(ROCR)

pred <- prediction(dataClassProb_split[[1]]$predictions_ensemble_prob, dataClassProb_split[[1]]$Class)
perf <- performance(pred,"tpr","fpr")

library(plotROC)

basicplot <- ggplot(dataClassProb_split[[1]], aes(m = predictions_ensemble_prob, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
basicplot

rp <- ggplot(dataClassProb,  aes(m = predictions_ensemble_prob, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
rp<- rp +facet_wrap(~FailedRobot)  
rp <-  rp+ style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rp)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .2))
rp
 

dataClassProb$FailedRobot <- factor(dataClassProb$FailedRobot, levels=c("R1", "R2", "R3","R4","R5","R6","R7","R8","R9","R10","R11"))

rp <- ggplot(dataClassProb,  aes(m = Diff_PCA_LR, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
rp<- rp +facet_wrap(~FailedRobot)  
rp <-  rp+ style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("ROC Curves for Out of Logistic Regression - 1 Day before failure") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rp)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .2))
rp
ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/ROC_LR_DBF3.pdf", width = 20, height = 20, units = "cm")


rp <- ggplot(dataClassProb,  aes(m = Diff_PCA_SVM, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
rp<- rp +facet_wrap(~FailedRobot)  
rp <-  rp+ style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rp)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .2))
rp

rp <- ggplot(dataClassProb,  aes(m = Diff_PCA_RF, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
rp<- rp +facet_wrap(~FailedRobot)  
rp <-  rp+ style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rp)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .2))
rp



##AUC with confidence interval

library(pROC)
rocobj <- list()
ciauc  <- list()
ciauc_LL <- list()
ciauc_UL <- list()

for (i in 1:length(dataClassProb_split) ){
rocobj[[i]] <- roc(dataClassProb_split[[i]]$Class,dataClassProb_split[[i]]$predictions_ensemble_prob ,direction = "<"   )
ciauc_LL[[i]] <- ci.auc(rocobj[[i]])[1] 
ciauc[[i]] <- ci.auc(rocobj[[i]])[2] 
ciauc_UL[[i]] <- ci.auc(rocobj[[i]])[3] 
}

 
ciauc_LL_all <- unsplit(ciauc_LL,f=unique(dataClassProb$FailedRobot))
ciauc_all <- unsplit(ciauc,f=unique(dataClassProb$FailedRobot))
ciauc_UL_all <- unsplit(ciauc_UL,f=unique(dataClassProb$FailedRobot))

PerformanceData_ensemble <- cbind(  ciauc_LL_all, ciauc_all,ciauc_UL_all)
PerformanceData_ensemble <- as.data.frame(PerformanceData_ensemble)

indx <- sapply(PerformanceData_ensemble, is.factor)
PerformanceData_ensemble[indx] <- lapply(PerformanceData_ensemble[indx], function(x) as.numeric(as.character(x)))


is.num <- sapply(PerformanceData_ensemble, is.numeric)
PerformanceData_ensemble[is.num] <- lapply(PerformanceData_ensemble[is.num], round,3)
PerformanceData_ensemble <- cbind(RobotList,PerformanceData_ensemble)
PerformanceData_ensemble$Method <- c("Ensemble")
##Forest Plots
type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
PerformanceData_ensemble$RobotList <- factor(PerformanceData_ensemble$RobotList, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
ggplot(data=PerformanceData_ensemble,aes(x=ciauc_all,y=RobotList))+
  geom_point(aes(fill=type), colour="black",shape=21)+
  geom_errorbarh(aes(xmin=ciauc_LL_all,xmax=ciauc_UL_all),height=0.2,colour="blue")+
  geom_vline(xintercept=0.5,linetype="dashed")+
  xlab("AUC") + ylab("Robot No")+
  theme(legend.position="none") 

ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_CI_LocVar5_DBF3.pdf", width = 20, height = 20, units = "cm")

write.csv(PerformanceData_ensemble,"C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/PerformanceData_ensemble_LocVar3_DBF3.csv")

##Test to compare ROC curves
 roc.test(rocobj[[1]],rocobj[[2]])
 
 
#Add AUC for  each of the Method
 
 library(pROC)
 rocobj_LR <- list()
 ciauc_LR  <- list()
 ciauc_LL_LR <- list()
 ciauc_UL_LR <- list()
 
 for (i in 1:length(dataClassProb_split) ){
   rocobj_LR[[i]] <- roc(dataClassProb_split[[i]]$Class,dataClassProb_split[[i]]$Diff_PCA_LR, direction = "<" )
 
   ciauc_LR[[i]] <- ci.auc(rocobj_LR[[i]])[2] 
   ciauc_LL_LR[[i]] <- ci.auc(rocobj_LR[[i]])[1] 
   ciauc_UL_LR[[i]] <- ci.auc(rocobj_LR[[i]])[3] 
 }
 
 ciauc_LR_all <- unsplit(ciauc_LR,f=unique(dataClassProb$FailedRobot))
 ciauc_LL_LR_all <- unsplit(ciauc_LL_LR,f=unique(dataClassProb$FailedRobot))
 ciauc_UL_LR_all <- unsplit(ciauc_UL_LR,f=unique(dataClassProb$FailedRobot))
 
 PerformanceData_LR <- cbind(ciauc_LR_all,ciauc_LL_LR_all,ciauc_UL_LR_all)
 PerformanceData_LR <- as.data.frame(PerformanceData_LR)
 indx <- sapply(PerformanceData_LR, is.factor)
 PerformanceData_LR[indx] <- lapply(PerformanceData_LR[indx], function(x) as.numeric(as.character(x)))
 
 
 is.num <- sapply(PerformanceData_LR, is.numeric)
 PerformanceData_LR[is.num] <- lapply(PerformanceData_LR[is.num], round,3)
 
 PerformanceData_LR <- cbind(RobotList, PerformanceData_LR)

 PerformanceData_LR$Method <- c("LR")

 
 
  
 ###SVM
 rocobj_svm <- list()
 ciauc_svm  <- list()
 ciauc_LL_svm <- list()
 ciauc_UL_svm <- list()
 
 for (i in 1:length(dataClassProb_split) ){
   rocobj_svm[[i]] <- roc(dataClassProb_split[[i]]$Class,dataClassProb_split[[i]]$Diff_PCA_SVM,direction = "<" )
   
   ciauc_svm[[i]] <- ci.auc(rocobj_svm[[i]])[2] 
   ciauc_LL_svm[[i]] <- ci.auc(rocobj_svm[[i]])[1] 
   ciauc_UL_svm[[i]] <- ci.auc(rocobj_svm[[i]])[3] 
   
 }
 
 ciauc_svm_all <- unsplit(ciauc_svm,f=unique(dataClassProb$FailedRobot))
 ciauc_LL_svm_all <- unsplit(ciauc_LL_svm,f=unique(dataClassProb$FailedRobot))
 ciauc_UL_svm_all <- unsplit(ciauc_UL_svm,f=unique(dataClassProb$FailedRobot))
 
 PerformanceData_svm <- cbind( ciauc_svm_all,ciauc_LL_svm_all,ciauc_UL_svm_all)
 
 PerformanceData_svm <- as.data.frame(PerformanceData_svm)
 indx <- sapply(PerformanceData_svm, is.factor)
 PerformanceData_svm[indx] <- lapply(PerformanceData_svm[indx], function(x) as.numeric(as.character(x)))
 
 
 is.num <- sapply(PerformanceData_svm, is.numeric)
 PerformanceData_svm[is.num] <- lapply(PerformanceData_svm[is.num], round,3)
 
 PerformanceData_svm <- cbind(RobotList, PerformanceData_svm)
 
 PerformanceData_svm$Method <- c("SVM")
 
 
 ###rf
 rocobj_rf <- list()
 ciauc_rf <- list()
 ciauc_LL_rf <- list()
 ciauc_UL_rf <- list()
 
 for (i in 1:length(dataClassProb_split) ){
   rocobj_rf[[i]] <- roc(dataClassProb_split[[i]]$Class,dataClassProb_split[[i]]$Diff_PCA_RF,direction = "<")
   
   ciauc_rf[[i]] <- ci.auc(rocobj_rf[[i]])[2] 
   ciauc_LL_rf[[i]] <- ci.auc(rocobj_rf[[i]])[1] 
   ciauc_UL_rf[[i]] <- ci.auc(rocobj_rf[[i]])[3] 
   
 }
 
 ciauc_rf_all <- unsplit(ciauc_rf,f=unique(dataClassProb$FailedRobot))
 
 ciauc_LL_rf_all <- unsplit(ciauc_LL_rf,f=unique(dataClassProb$FailedRobot))
 ciauc_UL_rf_all <- unsplit(ciauc_UL_rf,f=unique(dataClassProb$FailedRobot))
 PerformanceData_rf <- cbind( ciauc_rf_all,ciauc_LL_rf_all,ciauc_UL_rf_all)
 
 PerformanceData_rf <- as.data.frame(PerformanceData_rf)
 indx <- sapply(PerformanceData_rf, is.factor)
 PerformanceData_rf[indx] <- lapply(PerformanceData_rf[indx], function(x) as.numeric(as.character(x)))
 
 
 is.num <- sapply(PerformanceData_rf, is.numeric)
 PerformanceData_rf[is.num] <- lapply(PerformanceData_rf[is.num], round,3)
 
 PerformanceData_rf <- cbind(RobotList, PerformanceData_rf)
 
 PerformanceData_rf$Method <- c("RF")
 
 names(PerformanceData_LR) <- c("RobtoNo", "AUC","AUC_LL","AUC_UL","Method")
 names(PerformanceData_svm) <- c("RobtoNo", "AUC","AUC_LL","AUC_UL","Method")
 names(PerformanceData_rf) <- c("RobtoNo", "AUC","AUC_LL","AUC_UL","Method")
 
 PerformanceData <- rbind(PerformanceData_LR,PerformanceData_svm,PerformanceData_rf)
 
 ##Forest Plots
 type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
 PerformanceData$RobtoNo <- factor(PerformanceData$RobtoNo, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
 ggplot(data=PerformanceData,aes(x=AUC,y=RobtoNo))+facet_wrap(~Method)+
    geom_point( aes(fill="black"), colour="black",shape=21)+
   geom_errorbarh(aes(xmin=AUC_LL,xmax=AUC_UL),height=0.2,colour="blue")+
   geom_vline(xintercept=0.5,linetype="dashed")+
   xlab("AUC") + ylab("Robot No")+
   theme(legend.position="none") 
 
 ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_All_methods_CI.pdf", width = 20, height = 20, units = "cm")
 

 ##Local Variation 3 with all DBFs 
 PreformanceData_all <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/PerformanceData_ensemble_LocVar3_all.csv")
 ##Forest Plots
 type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
 PreformanceData_all$RobotNo <- factor(PreformanceData_all$RobotNo, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
 ggplot(data=PreformanceData_all,aes(x=AUC,y=RobotNo))+facet_wrap(~DBF)+
   geom_point( aes(fill="black"), colour="black",shape=21)+
   geom_errorbarh(aes(xmin=AUC_LL,xmax=AUC_UL),height=0.2,colour="blue")+
   geom_vline(xintercept=0.5,linetype="dashed")+
   xlab("AUC") + ylab("Robot No")+
   theme(legend.position="none") 
 
 ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_LocVar3_DBF_ALL_CI.pdf", width = 20, height = 20, units = "cm")
 
 
 
 
 
 library(ggplot2)
 ##Local Variation 3 with all DBFs 
 PreformanceData_all <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/PerformanceData_ensemble_LocVar_all_DBF3.csv")

 ##Forest Plots
 type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
 PreformanceData_all$LocVar <- factor(PreformanceData_all$LocVar, c("No","3 Days","5 Days"))
 PreformanceData_all$RobotNo <- factor(PreformanceData_all$RobotNo, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
 ggplot(data=PreformanceData_all,aes(x=AUC,y=RobotNo))+facet_wrap(~LocVar)+
  geom_point( aes(fill="black"), colour="black",shape=21)+
   geom_errorbarh(aes(xmin=AUC_LL,xmax=AUC_UL),height=0.2,colour="blue")+
   geom_vline(xintercept=0.5,linetype="dashed")+
   xlab("AUC") + ylab("Robot No")+
   theme(legend.position="none")  
 
 ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_DBF_3_LocVar_ALL_CI.pdf", width = 20, height = 20, units = "cm")
 
 
 PreformanceData_all$check <- ifelse(PreformanceData_all$AUC <0.5,"1",
                                     ifelse(PreformanceData_all$AUC <0.6,"2",
                                            ifelse(PreformanceData_all$AUC <0.7,"3",
                                                   ifelse(PreformanceData_all$AUC <0.8,"4",
                                                          ifelse(PreformanceData_all$AUC <0.9,"5",
                                                                 ifelse(PreformanceData_all$AUC <1.0,"6"
                                                                 ))))))
 
 #PreformanceData_all$col <- cut(PreformanceData_all$AUC, c(0,0.5,0.7,  1.0))
 cols = c("1" = "blue",
          "2" = "green",
          "3" = "gold",
          "4" = "orange",
          "5" = "red",
          "6" = "firebrick")
   type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
 PreformanceData_all$LocVar <- factor(PreformanceData_all$LocVar, c("No","3 Days","5 Days"))
 PreformanceData_all$RobotNo <- factor(PreformanceData_all$RobotNo, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
 ggplot(data=PreformanceData_all,aes(x=AUC,y=RobotNo))+facet_wrap(~LocVar)+
   geom_point( aes(fill=check,colour =check), shape=21)+
   #geom_point( aes(colour=col,fill= col),shape=21)+
   #geom_point(aes(colour=col)+
   geom_errorbarh(aes(xmin=AUC_LL,xmax=AUC_UL,colour=check),height=0.2)+
   geom_vline(xintercept=0.5,linetype="dashed")+
   xlab("AUC") + ylab("Robot No")+
   theme(legend.position="none") +
   scale_color_manual( values = cols)
 
 ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_DBF_3_LocVar_ALL_CI.pdf", width = 20, height = 20, units = "cm")
 
 
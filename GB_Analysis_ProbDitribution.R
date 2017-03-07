
rm(list =ls())
dataClassProb <- read.csv("C:/IIITD/WIP/Analysis/Journal/TestResults_Final/EnsembleResults/Ensemble_LocVar3_DBF1_output.csv")

library(ggplot2)

a <- ggplot(dataClassProb, aes(x  =predictions_ensemble_prob ))
a <- a + geom_histogram(data=subset(dataClassProb,Class == 1),fill = "red", binwidth = 0.1,alpha = 0.5)
a <-  a + geom_histogram(data=subset(dataClassProb,Class == 0),fill = "blue", binwidth = 0.1,alpha = 0.5)
a+facet_wrap(~FailedRobot,ncol=3,scales ="free")

dataClassProb$Class <- as.factor(dataClassProb$Class)

a <- ggplot(dataClassProb, aes(x  =predictions_ensemble_prob, fill= Class ))
 a<-  a + geom_histogram( binwidth = 0.1,alpha = 0.8,position="identity")
a<-  a+facet_wrap(~FailedRobot,ncol=3,scales ="free")+xlab("Probability of Failed Class")
a <- a +ylab("Frequency")
a +scale_fill_manual(values=c("blue", "red"), 
                     name="Robot Condition",
                     labels=c( "Normal", "Failed"))




###KL Distance
# dataClassProb_Rob1_C1 <- dataClassProb[which(dataClassProb$Class==1 & dataClassProb$FailedRobot==1),]
# dataClassProb_Rob1_C0 <- dataClassProb[which(dataClassProb$Class==0 & dataClassProb$FailedRobot==1),]
# 
# dataClassProb_Rob4_C1 <- dataClassProb[which(dataClassProb$Class==1 & dataClassProb$FailedRobot==4),]
# dataClassProb_Rob4_C0 <- dataClassProb[which(dataClassProb$Class==0 & dataClassProb$FailedRobot==4),]
# dataClassProb_Rob5_C1 <- dataClassProb[which(dataClassProb$Class==1 & dataClassProb$FailedRobot==5),]
# dataClassProb_Rob5_C0 <- dataClassProb[which(dataClassProb$Class==0 & dataClassProb$FailedRobot==5),]
# 
# dataClassProb_Rob11_C1 <- dataClassProb[which(dataClassProb$Class==1 & dataClassProb$FailedRobot==11),]
# dataClassProb_Rob11_C0 <- dataClassProb[which(dataClassProb$Class==0 & dataClassProb$FailedRobot==11),]
# 
# 
# library(seewave)
# 
# kl.dist(dataClassProb_Rob5_C1$predictions_ensemble_prob, dataClassProb_Rob5_C0$predictions_ensemble_prob, base = 2)

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
   scale_x_log10()+ xlab("Diagnoistics Odds Ratio in log scale") + ylab("Robot List")+
  theme(legend.position="none") 

ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/DiagnosticOddsRatio.pdf", width = 20, height = 20, units = "cm")



 
 # scale_size_continuous(breaks=c(5000,10000,15000))+
 # geom_text(aes(x=2.8,label=type),size=4)

 
# 
# # d is a data frame with 4 columns
# # d$x gives variable names
# # d$y gives center point
# # d$ylo gives lower limits
# # d$yhi gives upper limits
# forestplot <- function(d, xlab="Odds Ratio", ylab="Study"){
#   require(ggplot2)
#   p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi)) + 
#     geom_pointrange() + 
#     coord_flip() +
#     geom_hline(aes(x=0), lty=2) +
#     ylab(xlab) +
#     xlab(ylab) #switch because of the coord_flip() above
#   return(p)
# }
# 
#   
# d <- PerformanceData[,c(1,11:13)]
# #orderColumn <- c(2,1,3,4)
#  d <- d[,orderColumn]
# forestplot(d)
library(ggplot2)

a <- ggplot(PerformanceData, aes(x  = RobotList, y =Accuracy_all ))
a <-  a+geom_bar(stat ="identity")
a
 
library(corrplot)
M <- cor(dataClassProb[,2:4])
corrplot(M, method="number")



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
 


rp <- ggplot(dataClassProb,  aes(m = Diff_PCA_LR, d = Class)) + geom_roc(n.cuts = 50, labels = FALSE)
rp<- rp +facet_wrap(~FailedRobot)  
rp <-  rp+ style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rp)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .2))
rp

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

library(pROC)
rocobj <- list()
ciauc  <- list()
ciauc_LL <- list()
ciauc_UL <- list()

for (i in 1:length(dataClassProb_split) ){
rocobj[[i]] <- roc(dataClassProb_split[[i]]$Class,dataClassProb_split[[i]]$predictions_ensemble_prob    )
ciauc_LL[[i]] <- ci.auc(rocobj[[i]])[1] 
ciauc[[i]] <- ci.auc(rocobj[[i]])[2] 
ciauc_UL[[i]] <- ci.auc(rocobj[[i]])[3] 
}

 
ciauc_LL_all <- unsplit(ciauc_LL,f=unique(dataClassProb$FailedRobot))
ciauc_all <- unsplit(ciauc,f=unique(dataClassProb$FailedRobot))
ciauc_UL_all <- unsplit(ciauc_UL,f=unique(dataClassProb$FailedRobot))

PerformanceData <- cbind(PerformanceData,ciauc_LL_all, ciauc_all,ciauc_UL_all)
##Forest Plots
type <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11")
PerformanceData$RobotList <- factor(PerformanceData$RobotList, c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"))
ggplot(data=PerformanceData,aes(x=ciauc_all,y=RobotList))+
  geom_point(aes(fill=type), colour="black",shape=21)+
  geom_errorbarh(aes(xmin=ciauc_LL_all,xmax=ciauc_UL_all),height=0.2,colour="blue")+
  geom_vline(xintercept=0.5,linetype="dashed")+
  xlab("AUC") + ylab("Robot List")+
  theme(legend.position="none") 

ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/AUC_CI.pdf", width = 20, height = 20, units = "cm")

 roc.test(rocobj[[1]],rocobj[[2]])
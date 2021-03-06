rm(list=ls())
## Random forest without PCA preprocessing

ptm <- proc.time()
#Load Data
noSamples <- 121
dayList <- c(21,19, 17,12 ) 
#for ( lastDay in dayList){  
   lastDay <- 12 # 21, 19,17,12
  rollingWindow <- 10
  
  dbf <-  22-lastDay
  
  # 1 For Signal Type dist ( Raw); 2 for diff signal; 3 for both combined
  
  #SigTypeInt <- 2
  
  ## Set flag for dropping calucalted Torque and speed
  drop_act <- 1 # 1 for True and   Not for other values
  drop_KL <- 0 # 1 for TRue and not for other values
  drop_SISAVg <- 0 # for TRue and not for other values
  
  setwd("C:/IIITD/WIP/Data/GearboxFailure/Journal/8_Data_Classification")
  library(xlsx)
  drop <- c("filename")
  #if (SigTypeInt==1) {
  
  data_F_ST1 <- read.xlsx("Data_classification.xlsx", sheetName="dist_data_F", header=TRUE)
  data_F_ST1  <- data_F_ST1[,-1]
  data_F_ST1  <- data_F_ST1[,!(names(data_F_ST1) %in% drop)]
  
  data_N_ST1 <- read.xlsx("Data_classification.xlsx", sheetName="dist_data_N", header=TRUE)
  data_N_ST1 <- data_N_ST1[,-1]
  data_N_ST1 <- data_N_ST1[,!(names(data_N_ST1) %in% drop)]
  
  #} else  if (SigTypeInt==2) 
  
  #{
  data_F_ST2 <- read.xlsx("Data_classification.xlsx", sheetName="diff_data_F", header=TRUE)
  data_F_ST2 <- data_F_ST2[,-1]
  data_F_ST2 <- data_F_ST2[,!(names(data_F_ST2) %in% drop)]
  
  data_N_ST2 <- read.xlsx("Data_classification.xlsx", sheetName="diff_data_N", header=TRUE)
  data_N_ST2 <- data_N_ST2[,-1]
  data_N_ST2 <- data_N_ST2[,!(names(data_N_ST2) %in% drop)]
   
  
  if (drop_act ==1) { 
    data_F_ST1 <- data_F_ST1[, !grepl("act",colnames(data_F_ST1))] 
    data_N_ST1 <- data_N_ST1[, !grepl("act",colnames(data_N_ST1))] 
    
    data_F_ST2 <- data_F_ST1[, !grepl("act",colnames(data_F_ST1))] 
    data_N_ST2 <- data_N_ST1[, !grepl("act",colnames(data_N_ST1))] 
    
  }
  
  
  ##Select Random controllers 
  
  set.seed(1000)
 
  t1_1 <- rep(seq(1:11),11) 
  sample_data_F_ST1_Trg<- list()
  sample_data_F_ST2_Trg<- list()
  set.seed(1000)
  for (i in 1:noSamples){
    sample_data_F_ST1_Trg[[i]]<- subset(data_F_ST1, controllerID !=t1_1[i] )
    sample_data_F_ST2_Trg[[i]]<- subset(data_F_ST2, controllerID !=t1_1[i] )
    #sample_data_F_Trg[[i]]<- subset(data_F_T1, controllerID !=t1_1[i] )
    
    #Consider only Rolling window Data
    sample_data_F_ST1_Trg[[i]] <- sample_data_F_ST1_Trg[[i]][which(sample_data_F_ST1_Trg[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_F_ST1_Trg[[i]]$SeqNo <= lastDay),   ]
    sample_data_F_ST2_Trg[[i]] <- sample_data_F_ST2_Trg[[i]][which(sample_data_F_ST2_Trg[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_F_ST2_Trg[[i]]$SeqNo <= lastDay),   ]
    
  }
  
  sample_data_F_ST1_Test<- list()
  sample_data_F_ST2_Test<- list()
  set.seed(1000)
  for (i in 1:noSamples){
    sample_data_F_ST1_Test[[i]]<- subset(data_F_ST1, controllerID ==t1_1[i]  )
    sample_data_F_ST2_Test[[i]]<- subset(data_F_ST2, controllerID ==t1_1[i]  )
    #sample_data_F_Test[[i]]<- subset(data_F_T1, controllerID ==t1_1[i]  )  
    #Consider only Rolling window Data
    sample_data_F_ST1_Test[[i]] <- sample_data_F_ST1_Test[[i]][which(sample_data_F_ST1_Test[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_F_ST1_Test[[i]]$SeqNo <= lastDay),   ]
    sample_data_F_ST2_Test[[i]] <- sample_data_F_ST2_Test[[i]][which(sample_data_F_ST2_Test[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_F_ST2_Test[[i]]$SeqNo <= lastDay),   ]
    
    
  }
  
  
  
  # set.seed(2000)
  # t2<- replicate(11,sample(1:11,3,replace = F))
  # t2_1<- sample(1:11,11,replace = F)
  #t2_1 <- rep(seq(1:11),11) 
  t2_1 <- rep(seq(1:11), each=11)
  sample_data_N_ST1_Trg<- list()
  sample_data_N_ST2_Trg<- list()
  
  set.seed(1000)
  for (i in 1:noSamples){
    sample_data_N_ST1_Trg[[i]]<- subset(data_N_ST1, controllerID !=t2_1[i] )
    sample_data_N_ST2_Trg[[i]]<- subset(data_N_ST2, controllerID !=t2_1[i] )
    #sample_data_N_Trg[[i]]<- subset(data_N_T1, controllerID !=t1_1[i] )
    
    #Consider only Rolling window Data
    sample_data_N_ST1_Trg[[i]] <- sample_data_N_ST1_Trg[[i]][which(sample_data_N_ST1_Trg[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_N_ST1_Trg[[i]]$SeqNo <= lastDay),   ]
    sample_data_N_ST2_Trg[[i]] <- sample_data_N_ST2_Trg[[i]][which(sample_data_N_ST2_Trg[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_N_ST2_Trg[[i]]$SeqNo <= lastDay),   ]
    
  }
  
  sample_data_N_ST1_Test<- list()
  sample_data_N_ST2_Test<- list()
  set.seed(1000)
  for (i in 1:noSamples){
    sample_data_N_ST1_Test[[i]]<- subset(data_N_ST1, controllerID ==t2_1[i] )
    sample_data_N_ST2_Test[[i]]<- subset(data_N_ST2, controllerID ==t2_1[i] )
    # sample_data_N_Test[[i]]<- subset(data_N_T1, controllerID ==t1_1[i] )
    
    #Consider only Rolling window Data
    sample_data_N_ST1_Test[[i]] <- sample_data_N_ST1_Test[[i]][which(sample_data_N_ST1_Test[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_N_ST1_Test[[i]]$SeqNo <= lastDay),   ]
    sample_data_N_ST2_Test[[i]] <- sample_data_N_ST2_Test[[i]][which(sample_data_N_ST2_Test[[i]]$SeqNo >= (lastDay - rollingWindow) & sample_data_N_ST2_Test[[i]]$SeqNo <= lastDay),   ]
    
  }
  
  sample_data_ST1_Train <- list()
  sample_data_ST2_Train <- list()
  
  for (i  in 1:noSamples){
    
    sample_data_ST1_Train[[i]] <- rbind(sample_data_F_ST1_Trg[[i]],sample_data_N_ST1_Trg[[i]])
    sample_data_ST2_Train[[i]] <- rbind(sample_data_F_ST2_Trg[[i]],sample_data_N_ST2_Trg[[i]])
  }
  
  sample_data_ST1_Test <- list()
  sample_data_ST2_Test <- list()
  for (i  in 1:noSamples){
    
    sample_data_ST1_Test[[i]] <- rbind(sample_data_F_ST1_Test[[i]],sample_data_N_ST1_Test[[i]])
    sample_data_ST2_Test[[i]] <- rbind(sample_data_F_ST2_Test[[i]],sample_data_N_ST2_Test[[i]])
  }
  
  
  
  drops <- c("controllerID")
  sample_data_ST1_Train_1 <- list()
  sample_data_ST1_Test_1 <- list()
  sample_data_ST2_Train_1 <- list()
  sample_data_ST2_Test_1 <- list()
  for (i in 1:noSamples){
    sample_data_ST1_Train_1[[i]] <- sample_data_ST1_Train[[i]][ , !(names(sample_data_ST1_Train[[i]]) %in% drops)]
    sample_data_ST1_Test_1[[i]] <- sample_data_ST1_Test[[i]][ , !(names(sample_data_ST1_Test[[i]]) %in% drops)]
    
    sample_data_ST2_Train_1[[i]] <- sample_data_ST2_Train[[i]][ , !(names(sample_data_ST2_Train[[i]]) %in% drops)]
    sample_data_ST2_Test_1[[i]] <- sample_data_ST2_Test[[i]][ , !(names(sample_data_ST2_Test[[i]]) %in% drops)]
  }
  
  
  
  #Randomize rows in the dataframe
  sample_data_ST1_Train_2 <- list()
  sample_data_ST1_Test_2 <- list()
  
  sample_data_ST2_Train_2 <- list()
  sample_data_ST2_Test_2 <- list()
  drop <- c("SeqNo")
  
  for (i in 1:noSamples) {
    sample_data_ST1_Train_1[[i]] <- sample_data_ST1_Train_1[[i]][sample(1:nrow(sample_data_ST1_Train_1[[i]])), ]
    sample_data_ST1_Train_2[[i]] <- sample_data_ST1_Train_1[[i]][,!(names(sample_data_ST1_Train_1[[i]]) %in% drop) ]
    sample_data_ST1_Test_1[[i]] <- sample_data_ST1_Test_1[[i]][sample(1:nrow(sample_data_ST1_Test_1[[i]])), ]
    sample_data_ST1_Test_2[[i]] <- sample_data_ST1_Test_1[[i]][,!(names(sample_data_ST1_Test_1[[i]]) %in% drop) ]
    
    
    sample_data_ST2_Train_1[[i]] <- sample_data_ST2_Train_1[[i]][sample(1:nrow(sample_data_ST2_Train_1[[i]])), ]
    sample_data_ST2_Train_2[[i]] <- sample_data_ST2_Train_1[[i]][,!(names(sample_data_ST2_Train_1[[i]]) %in% drop) ]
    sample_data_ST2_Test_1[[i]] <- sample_data_ST2_Test_1[[i]][sample(1:nrow(sample_data_ST2_Test_1[[i]])), ]
    sample_data_ST2_Test_2[[i]] <- sample_data_ST2_Test_1[[i]][,!(names(sample_data_ST2_Test_1[[i]]) %in% drop) ]
    
  }
  
  
  
  
## For RF class  changed to numeric
for (i in 1:noSamples){
  sample_data_ST1_Train_2[[i]]$Class <- ifelse(sample_data_ST1_Train_2[[i]]$Class=="F",1,0)
  sample_data_ST1_Test_2[[i]]$Class <- ifelse(sample_data_ST1_Test_2[[i]]$Class=="F",1,0)
  sample_data_ST2_Train_2[[i]]$Class <- ifelse(sample_data_ST2_Train_2[[i]]$Class=="F",1,0)
  sample_data_ST2_Test_2[[i]]$Class <- ifelse(sample_data_ST2_Test_2[[i]]$Class=="F",1,0)
}

# Model Development

library(caret)
  rfModel_ST1 <- list()
  rfModel_ST2 <- list()

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE)
for (i in 1:noSamples ){
  # train a logistic regression model
 
  rfModel_ST1[[i]] <- train(Class~.,data =  sample_data_ST1_Train_2[[i]],method="rf",  ntree=75, importance = TRUE, preProc = c("center", "scale"),tuneLength = 9, trControl=ctrl )
  rfModel_ST2[[i]] <- train(Class~.,data =  sample_data_ST2_Train_2[[i]],method="rf",  ntree=75, importance = TRUE, preProc = c("center", "scale"),tuneLength = 9, trControl=ctrl )
  
}

  
## Save variable Importance
df_varImp_ST1 <-   varImp(rfModel_ST1[[i]]$finalModel) 
df_varImp_ST1<- cbind(Measurement = rownames(df_varImp_ST1), df_varImp_ST1) 
df_varImp_ST1$ID <- 1 

df_varImp_ST2 <-   varImp(rfModel_ST2[[i]]$finalModel) 
df_varImp_ST2<- cbind(Measurement = rownames(df_varImp_ST2), df_varImp_ST2) 
df_varImp_ST2$ID <- 1 

for (i in 2:noSamples){
  temp_ST1<- varImp(rfModel_ST1[[i]]$finalModel)
  temp_ST1<- cbind(Measurement = rownames(temp_ST1), temp_ST1) 
  temp_ST1$ID <- i
  df_varImp_ST1 <- rbind(df_varImp_ST1, temp_ST1 )
  
  temp_ST2<- varImp(rfModel_ST2[[i]]$finalModel)
  temp_ST2<- cbind(Measurement = rownames(temp_ST2), temp_ST2) 
  temp_ST2$ID <- i
  df_varImp_ST2 <- rbind(df_varImp_ST2, temp_ST2 )
  
}
row.names(df_varImp_ST1) <- 1:nrow(df_varImp_ST1)
row.names(df_varImp_ST2) <- 1:nrow(df_varImp_ST2)

## Write prediction prob and actual to csv
fileVarImp_ST1 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","VarImportance","Signal_Type_1", "_dbf_",dbf,"_RF.csv")
fileVarImp_ST2 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","VarImportance","Signal_Type_2", "_dbf_",dbf,"RF.csv")


df_varImp_ST1 <- data.frame(df_varImp_ST1)
write.table(df_varImp_ST1,fileVarImp_ST1,sep = ",", append = T,col.names = T)
df_varImp_ST2 <- data.frame(df_varImp_ST2)
write.table(df_varImp_ST2,fileVarImp_ST2,sep = ",", append = T,col.names = T)
 
 

## Make predictions

x_test_ST1 <- list()
y_test_ST1 <- list()
predictions_ST1 <- list()
predictions_prob_ST1 <- list()
cm_ST1 <- list()
misClasificError_ST1 <- list()
Accuracy_ST1 <- list()

x_test_ST2 <- list()
y_test_ST2 <- list()
predictions_ST2 <- list()
predictions_prob_ST2 <- list()
cm_ST2 <- list()
misClasificError_ST2 <- list()
Accuracy_ST2 <- list()


for (i in 1: noSamples ){
  x_last_ST1 <-  ncol(sample_data_ST1_Test_2[[i]])-1
  y_last_ST1 <-  ncol(sample_data_ST1_Test_2[[i]])
  x_test_ST1[[i]] <- sample_data_ST1_Test_2[[i]][,1:x_last_ST1]
  y_test_ST1[[i]] <- sample_data_ST1_Test_2[[i]][,y_last_ST1]
  
  predictions_ST1[[i]] <- predict(rfModel_ST1[[i]], x_test_ST1[[i]] )
  predictions_prob_ST1[[i]] <-  predictions_ST1[[i]]
  predictions_ST1[[i]] <- ifelse(predictions_ST1[[i]] > 0.5,1,0)
  #predictions_prob_ST1[[i]] <- predict(glmModel_ST1[[i]], x_test_ST1[[i]] ,type="prob",probability = TRUE)
  cm_ST1[[i]] <- confusionMatrix(predictions_ST1[[i]] , as.factor(y_test_ST1[[i]]))
  misClasificError_ST1[[i]] <- mean(predictions_ST1[[i]] != y_test_ST1[[i]])
  Accuracy_ST1[[i]] <- 1 - misClasificError_ST1[[i]]
  
  
  x_last_ST2 <-  ncol(sample_data_ST2_Test_2[[i]])-1
  y_last_ST2 <-  ncol(sample_data_ST2_Test_2[[i]])
  x_test_ST2[[i]] <- sample_data_ST2_Test_2[[i]][,1:x_last_ST2]
  y_test_ST2[[i]] <- sample_data_ST2_Test_2[[i]][,y_last_ST2]
  predictions_ST2[[i]] <- predict(rfModel_ST2[[i]], x_test_ST2[[i]] )
  predictions_prob_ST2[[i]] <-  predictions_ST2[[i]]
  predictions_ST2[[i]] <- ifelse(predictions_ST2[[i]] > 0.5,1,0)
  #predictions_prob_ST2[[i]] <- predict(glmModel_ST2[[i]], x_test_ST2[[i]] ,type="prob",probability = TRUE)
  cm_ST2[[i]] <- confusionMatrix(predictions_ST2[[i]] , as.factor(y_test_ST2[[i]]))
  misClasificError_ST2[[i]] <- mean(predictions_ST2[[i]] != y_test_ST2[[i]])
  Accuracy_ST2[[i]] <- 1 - misClasificError_ST2[[i]]
  
}

mean(sapply(Accuracy_ST1,mean))
mean(sapply(Accuracy_ST2,mean))





## Write prediction prob and actual to csv
filePredAct <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","Pred_Act", "_dbf_",dbf,"_RF.csv")


pred_act <- data.frame(cbind(1,y_test_ST2[[1]],predictions_prob_ST1[[1]],predictions_prob_ST2[[1]]))
write.table(pred_act,filePredAct,sep = ",", append = T,col.names = T)

for (i in 2:noSamples){
  pred_act <- data.frame(cbind(i, y_test_ST2[[i]],predictions_prob_ST1[[i]],predictions_prob_ST2[[i]]))
  write.table(pred_act,filePredAct, sep = ",",append = T,col.names = F)
}



## Write the cm to csv

fileCM_ST1 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","CM_","SigType_1","_dbf_",dbf,"_RF.csv")
fileCM_ST2 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","CM_","SigType_2","_dbf_",dbf,"_RF.csv")

write.table(cm_ST1[[1]]$table,fileCM_ST1,sep = "," )
write.table(cm_ST2[[1]]$table,fileCM_ST2,sep = "," )
for (i in 2:noSamples){
  write.table(cm_ST1[[i]]$table,fileCM_ST1, sep = ",",append = T)
  write.table(cm_ST2[[i]]$table,fileCM_ST2, sep = ",",append = T)  
}

tocsv_ST1 <- data.frame(cbind(t(cm_ST1[[1]]$overall),t(cm_ST1[[1]]$byClass)))
tocsv_ST2 <- data.frame(cbind(t(cm_ST2[[1]]$overall),t(cm_ST2[[1]]$byClass)))
write.table(tocsv_ST1,fileCM_ST1,sep = ",", append = T,col.names = T)
write.table(tocsv_ST2,fileCM_ST2,sep = ",", append = T,col.names = T)

for (i in 2:noSamples){
  tocsv_ST1 <- data.frame(cbind(t(cm_ST1[[i]]$overall),t(cm_ST1[[i]]$byClass)))
  tocsv_ST2 <- data.frame(cbind(t(cm_ST2[[i]]$overall),t(cm_ST2[[i]]$byClass)))
  write.table(tocsv_ST1,fileCM_ST1, sep = ",",append = T,col.names = F)
  write.table(tocsv_ST2,fileCM_ST2, sep = ",",append = T,col.names = F)
}





p_ST1 <- list()
pr_ST1 <- list()
prf_ST1 <- list()


p_ST2 <- list()
pr_ST2 <- list()
prf_ST2 <- list()

library(ROCR)
for (i in 1 : noSamples) {
  p_ST1[[i]] <- predict(rfModel_ST1[[i]], newdata=x_test_ST1[[i]])
  #p_ST1[[i]]  <- ifelse( p_ST1[[i]]<0.5,0,1)
  pr_ST1[[i]] <- prediction(  p_ST1[[i]],y_test_ST1[[i]] ) # Reveresed the polarity changed to N
  prf_ST1[[i]] <- performance(pr_ST1[[i]], measure = "tpr", x.measure = "fpr")
  
  p_ST2[[i]] <- predict(rfModel_ST2[[i]], newdata=x_test_ST2[[i]])
  pr_ST2[[i]] <- prediction(  p_ST2[[i]],y_test_ST2[[i]] ) # Reveresed the polarity changed to N
  prf_ST2[[i]] <- performance(pr_ST2[[i]], measure = "tpr", x.measure = "fpr")
  
}

###Create a complete dataframe from the list - FPR and TPR (ROC curve)

tpr_fpr_ST1 <- as.data.frame(prf_ST1[[1]]@y.values)
names(tpr_fpr_ST1) <- "TPRate_ST1"
fpr_ST1 <- as.data.frame(prf_ST1[[1]]@x.values)
names(fpr_ST1) <- "FPRate_ST1"
tpr_fpr_ST1<- cbind(tpr_fpr_ST1, fpr_ST1)
tpr_fpr_ST1$testno <- 1
tpr_fpr_ST1$SeqNo <- 1:nrow(tpr_fpr_ST1)

tpr_fpr_ST2 <- as.data.frame(prf_ST2[[1]]@y.values)
names(tpr_fpr_ST2) <- "TPRate_ST2"
fpr_ST2 <- as.data.frame(prf_ST2[[1]]@x.values)
names(fpr_ST2) <- "FPRate_ST2"
tpr_fpr_ST2<- cbind(tpr_fpr_ST2, fpr_ST2)
tpr_fpr_ST2$testno <- 1
tpr_fpr_ST2$SeqNo <- 1:nrow(tpr_fpr_ST2)


for (i in 2:noSamples){
  temp1_ST1 <- as.data.frame(prf_ST1[[i]]@y.values)
  names(temp1_ST1) <- "TPRate_ST1"
  temp2_ST1 <-  as.data.frame(prf_ST1[[i]]@x.values)
  names(temp2_ST1) <- "FPRate_ST1"
  temp3_ST1 <- cbind(temp1_ST1,temp2_ST1)
  temp3_ST1$testno <- i
  temp3_ST1$SeqNo <- 1:nrow(temp2_ST1)
  tpr_fpr_ST1<- rbind(tpr_fpr_ST1, temp3_ST1)
  
  
  temp1_ST2 <- as.data.frame(prf_ST2[[i]]@y.values)
  names(temp1_ST2) <- "TPRate_ST2"
  temp2_ST2 <-  as.data.frame(prf_ST2[[i]]@x.values)
  names(temp2_ST2) <- "FPRate_ST2"
  temp3_ST2 <- cbind(temp1_ST2,temp2_ST2)
  temp3_ST2$testno <- i
  temp3_ST2$SeqNo <- 1:nrow(temp2_ST2)
  tpr_fpr_ST2 <- rbind(tpr_fpr_ST2, temp3_ST2)
}


##Copy auc 
auc_ST1 <- list()
auc_ST2 <- list()
for (i in 1:noSamples){ 
  auc_ST1[[i]] <- performance(pr_ST1[[i]], measure = "auc")
  auc_ST2[[i]] <- performance(pr_ST2[[i]], measure = "auc")
}

auc_y_ST1 <- list() 
auc_y_ST2  <- list() 
for (i in 1:noSamples){
  auc_y_ST1[[i]]<- auc_ST1[[i]]@y.values[[1]]
  auc_y_ST2[[i]]<- auc_ST2[[i]]@y.values[[1]]
}


aucFinal_ST1 <- as.data.frame( t(as.data.frame(auc_y_ST1)))
names(aucFinal_ST1) <- "auc"
aucFinal_ST1$testno <- seq(1:noSamples)

aucFinal_ST2 <- as.data.frame( t(as.data.frame(auc_y_ST2)))
names(aucFinal_ST2) <- "auc"
aucFinal_ST2$testno <- seq(1:noSamples)

tpr_fpr_ST1 <- merge(tpr_fpr_ST1,aucFinal_ST1, by = "testno")
labs_ST1 <- round(aucFinal_ST1$auc,2)
names(labs_ST1) <- 'auc'
tpr_fpr_ST2 <- merge(tpr_fpr_ST2,aucFinal_ST2, by = "testno")
labs_ST2 <- round(aucFinal_ST2$auc,2)
names(labs_ST2) <- 'auc'

##plot and save as pdf
filename1 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","ROC_Curve_dbf_",dbf,"_SigType_1", "_RF.pdf")
a <- ggplot(tpr_fpr_ST1, aes(FPRate_ST1,TPRate_ST1)) +facet_wrap(~testno,ncol=11)
a<- a+  geom_line() + theme(axis.text.x = element_text(size = 8,angle = 90))
a+ annotate("text", x = 0.2, y = .8, label =paste("auc =", labs_ST1),fontface =2)
ggsave(filename1,width = 7, height = 7)

filename2 <- paste0("C:/IIITD/WIP/Analysis/Journal/Figures/","ROC_Curve_dbf_",dbf,"_SigType_2", "_RF.pdf")
a <- ggplot(tpr_fpr_ST2, aes(FPRate_ST2,TPRate_ST2)) +facet_wrap(~testno,ncol=11)
a<- a+  geom_line() + theme(axis.text.x = element_text(size = 8,angle = 90))
a+ annotate("text", x = 0.2, y = .8, label =paste("auc =", labs_ST2),fontface =1)
ggsave(filename2,width = 7, height = 7)
#}
proc.time() - ptm
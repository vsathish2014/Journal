rm(list=ls())
 
ptm <- proc.time()
#Load Data
dayList <- c(21,19,17,12 ) 
pca_pct <- 0.90
noSamples <- 121
#for ( lastDay in dayList){  
  lastDay <- 17 # 21, 19,17,12
  rollingWindow <- 10
  
  dbf <-  22-lastDay
  
  setwd("C:/IIITD/WIP/Analysis/Journal/Journal")
  
  locVar <- 0 # 3 for onsidering local variation 3 days
  
  if (locVar==0){
  #for 3 days data for Local variartion
  source("LocVarClassData0.R") 
    
  } else if (locVar ==3){
  #for 3 days data for Local variartion
     source("LocVarClassData3.R") 
    data_F_ST2 <- data_F_ST2_c
    data_N_ST2 <- data_N_ST2_c
  } else {
  
  #for 5 days data for Local variartion
  #source("LocVarClassData5.R")
    data_F_ST2 <- data_F_ST2_c
    data_N_ST2 <- data_N_ST2_c
  }

  
 
 
  set.seed(1000)
 
 # t1_1<-  sample(1:noSamples,11,replace = F) 
  t1_1 <- rep(seq(1:11), each=11)
 
  sample_data_F_ST2_Trg<- list()
  set.seed(1000)
  for (i in 1:noSamples){
 
    sample_data_F_ST2_Trg[[i]]<- subset(data_F_ST2, controllerID !=t1_1[i] )
 
    
    #Consider only Rolling window Data
     sample_data_F_ST2_Trg[[i]] <- sample_data_F_ST2_Trg[[i]][which(sample_data_F_ST2_Trg[[i]]$SeqNo > (lastDay - rollingWindow) & sample_data_F_ST2_Trg[[i]]$SeqNo <= lastDay),   ]
    
  }
  
  
  sample_data_F_ST2_Test<- list()
  set.seed(1000)
  for (i in 1:noSamples){
   
    sample_data_F_ST2_Test[[i]]<- subset(data_F_ST2, controllerID ==t1_1[i]  )
      sample_data_F_ST2_Test[[i]] <- sample_data_F_ST2_Test[[i]][which(sample_data_F_ST2_Test[[i]]$SeqNo > (lastDay - rollingWindow) & sample_data_F_ST2_Test[[i]]$SeqNo <= lastDay),   ]
    
    
  }
  
  
  
  # set.seed(2000)
  # t2<- replicate(11,sample(1:11,3,replace = F))
  # t2_1<- sample(1:11,11,replace = F)
  #t2_1 <- rep(seq(1:11),11) 
  t2_1 <- rep(seq(1:11),11) 

    sample_data_N_ST2_Trg<- list()
  
  set.seed(1000)
  for (i in 1:noSamples){
      sample_data_N_ST2_Trg[[i]]<- subset(data_N_ST2, controllerID !=t2_1[i] )
    #sample_data_N_Trg[[i]]<- subset(data_N_T1, controllerID !=t1_1[i] )
    
    #Consider only Rolling window Data
      sample_data_N_ST2_Trg[[i]] <- sample_data_N_ST2_Trg[[i]][which(sample_data_N_ST2_Trg[[i]]$SeqNo > (lastDay - rollingWindow) & sample_data_N_ST2_Trg[[i]]$SeqNo <= lastDay),   ]
    
  }
  
   sample_data_N_ST2_Test<- list()
  set.seed(1000)
  for (i in 1:noSamples){
     sample_data_N_ST2_Test[[i]]<- subset(data_N_ST2, controllerID ==t2_1[i] )
    # sample_data_N_Test[[i]]<- subset(data_N_T1, controllerID ==t1_1[i] )
    
     sample_data_N_ST2_Test[[i]] <- sample_data_N_ST2_Test[[i]][which(sample_data_N_ST2_Test[[i]]$SeqNo > (lastDay - rollingWindow) & sample_data_N_ST2_Test[[i]]$SeqNo <= lastDay),   ]
    
  }
  
    sample_data_ST2_Train <- list()
  
  for (i  in 1:noSamples){
    
      sample_data_ST2_Train[[i]] <- rbind(sample_data_F_ST2_Trg[[i]],sample_data_N_ST2_Trg[[i]])
  }
  
   sample_data_ST2_Test <- list()
  for (i  in 1:noSamples){
    
     sample_data_ST2_Test[[i]] <- rbind(sample_data_F_ST2_Test[[i]],sample_data_N_ST2_Test[[i]])
  }
  
  
  
  drops <- c("controllerID")
   sample_data_ST2_Train_1 <- list()
  sample_data_ST2_Test_1 <- list()
  for (i in 1:noSamples){
     
    sample_data_ST2_Train_1[[i]] <- sample_data_ST2_Train[[i]][ , !(names(sample_data_ST2_Train[[i]]) %in% drops)]
    sample_data_ST2_Test_1[[i]] <- sample_data_ST2_Test[[i]][ , !(names(sample_data_ST2_Test[[i]]) %in% drops)]
  }
  
  
  
  #No randomization
  
  sample_data_ST2_Train_2 <- list()
  sample_data_ST2_Test_2 <- list()
  drop <- c("SeqNo")
  
  for (i in 1:noSamples) {
 
    
    #sample_data_ST2_Train_1[[i]] <- sample_data_ST2_Train_1[[i]][sample(1:nrow(sample_data_ST2_Train_1[[i]])), ]
    sample_data_ST2_Train_2[[i]] <- sample_data_ST2_Train_1[[i]][,!(names(sample_data_ST2_Train_1[[i]]) %in% drop) ]
    #sample_data_ST2_Test_1[[i]] <- sample_data_ST2_Test_1[[i]][sample(1:nrow(sample_data_ST2_Test_1[[i]])), ]
    sample_data_ST2_Test_2[[i]] <- sample_data_ST2_Test_1[[i]][,!(names(sample_data_ST2_Test_1[[i]]) %in% drop) ]
    
  }
  
  
  
  ## PCA preproessing
  library(caret)
  movetolast <- function(data, move) {
    data[c(setdiff(names(data), move), move)]
  }
  
  
 
  sample_data_ST2_Train_3 <- list()
  sample_data_ST2_Test_3 <- list()
  sample_data_ST2_Train_4 <- list()
  sample_data_ST2_Test_4 <- list()
  
  for (i  in 1:noSamples) {
    sample_data_ST2_Train_3[[i]] <- sample_data_ST2_Train_2[[i]]
    sample_data_ST2_Train_3[[i]] <- sample_data_ST2_Train_3[[i]][,sapply(sample_data_ST2_Train_3[[i]] , function(v) var(v, na.rm=TRUE)!=0)]
    preprocessParams_ST2 <- preProcess(sample_data_ST2_Train_3[[i]], method=c("center", "scale", "pca"),pcaComp = 20)
    sample_data_ST2_Train_4[[i]] <- predict(preprocessParams_ST2 , sample_data_ST2_Train_3[[i]])
    sample_data_ST2_Train_4[[i]] <- movetolast(sample_data_ST2_Train_4[[i]], c("Class"))
    sample_data_ST2_Test_3[[i]] <- sample_data_ST2_Test_2[[i]]
    #sample_data_ST2_Test_3[[i]] <- sample_data_ST2_Test_3[[i]][,sapply(sample_data_ST2_Test_3[[i]] , function(v) var(v, na.rm=TRUE)!=0)]
    sample_data_ST2_Test_4[[i]] <- predict(preprocessParams_ST2 , sample_data_ST2_Test_3[[i]])
    sample_data_ST2_Test_4[[i]] <- movetolast(sample_data_ST2_Test_4[[i]], c("Class"))
    sample_data_ST2_Test_4[[i]] <- sample_data_ST2_Test_4[[i]][grepl("PC|Class", names(sample_data_ST2_Test_4[[i]]))]
  }
  
 
# Model Development

library(caret)
 
 

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     savePredictions="final",
                     summaryFunction=twoClassSummary,
                     classProbs = TRUE)
 

library("caret")
library("mlbench")
library("pROC")

library("rpart")
library("caretEnsemble")
model_list <- list()
for (i in 1: noSamples) { 
model_list[[i]] <- caretList(
  Class~., data=sample_data_ST2_Train_4[[i]],
  trControl=ctrl,
  #methodList=c( "LogitBoost","cforest","svmRadial")
  methodList=c( "LogitBoost" ,"svmRadial")
)
}
 
# greedy_ensemble <- list()
# for (i in  1: noSamples)
# greedy_ensemble <- caretEnsemble(
#   model_list[[i]], 
#   metric="ROC",
#   trControl=trainControl(
#     number=2,
#     summaryFunction=twoClassSummary,
#     classProbs=TRUE
#   ))
# 
# summary(greedy_ensemble)

glm_ensemble <- list()
for (i in 1: noSamples) { 
glm_ensemble[[i]] <- caretStack(
  model_list[[i]],
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
}

x_test_ST2 <- list()
y_test_ST2 <- list()
model_preds <- list()

for (i in 1: noSamples ){ 
x_last_ST2 <-  ncol(sample_data_ST2_Test_4[[i]])-1
y_last_ST2 <-  ncol(sample_data_ST2_Test_4[[i]])
x_test_ST2[[i]] <- sample_data_ST2_Test_4[[i]][,1:x_last_ST2]
y_test_ST2[[i]] <- sample_data_ST2_Test_4[[i]][,y_last_ST2]
 
library("caTools")
model_preds[[i]] <- lapply(model_list[[i]], predict, newdata=sample_data_ST2_Test_4[[i]], type="prob")
 
#model_preds[[i]]$rpart  <- lapply(list(model_preds[[i]]$cforest) ,  function(x) x[,"F"])
model_preds[[i]]$LogitBoost  <- lapply(list(model_preds[[i]]$LogitBoost) ,  function(x) x[,"F"])
model_preds[[i]]$svmRadial  <- lapply(list(model_preds[[i]]$svmRadial) ,  function(x) x[,"F"])
model_preds[[i]] <- data.frame(model_preds[[i]])

names(model_preds[[i]]) <-c( "LogitBoost" ,"svmRadial")
}
 
model_preds2 <- list()
model_preds2_woP<- list()
accuracy <- list()
for (i in 1: noSamples) {
model_preds2[[i]] <- model_preds[[i]]
model_preds2_woP[[i]] <- model_preds[[i]]
model_preds2[[i]]$ensemble <- predict(glm_ensemble[[i]], newdata=x_test_ST2[[i]], type="prob")
model_preds2_woP[[i]]$ensemble <- predict(glm_ensemble[[i]], newdata=x_test_ST2[[i]] )

CF <- coef(glm_ensemble[[i]]$ens_model$finalModel)[-1]
accuracy[[i]] <- colAUC(model_preds2[[i]], y_test_ST2[[i]])
}

cm_ST2 <- list()
misClasificError_ST2 <- list()
Accuracy_ST2 <- list()
for (i in 1: noSamples) {
cm_ST2[[i]] <- confusionMatrix(model_preds2_woP[[i]]$ensemble , as.factor(y_test_ST2[[i]]))
misClasificError_ST2[[i]] <- mean(model_preds2_woP[[i]]$ensemble != y_test_ST2[[i]])
Accuracy_ST2[[i]] <- 1 - misClasificError_ST2[[i]]
}
# model_preds3 <- list()
# accuracy_Greedy <- list()
# for (i in 1: noSamples) {
#   model_preds3[[i]] <- model_preds[[i]]
#   model_preds3[[i]]$ensemble <- predict(greedy_ensemble[[i]], newdata=x_test_ST2[[i]], type="prob")
#   CF_G <- coef(glm_ensemble[[i]]$ens_model$finalModel)[-1]
#   accuracy_Greedy[[i]] <- colAUC(model_preds2[[i]], y_test_ST2[[i]])
# }


 
library(plyr)
setwd("C:/IIITD/WIP/Analysis/Journal/TestResults")
model_preds2_all <- ldply(model_preds2, data.frame)
y_test_ST2_all <- ldply(y_test_ST2, data.frame)
model_preds2_all <-cbind( model_preds2_all, y_test_ST2_all)


model_preds2_woP_all <- ldply(model_preds2_woP, data.frame)
y_test_ST2_all <- ldply(y_test_ST2, data.frame)
model_preds2_woP_all <-cbind( model_preds2_woP_all, y_test_ST2_all)

accuracy_all <- ldply(accuracy, data.frame)


write.csv(accuracy_all,"C:/IIITD/WIP/Analysis/Journal/TestResults/accuracy_all_SigType2_PCA_locvar0_noRandom.csv" )

write.csv(model_preds2_all,"C:/IIITD/WIP/Analysis/Journal/TestResults/model_preds2_all_SigType2_PCA_locvar0_noRandom.csv" )

write.csv(model_preds2_woP_all,"C:/IIITD/WIP/Analysis/Journal/TestResults/model_preds2_woP_all_SigType2_PCA_locvar0_noRandom.csv" )

 
proc.time() - ptm
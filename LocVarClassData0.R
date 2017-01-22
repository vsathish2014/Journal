 
 

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
 
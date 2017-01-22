 
### Working
shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )

 

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

col_idx <- c(49,50,51)


data_F_ST1_split <- split(data_F_ST1, f = data_F_ST1$controllerID)


shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )

 

data_F_ST1_prev <- list()
data_F_ST1_next <- list()
data_F_ST1_prev1 <- list()
data_F_ST1_next1 <- list()
data_F_ST1_c <- list()

for ( i in  1:length(data_F_ST1_split)){ 
  
  data_F_ST1_prev[[i]] <- shift(data_F_ST1_split[[i]],1)
  colnames(data_F_ST1_prev[[i]]) <- paste("Prev", colnames(data_F_ST1_prev[[i]]), sep = "_")
  data_F_ST1_prev[[i]] <- data_F_ST1_prev[[i]][,-c(49,50,51)]
  
  data_F_ST1_prev1[[i]] <- shift(data_F_ST1_split[[i]],2)
  colnames(data_F_ST1_prev1[[i]]) <- paste("Prev1", colnames(data_F_ST1_prev1[[i]]), sep = "_")
  data_F_ST1_prev1[[i]] <- data_F_ST1_prev1[[i]][,-c(49,50,51)]
  

  
  data_F_ST1_next[[i]] <- shift(data_F_ST1_split[[i]],-1)
  colnames( data_F_ST1_next[[i]] ) <- paste("Next", colnames( data_F_ST1_next[[i]] ), sep = "_")
  data_F_ST1_next[[i]] <- data_F_ST1_next[[i]][,-c(49,50,51)]
  
  data_F_ST1_next1[[i]] <- shift(data_F_ST1_split[[i]],-2)
  colnames( data_F_ST1_next1[[i]] ) <- paste("Next1", colnames( data_F_ST1_next1[[i]] ), sep = "_")
  data_F_ST1_next1[[i]] <- data_F_ST1_next1[[i]][,-c(49,50,51)]
  
  
  data_F_ST1_c[[i]] <- cbind(data_F_ST1_split[[i]],data_F_ST1_prev[[i]],data_F_ST1_next[[i]],data_F_ST1_prev1[[i]],data_F_ST1_next1[[i]])
  
  data_F_ST1_c[[i]] <- data_F_ST1_c[[i]][, c( (1:ncol(data_F_ST1_c[[i]]))[-col_idx],col_idx)]
  
}

data_F_ST1_c <- unsplit(data_F_ST1_c,f= data_F_ST1$controllerID)



data_F_ST2_split <- split(data_F_ST2, f = data_F_ST2$controllerID)

data_F_ST2_prev <- list()
data_F_ST2_next <- list()
data_F_ST2_prev1 <- list()
data_F_ST2_next1 <- list()
data_F_ST2_c <- list()

for ( i in  1:length(data_F_ST2_split)){ 
  
  data_F_ST2_prev[[i]] <- shift(data_F_ST2_split[[i]],1)
  colnames(data_F_ST2_prev[[i]]) <- paste("Prev", colnames(data_F_ST2_prev[[i]]), sep = "_")
  data_F_ST2_prev[[i]] <- data_F_ST2_prev[[i]][,-c(49,50,51)]
  
  data_F_ST2_prev1[[i]] <- shift(data_F_ST2_split[[i]],2)
  colnames(data_F_ST2_prev1[[i]]) <- paste("Prev1", colnames(data_F_ST2_prev1[[i]]), sep = "_")
  data_F_ST2_prev1[[i]] <- data_F_ST2_prev1[[i]][,-c(49,50,51)]
  
  
  
  data_F_ST2_next[[i]] <- shift(data_F_ST2_split[[i]],-1)
  colnames( data_F_ST2_next[[i]] ) <- paste("Next", colnames( data_F_ST2_next[[i]] ), sep = "_")
  data_F_ST2_next[[i]] <- data_F_ST2_next[[i]][,-c(49,50,51)]
  
  data_F_ST2_next1[[i]] <- shift(data_F_ST2_split[[i]],-2)
  colnames( data_F_ST2_next1[[i]] ) <- paste("Next1", colnames( data_F_ST2_next1[[i]] ), sep = "_")
  data_F_ST2_next1[[i]] <- data_F_ST2_next1[[i]][,-c(49,50,51)]
  
  
  data_F_ST2_c[[i]] <- cbind(data_F_ST2_split[[i]],data_F_ST2_prev[[i]],data_F_ST2_next[[i]],data_F_ST2_prev1[[i]],data_F_ST2_next1[[i]])
  
  data_F_ST2_c[[i]] <- data_F_ST2_c[[i]][, c( (1:ncol(data_F_ST2_c[[i]]))[-col_idx],col_idx)]
  
  
}

data_F_ST2_c <- unsplit(data_F_ST2_c,f= data_F_ST2$controllerID)


data_N_ST1_split <- split(data_N_ST1, f = data_N_ST1$controllerID)

data_N_ST1_prev <- list()
data_N_ST1_next <- list()
data_N_ST1_prev1 <- list()
data_N_ST1_next1 <- list()
data_N_ST1_c <- list()

for ( i in  1:length(data_N_ST1_split)){ 
  
  data_N_ST1_prev[[i]] <- shift(data_N_ST1_split[[i]],1)
  colnames(data_N_ST1_prev[[i]]) <- paste("Prev", colnames(data_N_ST1_prev[[i]]), sep = "_")
  data_N_ST1_prev[[i]] <- data_N_ST1_prev[[i]][,-c(49,50,51)]
  
  data_N_ST1_prev1[[i]] <- shift(data_N_ST1_split[[i]],2)
  colnames(data_N_ST1_prev1[[i]]) <- paste("Prev1", colnames(data_N_ST1_prev1[[i]]), sep = "_")
  data_N_ST1_prev1[[i]] <- data_N_ST1_prev1[[i]][,-c(49,50,51)]
  
  
  
  data_N_ST1_next[[i]] <- shift(data_N_ST1_split[[i]],-1)
  colnames( data_N_ST1_next[[i]] ) <- paste("Next", colnames( data_N_ST1_next[[i]] ), sep = "_")
  data_N_ST1_next[[i]] <- data_N_ST1_next[[i]][,-c(49,50,51)]
  
  data_N_ST1_next1[[i]] <- shift(data_N_ST1_split[[i]],-2)
  colnames( data_N_ST1_next1[[i]] ) <- paste("Next1", colnames( data_N_ST1_next1[[i]] ), sep = "_")
  data_N_ST1_next1[[i]] <- data_N_ST1_next1[[i]][,-c(49,50,51)]
  
  
  data_N_ST1_c[[i]] <- cbind(data_N_ST1_split[[i]],data_N_ST1_prev[[i]],data_N_ST1_next[[i]],data_N_ST1_prev1[[i]],data_N_ST1_next1[[i]])
  
  data_N_ST1_c[[i]] <- data_N_ST1_c[[i]][, c( (1:ncol(data_N_ST1_c[[i]]))[-col_idx],col_idx)]
  
  
}

data_N_ST1_c <- unsplit(data_N_ST1_c,f= data_N_ST1$controllerID)



data_N_ST2_split <- split(data_N_ST2, f = data_N_ST2$controllerID)

data_N_ST2_prev <- list()
data_N_ST2_next <- list()
data_N_ST2_prev1 <- list()
data_N_ST2_next1 <- list()
data_N_ST2_c <- list()

for ( i in  1:length(data_N_ST2_split)){ 
  
  data_N_ST2_prev[[i]] <- shift(data_N_ST2_split[[i]],1)
  colnames(data_N_ST2_prev[[i]]) <- paste("Prev", colnames(data_N_ST2_prev[[i]]), sep = "_")
  data_N_ST2_prev[[i]] <- data_N_ST2_prev[[i]][,-c(49,50,51)]
  
  data_N_ST2_prev1[[i]] <- shift(data_N_ST2_split[[i]],2)
  colnames(data_N_ST2_prev1[[i]]) <- paste("Prev1", colnames(data_N_ST2_prev1[[i]]), sep = "_")
  data_N_ST2_prev1[[i]] <- data_N_ST2_prev1[[i]][,-c(49,50,51)]
  
  
  
  data_N_ST2_next[[i]] <- shift(data_N_ST2_split[[i]],-1)
  colnames( data_N_ST2_next[[i]] ) <- paste("Next", colnames( data_N_ST2_next[[i]] ), sep = "_")
  data_N_ST2_next[[i]] <- data_N_ST2_next[[i]][,-c(49,50,51)]
  
  data_N_ST2_next1[[i]] <- shift(data_N_ST2_split[[i]],-2)
  colnames( data_N_ST2_next1[[i]] ) <- paste("Next1", colnames( data_N_ST2_next1[[i]] ), sep = "_")
  data_N_ST2_next1[[i]] <- data_N_ST2_next1[[i]][,-c(49,50,51)]
  
  
  data_N_ST2_c[[i]] <- cbind(data_N_ST2_split[[i]],data_N_ST2_prev[[i]],data_N_ST2_next[[i]],data_N_ST2_prev1[[i]],data_N_ST2_next1[[i]])
  
  data_N_ST2_c[[i]] <- data_N_ST2_c[[i]][, c( (1:ncol(data_N_ST2_c[[i]]))[-col_idx],col_idx)]
  
  
}

data_N_ST2_c <- unsplit(data_N_ST2_c,f= data_N_ST2$controllerID)
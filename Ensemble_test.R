glm_ensemble_t <- list()
for (i in 1: 11) { 
  glm_ensemble_t[[i]] <- caretStack(
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

model_preds2 <- list()
model_preds2_woP<- list()
accuracy <- list()
for (i in 1: 11) {
  model_preds2[[i]] <- model_preds[[i]]
  model_preds2_woP[[i]] <- model_preds[[i]]
  model_preds2[[i]]$ensemble <- predict(glm_ensemble_t[[i]], newdata=x_test_ST1[[i]], type="prob")
  model_preds2_woP[[i]]$ensemble <- predict(glm_ensemble_t[[i]], newdata=x_test_ST1[[i]] )
  
  CF <- coef(glm_ensemble[[i]]$ens_model$finalModel)[-1]
  accuracy[[i]] <- colAUC(model_preds2[[i]], y_test_ST1[[i]])
}
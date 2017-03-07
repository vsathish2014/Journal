rm(list=ls())
library(xlsx)
productionTime <- read.xlsx("C:/IIITD/WIP/Data/GearboxFailure/Journal/Robots_ProductionTime.xlsx",sheetName = "ProductionTime")

productionTime_1 <- productionTime[,c(2,12:17)]

prodcutionTime_long <- melt(productionTime_1,id.vars = c("RobotCode"),
                            variable.name="Axis", value.name ="ProdTime_hrs")
prodcutionTime_long$Axis <- gsub("ProdTime_hrs_", "Axis_", prodcutionTime_long$Axis)


library(ggplot2)

a <- ggplot(prodcutionTime_long, aes(x = RobotCode, y = ProdTime_hrs)) + facet_wrap(~Axis, ncol=3)
  a+geom_line(aes(group = 1))

  
  productionTime_1$RobotCode <- factor(productionTime_1$RobotCode , levels=c("R1", "R2", "R3","R4","R5","R6","R7","R8","R9","R10","R11"))   
b <- ggplot(productionTime_1, aes(x = RobotCode, y = ProdTime_hrs_2))+geom_bar(stat = "identity")
b<- b +  theme_classic() +ggtitle("Production Time in hrs")  +labs( x="Robot No (Failed)", y = "Production Time in hrs") 
b<- b +theme(axis.text.x=element_text(angle= 90,size=12,colour ="black"))
b<- b +theme(axis.text.y=element_text(angle= 0,size=12,colour ="black"))
b <- b + theme(axis.title=element_text(size=14))+theme(plot.title = element_text(size=16))
b +theme(plot.title = element_text(hjust = 0.5))

ggsave("C:/IIITD/WIP/Analysis/Journal/Figures/FailedRobotProductionTime.pdf", width = 20, height = 20, units = "cm")

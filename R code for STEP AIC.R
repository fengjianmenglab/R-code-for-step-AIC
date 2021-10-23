
library(MASS)

setwd("F:/gxiang/AIC/145AIC")
Data <- read.csv("145_rarefied_points.csv")
head(Data)
Data<-na.omit(Data)

##Data$Triticum_a<factor(Data$Triticum_a,levels = c(0,1) ,labels = c('0',"1"))
View(Data)
names(Data)
summary(Data)
str(Data)

model<-glm(Triticum_a~.,data = Data)

summary(model)$coefficients
exp(cbind("OR"=coef(model),confint(model)))

model.forward<-stepAIC(model,direction = "forward")
summary(model.forward)$coefficients
exp(cbind("OR"=coef(model.forward),confint(model.forward)))

model.backward<-stepAIC(model,direction = "backward")
summary(model.backward)$coefficients
exp(cbind("OR"=coef(model.backward),confint(model.backward)))

model.both<-stepAIC(model,direction = "both")
AIC_CSV<-summary(model.both)$coefficients
AIC_CSV
write.csv(AIC_CSV,"./145AIC.CSV")
OR<-exp(cbind("OR"=coef(model.both),confint(model.both)))
OR
write.csv(OR,"./145OR.CSV")

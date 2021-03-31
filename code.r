ggplot(data = WeatherAustralia, aes(x = factor(Location), y = MaxTemp, color = Location)) + geom_line(aes(group = Location)) + geom_point()

plot(WeatherAustralia$Date[WeatherAustralia$Location=="Sydney"], WeatherAustralia$MinTemp[WeatherAustralia$Location=="Sydney"])

plot(WeatherAustralia$RainTomorrow[WeatherAustralia$Location=="WaggaWagga"], WeatherAustralia$MinTemp[WeatherAustralia$Location=="WaggaWagga"])

ds.AliceSprings <-  ds[ds$Location %in% c('AliceSprings'), ]
ds.Brisbane <-  ds[ds$Location %in% c('Brisbane'), ]
ds.Darwin <-  ds[ds$Location %in% c('Darwin'), ]
ds.Melbourne <-  ds[ds$Location %in% c('Melbourne'), ]
ds.Perth <-  ds[ds$Location %in% c('Perth'), ]
ds.Portland <-  ds[ds$Location %in% c('Portland'), ]
ds.Sydney <-  ds[ds$Location %in% c('Sydney'), ]

ds.WaggaWagga <-  ds[ds$Location %in% c('WaggaWagga'), ]

ds.test <- ds.AliceSprings[712:1016,1:12]
ds.test <- merge(ds.test, ds.Brisbane[1062:1515, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.Darwin[1086:1550, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.Melbourne[581:829, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.Perth[1115:1592, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.Portland[475:677, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.Sydney[1052:1502, 1:12], all=TRUE)
ds.test <- merge(ds.test, ds.WaggaWagga[756:1078, 1:12], all=TRUE)



ds.train <- ds.AliceSprings[1:711,1:12]
ds.train <- merge(ds.train, ds.Brisbane[1:1061, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.Darwin[1:1085, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.Melbourne[1:580, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.Perth[1:1114, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.Portland[1:474, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.Sydney[1:1051, 1:12], all=TRUE)
ds.train <- merge(ds.train, ds.WaggaWagga[1:755, 1:12], all=TRUE)



ggplot(WeatherAustralia[WeatherAustralia$Location=="WaggaWagga",], aes(MinTemp, Temp3pm)) + geom_point() - ggcorr(ds, label = TRUE)
- cor(ds$MaxTemp, ds$Temp9am)



library(corrplot)
corr <- cor(WeatherAustralia[,c(11,12)])
corrplot(corr)
long <- melt(ds[,c(13,14)])
long <- melt(ds[,c(11,12)])
long <- melt(ds[,c(8,9,10)])
long <- melt(ds[,c(3,4,18,17)])


#LINEAR REGRESSION


m1 <- lm(RainTomorrow ~ ., data = train.data)
anova (m1)
m2 <- lm(RainTomorrow ~ ., data = train.data[,-c(1,5,10)]) #--> remove Date, Evaporation and Cloud3pm
anova (m2)
m3 <- lm(RainTomorrow ~ ., data = train.data[,-c(1,5,10,11)]) #--> remove RainToday
anova(m3)


anova(m1,m2) #--> comparision of m1 and m2
anova(m2,m3) #-> comparision of m2 and m3
anova(m1,m3) #-> comparision of m1 and m3

#m3 is the best model, it has the highest P value 2.432e-09 in comparison to m1 and m2

#fitted(m3) #--> fitted values of m3


#fitted(m1) #--> fitted values of m1
C1 <- as.numeric(fitted(m1)>0.5)
mean(C1 == train.data$RainTomorrow) 
#--> mean value of V1 its equal to  0.8652129 which determine that its so good

#fitted(m2) #--> fitted values of m3
C2 <- as.numeric(fitted(m2)>0.5) 
#--> assign 1 when fitted value >0.5 and 0 when fitted value <0.5
mean(C2 == train.data$RainTomorrow)


C3 <- as.numeric(fitted(m3)>0.5) 
#--> assign 1 when fitted value >0.5 and 0 when fitted value <0.5
mean(C3 == train.data$RainTomorrow)




---------------------------------------------------------------------------------------------------

#LOGISTIC REGRESSION

gm2 <- glm(RainTomorrow ~ ., data = train.data[,-c(1,5,10)], family=binomial) #--> remove Date, Evaporation and Cloud3pm
#summary(gm2)
anova (gm2, test="Chisq")
#---------------------Brier Score----------------------------
mean((fitted(gm2) - train.data$RainTomorrow)^2)
#---------------------ROC CURVE------------------------------


gm3 <- glm(RainTomorrow ~ ., data = train.data[,-c(1,5,10,11)], family=binomial) #--> remove RainToday
summary(gm3)
anova (gm3, test="Chisq")

#---------------------Brier Score---------------------------
mean((fitted(gm3) - train.data$RainTomorrow)^2)

#----------------------ROC CURVE----------------------------


gm1 <- glm(RainTomorrow ~ ., data = train.data, family=binomial)
#summary(gm1)
anova (gm1, test="Chisq")
mean((fitted(gm1) - train.data$RainTomorrow)^2)




anova(gm1,gm2) #--> comparision of m1 and m2
anova(gm2,gm3) #-> comparision of m2 and m3
anova(gm1,gm3) #-> comparision of m1 and m3

#m3 is the best model, it has the highest P value 2.432e-09 in comparison to m1 and m2

#fitted(m3) #--> fitted values of m3


#fitted(m1) #--> fitted values of m1
GC1 <- as.numeric(fitted(gm1)>0.5)
mean(GC1 == train.data$RainTomorrow) 
#--> mean value of V1 its equal to  0.8652129 which determine that its so gsssood

#fitted(m2) #--> fitted values of m3
GC2 <- as.numeric(fitted(gm2)>0.5) 
#--> assign 1 when fitted value >0.5 and 0 when fitted value <0.5
mean(GC2 == train.data$RainTomorrow)


GC3 <- as.numeric(fitted(gm3)>0.5) 
#--> assign 1 when fitted value >0.5 and 0 when fitted value <0.5
mean(GC3 == train.data$RainTomorrow)




#ROC Curve 1

pred.gm1 <-fitted(gm1)
roc.gm1 <- roc(response=train.data$RainTomorrow, predictor=pred.gm1)
roc.gm1
plot.roc(roc.gm1)


#ROC Curve 2

pred.gm2 <-fitted(gm2)
roc.gm2 <- roc(response=train.data$RainTomorrow, predictor=pred.gm2)
roc.gm2
plot.roc(roc.gm2)



#ROC Curve 3

pred.gm3 <-fitted(gm3)
roc.gm3 <- roc(response=train.data$RainTomorrow, predictor=pred.gm3)
roc.gm3
plot.roc(roc.gm3)

--------------------------------------------------------------------------------------------------

library(caret)

library(tidyverse)

library(dplyr)

library(pROC)


set.seed(123)

training.samples <- ds$RainTomorrow %>% createDataPartition(p = 0.7, list = FALSE)

train.data  <- ds[training.samples, ]

test.data  <- ds[-training.samples, ]


#train.data  <- ds.train

#test.data <- ds.test


model1 <- train(RainTomorrow ~ ., data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model1$bestTune

predicted.classes <- (model1 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)



#--------------------------------------------------------------------------------------------------
model2 <- train(RainTomorrow ~ Date + Location + MaxTemp, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model2$bestTune

predicted.classes <- (model2 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)



#--------------------------------------------------------------------------------------------------
model3 <- train(RainTomorrow ~ Date + Location + MaxTemp + Rainfall + Evaporation, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model3$bestTune

predicted.classes <- (model3 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accurcy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)


#--------------------------------------------------------------------------------------------------


model4 <- train(RainTomorrow ~ Date + Location + MaxTemp + Rainfall + Evaporation + Sunshine + WindGustSpeed, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model4$bestTune


predicted.classes <- (model4 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accurcy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)





#--------------------------------------------------------------------------------------------------
model5 <- train(RainTomorrow ~ Date + Location + MaxTemp + Rainfall + Evaporation + Sunshine + WindGustSpeed + Humidity3pm + Pressure3pm, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model5$bestTune

predicted.classes <- (model5 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])
#----------Accurcy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)





#--------------------------------------------------------------------------------------------------
model6 <- train(RainTomorrow ~ Date + Location + MaxTemp + Rainfall + Evaporation + Sunshine + WindGustSpeed + Humidity3pm + Pressure3pm + Cloud3pm, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model6$bestTune

predicted.classes <- (model6 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])
#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)



#-------------------------------------------------


#--------------------------------------------------------------------------------------------------
model7 <- train(RainTomorrow ~ Date + Location + MaxTemp + Evaporation + Sunshine + WindGustSpeed + Humidity3pm + Pressure3pm + Cloud3pm, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)

model7$bestTune

predicted.classes <- (model7 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)


#----------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
model8 <- train(RainTomorrow ~ Date + Location + MaxTemp + Evaporation + Sunshine + WindGustSpeed + Humidity3pm + Pressure3pm + Cloud3pm, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)
?
model8$bestTune

predicted.classes <- (model8 %>% predict(test.data, preProcess = c("center","scale"), type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)


#--------------------------------------------------------------------------------------------------
model9 <- train(RainTomorrow ~ Location + MaxTemp + Evaporation + Sunshine + WindGustSpeed + Humidity3pm + Pressure3pm + Cloud3pm, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)
?
model8$bestTune

predicted.classes <- (model8 %>% predict(test.data, preProcess = c("center","scale"), type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)


#-------------------------------------------------


head(predicted.classes)
plot(model)
model$bestTune

-----------------------------------------------------

ds.Darwin_ <- ds$RainToday
ds.Darwin_ <- ds$RainToday
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
ds.mod <- as.data.frame(lapply(wbcd[1:19], normalize))



------------------------------------------------------------------- Deep Learning---------------------------------------------------------------------------


write.csv(WeatherAustralia, file="WA_CSV.csv")

https://stackoverflow.com/questions/46083268/how-to-get-the-class-probabilities-and-predictions-from-caretpredict



library(keras)
use_condaenv("r-reticulate")
set.seed(61)
DNN.train <- train.data[,-c(1,2,11)]
#DNN.train$Location <- as.numeric(DNN.train$Location)
DNN.train$RainTomorrow <- as.numeric(DNN.train$RainTomorrow) - 1
DNN.test <- test.data[,-c(1,2,11)]

#DNN.test$Location <- as.numeric(DNN.test$Location)
DNN.test$RainTomorrow <- as.numeric(DNN.test$RainTomorrow) - 1
DNN.trainingtarget <- DNN.train[9]
DNN.testingtarget <- DNN.test[9]
DNN.train <- DNN.train[,-c(9)]
DNN.test <- DNN.test[,-c(9)]

------------------


DNN.train <- as.matrix(DNN.train)
DNN.test <- as.matrix(DNN.test)
DNN.trainLabels <- to_categorical(as.matrix(DNN.trainingtarget))
DNN.testLabels <- to_categorical(as.matrix(DNN.testingtarget))
model1 <- keras_model_sequential()
model1 %>% layer_dense(units = 128, activation = 'relu', input_shape = c(8)) %>% layer_dense(units = 64, activation = 'relu') %>% layer_dense(units = 2, activation = 'softmax')

model1 %>% compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

model1 %>% fit(DNN.train, DNN.trainLabels, epochs = 35, batch_size = 5, validation_split = 0.2)

classes <- model1 %>% predict_classes(DNN.test, batch_size = 128)

#---------------Accuracy---------------

score <- model1 %>% evaluate(DNN.test, DNN.testLabels, batch_size = 128)

print(score)

probas <- predict_proba(model1, DNN.test)

#------------Brier Score---------

mean((as.numeric(DNN.testLabels[,2]) - probas[,2])^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = probas[,2])

roc.c

plot(roc.c)


----------------------------------------------------------------------------------------


DNN.train <- train.data[,-c(1,2,11)]

DNN.test <- test.data[,-c(1,2,11)]

DNN.trainingtarget <- DNN.train[9]

DNN.testingtarget <- DNN.test[9]

DNN.train <- DNN.train[,-c(9)]

DNN.test <- DNN.test[,-c(9)]


------------------------------------

DNN.train <- as.matrix(DNN.train)

DNN.test <- as.matrix(DNN.test)

DNN.trainLabels <- to_categorical(as.matrix(DNN.trainingtarget))

DNN.testLabels <- to_categorical(as.matrix(DNN.testingtarget))

model2 <- keras_model_sequential()

model2 %>% layer_dense(units = 256, activation = 'relu', input_shape = c(8)) %>% layer_dense(units = 128, activation = 'relu') %>% layer_dense(units = 2, activation = 'softmax')

model2 %>% compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

model2 %>% fit(DNN.train, DNN.trainLabels, epochs = 35, batch_size = 5, validation_split = 0.2)

classes <- model2 %>% predict_classes(DNN.test, batch_size = 128)

#---------------Accuracy---------------

score <- model2 %>% evaluate(DNN.test, DNN.testLabels, batch_size = 128)

print(score)

probas <- predict_proba(model2, DNN.test)

#------------Brier Score---------

mean((as.numeric(DNN.testLabels[,2]) - probas[,2])^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = probas[,2])

roc.c

plot(roc.c)

----------------------------------------------------------------------------------------


DNN.train <- train.data[,-c(1,2,11)]

DNN.test <- test.data[,-c(1,2,11)]

DNN.trainingtarget <- DNN.train[9]

DNN.testingtarget <- DNN.test[9]

DNN.train <- DNN.train[,-c(9)]

DNN.test <- DNN.test[,-c(9)]

----------------------------------------------------

DNN.train <- as.matrix(DNN.train)

DNN.test <- as.matrix(DNN.test)

DNN.trainLabels <- to_categorical(as.matrix(DNN.trainingtarget))

DNN.testLabels <- to_categorical(as.matrix(DNN.testingtarget))

model3 <- keras_model_sequential()

model3 %>% layer_dense(units = 128, activation = 'relu', input_shape = c(8)) %>% 
  layer_dropout(rate = 0.3) %>% layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>% layer_dense(units = 2, activation = 'softmax')

model3 %>% compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

model3 %>% fit(DNN.train, DNN.trainLabels, epochs = 35, batch_size = 5, validation_split = 0.2)

classes <- model3 %>% predict_classes(DNN.test, batch_size = 128)

#---------------Accuracy---------------

score <- model3 %>% evaluate(DNN.test, DNN.testLabels, batch_size = 128)

print(score)

probas <- predict_proba(model3, DNN.test)

#------------Brier Score---------

mean((as.numeric(DNN.testLabels[,2]) - probas[,2])^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = probas[,2])

roc.c

plot(roc.c)


----------------------------------------------------------------------------------------

DNN.train <- train.data[,-c(1,4,11)]

DNN.test <- test.data[,-c(1,4,11)]

DNN.train$Location <- as.numeric(DNN.train$Location)

DNN.test$Location <- as.numeric(DNN.test$Location)

DNN.trainingtarget <- as.numeric(DNN.train[[9]]) - 1

DNN.testingtarget <- as.numeric(DNN.test[[9]]) - 1

DNN.train <- DNN.train[,-c(9)]

DNN.test <- DNN.test[,-c(9)]

DNN.train <- as.matrix(DNN.train)

DNN.test <- as.matrix(DNN.test)

DNN.trainLabels <- to_categorical(as.matrix(DNN.trainingtarget))

DNN.testLabels <- to_categorical(as.matrix(DNN.testingtarget))

model4 <- keras_model_sequential()

model4 %>% layer_dense(units = 512, activation = 'relu', input_shape = c(8)) %>% layer_dense(units = 256, activation = 'relu') %>% layer_dense(units = 2, activation = 'softmax')

model4 %>% compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

model4 %>% fit(DNN.train, DNN.trainLabels, epochs = 35, batch_size = 10, validation_split = 0.2)

classes <- model4 %>% predict_classes(DNN.test, batch_size = 128)

#---------------Accuracy---------------

score <- model4 %>% evaluate(DNN.test, DNN.testLabels, batch_size = 128)

print(score)

probas <- predict_proba(model4, DNN.test)

#------------Brier Score---------

mean((as.numeric(DNN.testLabels[,2]) - probas[,2])^2)

#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = probas[,2])

roc.c

plot(roc.c)





----------------------------------------------------------------------------------------




DNN.ds[,9] <- as.numeric(DNN.ds[,9]) -1

DNN.test <- DNN.ds[ind == 2, 1:8]

nd <- sample(2, nrow(DNN.ds), replace=TRUE, prob=c(0.70,0.30))




--------------------------------------------------------------------------------------------



---------------------------------

Choosing Predictors:


After taking a look to the Correlation plot we can see that MaxTemp variable has strong correlations with these variables : Rainfall, Cloud3pm, Temp9am, Pressure9am, Evaporation





---------------------------------------



library(Matrix)
library(xgboost)
library(ggplot2)
sparse_matrix <- sparse.model.matrix(ds.mod$RainTomorrow~.,data = ds.mod)
model <- xgboost(data = sparse_matrix, label = ds.mod$RainTomorrow, max.depth = 6, eta = 0.3, nrounds = 16, objective = "binary:logitraw")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = model)
print(xgb.plot.importance(importance_matrix = importance))

---------------------------------------


library(caret)
library(tidyverse)
library(dplyr)
library(pROC)
set.seed(123)
training.samples <- WeatherAustralia$RainTomorrow %>% createDataPartition(p = 0.7, list = FALSE)
train.data  <- WeatherAustralia[training.samples, c(2,3,7,8,12,14,16,15,20)]
test.data  <- WeatherAustralia[-training.samples,]


#train.data  <- ds.train

#test.data <- ds.test


model1 <- train(RainTomorrow ~  Cloud9am + Cloud3pm + Humidity3pm + Humidity9am + Sunshine, data = train.data, method = "knn", trControl = trainControl("cv", number = 10), preProcess = c("center","scale"), tuneLength = 20)
model1$bestTune
predicted.classes <- (model1 %>% predict(test.data, type = "prob")) %>% mutate('class'=names(.)[apply(., 1, which.max)])

View(predicted.classes)
#----------Accuracy-------------
 	
mean(predicted.classes$class == test.data$RainTomorrow)

#------------Brier Score---------

mean(((as.numeric(test.data$RainTomorrow)-1) - predicted.classes$Yes)^2)


#------------ROC-Curve---------

roc.c <- roc(response = test.data$RainTomorrow, predictor = predicted.classes$Yes)

roc.c

plot(roc.c)


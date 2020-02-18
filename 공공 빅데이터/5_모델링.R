##final data
library(randomForest)
library(MASS)
setwd("C:\\rproject\\최종프로젝트\\final")

final<-fread("final.csv")

head(final)
colSums(is.na(final))
final$y_ha<- ifelse(is.na(final$y_ha)==TRUE,0,final$y_ha)
head(final)
result<-mice(final, m=1, maxit=500, method='cart', seed=500)
final <- complete(result)
str(final)
colSums(is.na(final))
write.csv(final, file='final_2.csv')

final<-fread("final_2.csv")

## y_factor factor화
final<-final[,-1]
final$y_factor<-as.factor(final$y_factor)
final$YEAR<- as.factor(final$YEAR)
head(final)
## 사용변수만 select
final2<-final
final2<- final2[,-c(1,2,12)]
head(final2)
str(final2)
model.summer5 <- glm(y_factor~.,data = final2,family=binomial())
summary(model.summer5)

forward<-step(glm(y_factor~1, data=final2, family=binomial()),scope=formula(model.summer5), direction = "forward")
summary(forward) 
formula(forward)

backward=step(glm(y_factor~., data=final2, family=binomial()),scope=formula(model.summer5), direction = "backward")
summary(backward) 
formula(backward)


both=step(glm(y_factor~., data=final2, family=binomial()) , scope=formula(model.summer5),direction = "both")
summary(both)
formula(both)


### 공선성 진단 
vif(model.summer5)

pred=predict(model.summer5, type="response")
table(final2$y_factor, pred>0.5)
for (cut in c(0.5)){
  tab=table(pred > cut, final2$y_factor)
  cat("##### Directoin with cut", cut, "#####", "\n")
  cat("Sensitiviy:",tab["TRUE","1"]/sum(tab["TRUE",]), "\n")
  cat("Specificity:",tab["FALSE","0"]/sum(tab["FALSE",]), "\n")
  cat("Accuracy:", sum(diag(tab))/sum(tab), "\n")
}


library(ROCR)
predob=prediction(pred,final2$y_factor)
plot(performance(predob, "tpr", "fpr"))
performance(predob, "auc")

###ha

## 
# ## 사용변수만 select
# final2<-final
# head(final2)
# final2$y_ha<-as.numeric(final2$y_ha)
# final2<- final2[,-c(11)]
# head(final2)
# str(final2)
# 
# final2<- final2[which(final2$YEAR==2018),]
# head(final2)
# final2<- final2[,-c(1,2)]
# str(final2)
# head(final2)
# 
# #scale
# da_s=scale(final2[,-9]) #표준화
# head(da_s)
# y_ha<-final2[,9]
# f_scale<- as.data.frame(da_s)
# head(f_scale)
# final2<-cbind(f_scale,y_ha)
# head(final2)
# str(final2)
# model.summer5 <- lm(y_ha~.,data = final2)
# summary(model.summer5)



## train-test 
set.seed(1234)
indexTrain <- sample(1:nrow(final2), round(nrow(final2) * .8))


training <- final2[ indexTrain, ]
testing  <- final2[-indexTrain, ]

control <- trainControl(method="repeatedcv", number=5, repeats=3)

str(training)
#### xgbtree


model2 <- train(y_factor ~. ,
                data = training,
                method ="xgbTree",
                trControl = control)

predict(model2, newdata = testing)
predict(model2, newdata = testing) %>% confusionMatrix(testing$y_factor)


#### RF 

rf_fit <- train(y_factor ~ ., 
                data = training, 
                method = "rf", 
                trControl = control, verbose = F)

predict(rf_fit, newdata = testing)
predict(rf_fit, newdata = testing) %>% confusionMatrix(testing$y_factor)


#### 2019년 기준으로 test 하기
setwd("C:\\rproject\\최종프로젝트\\final")
test<- fread("final_3.csv")
test<-test[,-c(1,2)]
str(test)
str(final2)
test$bd_cnt<- as.numeric(test$bd_cnt)
test$y_factor<- as.factor(test$y_factor)
## train-test 
set.seed(1234)


training <- final2
testing  <- test

control <- trainControl(method="repeatedcv", number=5, repeats=3)

str(training)
#### xgbtree


model2 <- train(y_factor ~. ,
                data = training,
                method ="xgbTree",
                trControl = control)

predict(model2, newdata = testing)
predict(model2, newdata = testing) %>% confusionMatrix(testing$y_factor)

#### RF 

rf_fit <- train(y_factor ~ ., 
                data = training, 
                method = "rf", 
                trControl = control)

predict(rf_fit, newdata = testing)
predict(rf_fit, newdata = testing) %>% confusionMatrix(testing$y_factor)

rf_fit = randomForest(y_factor ~ ., 
                      data=training, 
                      mtry = floor(sqrt(7)), 
                      ntree = 300, 
                      importance = T)

predict(rf_fit, newdata = testing) %>% confusionMatrix(testing$y_factor)
importance(rf_fit)

#### rda

rda_fit <- train(y_factor ~ ., 
                data = training, 
                method = "rda", 
                trControl = control)

####gbm

gbm_fit <- train(y_factor ~ ., 
                 data = training, 
                 method = "gbm", 
                 trControl = control)

predict(gbm_fit, newdata = testing) %>% confusionMatrix(testing$y_factor)

##nnet

nn_fit <- train(y_factor ~ ., 
                 data = training, 
                 method = "nnet", 
                 trControl = control)

predict(nn_fit, newdata = testing) %>% confusionMatrix(testing$y_factor)





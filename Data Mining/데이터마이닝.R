#데이터 마이닝
#ch1
train <- read.csv("pepTrainSet.csv", stringsAsFactors=F)
train <- subset(train, select=-c(id))
test <- read.csv("pepTestSet.csv", stringsAsFactors=F)
newd <- read.csv("pepNewCustomers.csv", stringsAsFactors=F)

train$pep <- factor(train$pep)
test$pep <- factor(test$pep)


install.packages("caret")
install.packages("ROCR")
install.packages("C50")

library(caret)
library(ROCR)
library(C50)

c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
c5_model <- C5.0(pep ~ ., data=train, control=c5_options, rules=FALSE)
summary(c5_model)
plot(c5_model)

lm_model <- glm(pep ~ ., data=train, family = binomial)
summary(lm_model)

test$c5_pred <- predict(c5_model, test, type="class")
test$c5_pred_prob <- predict(c5_model, test, type="prob")


install.packages("e1071")
library(e1071)
confusionMatrix(test$c5_pred, test$pep)


test$lm_pred <- ifelse(predict(lm_model, test, type="response") > 0.5, "YES", "NO")
test$lm_pred_prob <- predict(lm_model, test, type="response")
confusionMatrix(test$lm_pred, test$pep)

c5_pred <- prediction(test$c5_pred_prob[, "YES"], test$pep)
c5_model.perf <- performance(c5_pred, "tpr", "fpr")

lm_pred <- prediction(test$lm_pred_prob, test$pep)
lm_model.perf <- performance(lm_pred, "tpr", "fpr")

plot(c5_model.perf, col="red")
plot(lm_model.perf, col="blue", add=T)
legend(0.6, 0.6, c("C5","LM"), cex=0.9, col=c("red", "blue"), lty=1)

newd$c5_pred <- predict(c5_model, newd, type="class")
newd$c5_pred_prob <- predict(c5_model, newd, type="prob")
target <- subset(newd, c5_pred=="YES" & c5_pred_prob[, "YES"] > 0.8)
write.csv(target[order(target$c5_pred_prob[, "YES"], decreasing=T), ], "dm_target.csv", row.names=FALSE)


#ch.2
cb1 <- read.delim("Hshopping.txt", stringsAsFactors=FALSE)
str(cb1)
cb1$반품여부 <- factor(cb1$반품여부)
set.seed(1)
inTrain1 <- createDataPartition(y=cb1$반품여부, p=0.6, list=FALSE)
cb.train1<- cb1[inTrain,]
cb.test1 <- cb1[-inTrain,]
dim(cb.train); dim(cb.test)


c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
c5_model <- C5.0(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train1, control=c5_options, rules=FALSE)
summary(c5_model)
plot(c5_model)

cb.test1$c5_pred <- predict(c5_model, cb.test1,type="class")
cb.test1$c5_pred_prob <- predict(c5_model, cb.test1, type="prob")
head(cb.test1)


confusionMatrix(cb.test1$c5_pred, cb.test1$반품여부)

c5_pred <- prediction(cb.test1$c5_pred_prob[,2],cb.test$반품여부)
c5_model.perf1 <- performance(c5_pred, "tpr", "fpr") 
c5_model.perf2 <- performance(c5_pred, "lift", "rpp")
plot(c5_model.perf1, colorize=TRUE)
plot(c5_model.perf2, colorize=TRUE)
performance(c5_pred, "auc")@y.values[[1]]

install.packages("Epi")
library(Epi)
ROC(form=cb.test$반품여부~c5_pred_prob[,2], data=cb.test, plot="ROC")

#ch.3
install.packages("randomForest")
library(randomForest)


cb <- read.delim("Hshopping.txt", stringsAsFactors=FALSE)
cb$반품여부 <- factor(cb$반품여부)

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

set.seed(123)
rf_model <- randomForest(반품여부 ~ .-ID, data=cb.train, ntree=50, mtry=2)
rf_model

plot(rf_model, main="random Forest model")
importance(rf_model)
varImpPlot(rf_model)
cb.test$rf_pred <- predict(rf_model, cb.test, type="response")
confusionMatrix(cb.test$rf_pred, cb.test$반품여부)

cb.test$rf_pred_prob <- predict(rf_model, cb.test, type="prob")
rf_pred <- prediction(cb.test$rf_pred_prob[,2],cb.test$반품여부)
rf_model.perf1 <- performance(rf_pred, "tpr", "fpr") 
rf_model.perf2 <- performance(rf_pred, "lift", "rpp") 
plot(rf_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)
plot(rf_model.perf2, colorize=TRUE); abline(v=0.4, lty=3)
performance(rf_pred, "auc")@y.values[[1]] 

install.packages("e1071")
library(e1071)
svm_model <- svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, cost=100, gamma=1, probability = TRUE)
summary(svm_model)
plot(svm_model, data=cb.train, 구매금액~나이)

cb.test$svm_pred <- predict(svm_model, cb.test)
confusionMatrix(cb.test$rf_pred, cb.test$반품여부)

postResample(cb.test$svm_pred, cb.test$반품여부)


cb.test$svm_pred_prob <- attr(predict(svm_model, cb.test, probability = TRUE), "probabilities")[,2]
svm_pred <- prediction(cb.test$svm_pred_prob, cb.test$반품여부)
svm_model.perf1 <- performance(svm_pred, "tpr", "fpr")
svm_model.perf2 <- performance(svm_pred, "lift", "rpp") 
plot(svm_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)
plot(svm_model.perf2, colorize=TRUE); abline(v=0.4, lty=3)
performance(svm_pred, "auc")@y.values[[1]]
set.seed(123)
tune.svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, gamma=seq(.5, .9, by=.1), cost=seq(100,1000, by=100))

set.seed(1)
flds <- createFolds(cb$반품여부, k=5, list=TRUE, returnTrain=FALSE)
str(flds)

experiment <- function(train, test, m) {
  rf <- randomForest(반품여부 ~ .-ID, data=train, ntree=50)
  rf_pred <- predict(rf, test, type="response")
  m$acc = c(m$acc, confusionMatrix(rf_pred, test$반품여부)$overall[1])
  rf_pred_prob <- predict(rf, test, type="prob")
  rf_pred <- prediction(rf_pred_prob[,2], cb.test$반품여부)
  m$auc = c(m$auc, performance(rf_pred, "auc")@y.values[[1]])
  return(m) 
}

measure = list()
for(i in 1:5){
  inTest <- flds[[i]]
  cb.test <- cb[inTest, ]
  cb.train <- cb[-inTest, ]
  measure = experiment(cb.train, cb.test, measure) 
}
measure 
mean(measure$acc); sd(measure$acc)
mean(measure$auc); sd(measure$auc)



#데이터 마이닝 과제

library("xgboost")
library(caret)
library(ROCR)

cb <- read.delim("Hshopping.txt", stringsAsFactors=FALSE)
head(cb)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
#학습용 데이터와 검증데이터 나누기
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

cb.train_m<-cb.train[,2:6]
cb.train_m<-cb.train_m[,1:4]
cb.test_m<-cb.test[,2:6]
cb.test_m<-cb.test_m[,1:4]

matrix_train <- apply(cb.train_m, 2, function(x) as.numeric(as.character(x)))
matrix_test <- apply(cb.test_m, 2, function(x) as.numeric(as.character(x)))

trla<-as.numeric(cb.train$반품여부)
tela<-as.numeric(cb.test$반품여부)

xgb_train_matrix <- xgb.DMatrix(data = as.matrix(matrix_train), label = trla)
xgb_test_matrix <- xgb.DMatrix(data = as.matrix(matrix_test), label = tela)

watchlist <- list(train = xgb_train_matrix, test = xgb_test_matrix)
label <- getinfo(xgb_test_matrix, "label")
param <- list("objective" = "binary:logistic")

#cross validation to evaluate the error rate
xgb.cv(param = param, 
       data = xgb_train_matrix, 
       nfold = 3,
       label = getinfo(xgb_train_matrix, "label"),
       nrounds = 6)

#training with gbtree
bst_1 <- xgb.train(data = xgb_train_matrix, 
                   label = getinfo(xgb_train_matrix, "label"),
                   max.depth = 3, 
                   eta = 1, 
                   nthread = 2, 
                   nround = 50, # number of trees used for model building
                   watchlist = watchlist, 
                   objective = "binary:logistic")

features = colnames(matrix_train)
importance_matrix_1 <- xgb.importance(features, model = bst_1)
print(importance_matrix_1)
xgb.ggplot.importance(importance_matrix_1) +theme_minimal()

pred_1 <- predict(bst_1, xgb_test_matrix)
result_1 <- data.frame(ID = rownames(cb.test),
                       outcome = cb.test$반품여부, 
                       label = label, 
                       prediction_p_refund = round(pred_1, digits = 2),
                       prediction = as.integer(pred_1 > 0.5),
                       prediction_eval = ifelse(as.integer(pred_1 > 0.5) != label, "wrong", "correct"))
result_1

err <- as.numeric(sum(as.integer(pred_1 > 0.5) != label))/length(label)
print(paste("test-error =", round(err, digits = 2)))

#confusionMatrix 
confusionMatrix(result_1$prediction,cb.test$반품여부)

xg_pred<-prediction(result_1$prediction_p_refund,cb.test$반품여부)
xg_model.perf1 <- performance(xg_pred, "tpr", "fpr")
xg_model.perf2 <- performance(xg_pred, "lift", "rpp")
plot(xg_model.perf1, colorize=TRUE)
plot(xg_model.perf2, colorize=TRUE)
performance(xg_pred, "auc")@y.values[[1]]
library(Epi)
ROC(form=cb.test$반품여부~prediction_p_refund, data=result_1, plot="ROC")


#프로젝트 
#######4조 코드########
##패키지 
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)# Install & load data munging packages
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(xlsx)) install.packages("xlsx"); library(xlsx)
if(!require(reshape)) install.packages("reshape"); library(reshape)
if(!require(xgboost)) install.packages("xgboost"); library(xgboost)
if(!require(MLmetrics)) install.packages("MLmetrics"); library(MLmetrics)
if(!require(plyr)) install.packages("plyr"); library(plyr)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(ROCR)) install.packages("ROCR"); library(ROCR)
if(!require(C50)) install.packages("C50"); library(C50)
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(tm)) install.packages("tm"); library(tm)
if(!require(topicmodels)) install.packages("topicmodels"); library(topicmodels)
if(!require(matrixStats)) install.packages("matrixStats"); library(matrixStats)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(FSelector)) install.packages("FSelector"); library(FSelector)
if(!require(mlbench)) install.packages("mlbench"); library(mlbench)
if(!require(arules)) install.packages("arules"); library(arules)
if(!require(mice)) install.packages("mice"); library(mice)



####feature1 model(7조코드 참고및 응용)

setwd("c:/b")
train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]

tp<-train_profiles
library(data.table)

train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams


###########파생필드 생성##############

tr<-na.omit(tr)
tr$wk<-weekdays(as.Date(as.character(tr$TIME_ID),"%Y%m%d%H"))
tr$wk<-factor(tr$wk,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
tr$h<-as.numeric(substr(tr$TIME_ID,9,10))
tr$m<-as.numeric(substr(tr$TIME_ID,5,6))
tr$h2<-cut(tr$h,breaks=4,labels=c("0005","0611","1217","1823"))

library(dplyr)
cs.v1<- tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))
cs.v2<- tr %>% distinct(CUS_ID,TIME_ID) %>% group_by(CUS_ID) %>% summarise(VDAYS=n())
cs.v3<- tr %>% distinct(CUS_ID,BACT_NM) %>% group_by(CUS_ID) %>% summarise(COVERAGE=n()/22)

library(reshape)
##카테고리별 페이지뷰 비율
a<-cast(tr,CUS_ID~BACT_NM,sum,value="SITE_CNT")
a$sum<-rowSums(a[,2:23])
cs.v4<-a[,2:23]/a$sum*100
colnames(cs.v4)<-paste("CP",1:22,sep="")

##요일별 페이지뷰 비율
b<-cast(tr,CUS_ID~wk,sum,value="SITE_CNT")
b$sum<-rowSums(b[,2:8])
cs.v5<-b[,2:8]/b$sum*100
colnames(cs.v5)<-paste("DP",1:7,sep="")

##시간대별 페이지뷰 비율
c<-cast(tr,CUS_ID~h2,sum,value="SITE_CNT")
c$sum<-rowSums(c[,2:5])
cs.v6<-c[,2:5]/c$sum
colnames(cs.v6)<-paste("HP",1:4,sep="")

##월별 페이지뷰 비율
d<-cast(tr,CUS_ID~m,sum,value = "SITE_CNT")
d$sum<-rowSums(d[,2:13])
cs.v7<-d[,2:13]/d$sum*100
colnames(cs.v7)<-paste("MP",colnames(cs.v7),sep="")

###변동계수만들기####
COV<-data.frame(CUS_ID=1:2500)
##요일별 변동계수
DCOV<- tr %>% group_by(CUS_ID,wk) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(DCOV=sd(sum)/mean(sum))
COV<-merge(COV,DCOV,by="CUS_ID",all.x = T)
##월별 변동계수
MCOV<- tr %>% group_by(CUS_ID,m) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(MCOV=sd(sum)/mean(sum))
COV<-merge(COV,MCOV,by="CUS_ID",all.x = T)
##시간대별 변동계수
HCOV<- tr %>% group_by(CUS_ID,h2) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(HCOV=sd(sum)/mean(sum))
COV<-merge(COV,HCOV,by="CUS_ID",all.x = T)
##카데고리별 변동계수
CCOV<- tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(sum)/mean(sum))
COV<-merge(COV,CCOV,by="CUS_ID",all.x = T)

###############변수 추가###############
cs.v10<- tr %>% distinct(CUS_ID,SITE_NM) %>% group_by(CUS_ID) %>% summarise(NSITE=n())
cs.v11<- tr %>% group_by(CUS_ID,SITE_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(NSCOV=sd(N)/mean(N))


train_final<-cs.v1 %>% left_join(cs.v2) %>% left_join(cs.v3)
train_final<-cbind(train_final,cs.v4,cs.v5,cs.v6,cs.v7)
train_final<-merge(train_final,COV,by="CUS_ID",all.x=T)
train_final<-train_final %>% left_join(cs.v10) %>% left_join(cs.v11)
train_final<-merge(train_profiles,train_final,by="CUS_ID")
head(train_final)
#########변수 추가 2## 17 05 07
a<-tr %>% filter(BACT_NM=="쇼핑")%>%group_by(CUS_ID,MACT_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(cov=sd(N)/mean(N))

train_final<-left_join(train_final,a)

###카테고리별 자주가는 사이트 17 05 07
cate<-data.frame(CUS_ID=1:2500)

c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE11<-ifelse(is.na(cate$SITE11),"NONE",cate$SITE11)
cate$SITE11<-factor(cate$SITE11,labels=1:8)

train_final<-merge(train_final,cate,by="CUS_ID",all.x=T)

#######트레인 키워드 정리#######
train_searchkeywords<-read.delim("train_searchkeywords.tab", stringsAsFactors = F)
head(train_searchkeywords)
ts<-train_searchkeywords

a<-aggregate(ts[2],by=list(CUS_ID=ts$CUS_ID),length)
colnames(a)[2]<-"TOTS"
b<-aggregate(ts[3],by=list(CUS_ID=ts$CUS_ID),sd)
b$QRY_CNT<-ifelse(is.na(b$QRY_CNT),0,b$QRY_CNT)
c<-aggregate(ts[3],by=list(CUS_ID=ts$CUS_ID),mean)
d<-data.frame(CUS_ID=a$CUS_ID,SCOV=b$QRY_CNT/c$QRY_CNT)

train_final<-merge(train_final,a,by="CUS_ID",all=T)
train_final<-merge(train_final,d,by="CUS_ID",all=T)

library(mice)
d<-mice(train_final[,-1],print=F)
d<-merge(train_final[,1,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
train_final<-d[,-1]
train_final<-train_final[order(train_final$CUS_ID),]
head(train_final)

########### 변수추가 05 09 ############
a<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID) %>% summarise(SH=sum(ST_TIME))
train_final<-left_join(train_final,a)
train_final$SH<-ifelse(is.na(train_final$SH),0,train_final$SH)


#######변수추가 0511######
v0<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))

#시간대별 체류시간 비율

v1<-cast(tr,CUS_ID~h,sum,value="ST_TIME")
v11<-v1[,2:25]/rowSums(v1[,2:25])*100
colnames(v11)<-paste("H",colnames(v11),sep="")

#카테고리별 체류시간 비율
v2<-cast(tr,CUS_ID~BACT_NM,sum,value="ST_TIME")
v21<-v2[,2:23]/rowSums(v2[,2:23])*100
colnames(v21)<-paste("CP",1:22,".2",sep="")


#시간대별 체류시간 변동계수
v3<-tr %>% group_by(CUS_ID,h) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))
v4<-tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))

head(train_final)
train_final<-train_final%>%left_join(v0) %>% left_join(v3) %>% left_join(v4)
train_final<-cbind(train_final,v11,v21)


###트레인 소분류 워투백####
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)


###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items.txt","vec7.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec7.bin") # reload the model. 
df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(tr.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

#######방문한 SITE_NM모음#####
###트레인 모델 생성 ####
###### Make sites sentences

f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items-site_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-site_nm.txt","vec-site_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 

df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(tr.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")

train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

sum(is.na(train_final)) ## 0인가...?
write.csv(train_final,"train_final_170513_2.csv",row.names = F)


################테스트 데이터##################
test_clickstreams<-read.table("test_clickstreams.tab",sep="\t",header = T,stringsAsFactors = F)
tr<-test_clickstreams

##파생필드 생성
tr<-na.omit(tr)
tr$wk<-weekdays(as.Date(as.character(tr$TIME_ID),"%Y%m%d%H"))
tr$wk<-factor(tr$wk,levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
tr$h<-as.numeric(substr(tr$TIME_ID,9,10))
tr$m<-as.numeric(substr(tr$TIME_ID,5,6))
tr$h2<-cut(tr$h,breaks=4,labels=c("0005","0611","1217","1823"))

library(dplyr)
cs.v1<- tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))
cs.v2<- tr %>% distinct(CUS_ID,TIME_ID) %>% group_by(CUS_ID) %>% summarise(VDAYS=n())
cs.v3<- tr %>% distinct(CUS_ID,BACT_NM) %>% group_by(CUS_ID) %>% summarise(COVERAGE=n()/22)

library(reshape)

##카테고리별 페이지뷰 비율
a<-cast(tr,CUS_ID~BACT_NM,sum,value="SITE_CNT")
a$sum<-rowSums(a[,2:23])
cs.v4<-a[,2:23]/a$sum*100
colnames(cs.v4)<-paste("CP",1:22,sep="")

##요일별 페이지뷰 비율
b<-cast(tr,CUS_ID~wk,sum,value="SITE_CNT")
b$sum<-rowSums(b[,2:8])
cs.v5<-b[,2:8]/b$sum*100
colnames(cs.v5)<-paste("DP",1:7,sep="")

##시간대별 페이지뷰 비율
c<-cast(tr,CUS_ID~h2,sum,value="SITE_CNT")
c$sum<-rowSums(c[,2:5])
cs.v6<-c[,2:5]/c$sum
colnames(cs.v6)<-paste("HP",1:4,sep="")

##월별 페이지뷰 비율
d<-cast(tr,CUS_ID~m,sum,value = "SITE_CNT")
d$sum<-rowSums(d[,2:13])
cs.v7<-d[,2:13]/d$sum*100
colnames(cs.v7)<-paste("MP",colnames(cs.v7),sep="")

######변동계수만들기############
COV<-data.frame(CUS_ID=2501:5000)
##요일별 변동계수
DCOV<- tr %>% group_by(CUS_ID,wk) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(DCOV=sd(sum)/mean(sum))
COV<-merge(COV,DCOV,by="CUS_ID",all.x = T)
##월별 변동계수
MCOV<- tr %>% group_by(CUS_ID,m) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(MCOV=sd(sum)/mean(sum))
COV<-merge(COV,MCOV,by="CUS_ID",all.x = T)
##시간대별 변동계수
HCOV<- tr %>% group_by(CUS_ID,h2) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(HCOV=sd(sum)/mean(sum))
COV<-merge(COV,HCOV,by="CUS_ID",all.x = T)
##카데고리별 변동계수
CCOV<- tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(sum=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(sum)/mean(sum))
COV<-merge(COV,CCOV,by="CUS_ID",all.x = T)

###############변수 추가###############
cs.v10<- tr %>% distinct(CUS_ID,SITE_NM) %>% group_by(CUS_ID) %>% summarise(NSITE=n())
cs.v11<- tr %>% group_by(CUS_ID,SITE_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(NSCOV=sd(N)/mean(N))


test_final<-cs.v1 %>% left_join(cs.v2) %>% left_join(cs.v3)
test_final<-cbind(test_final,cs.v4,cs.v5,cs.v6,cs.v7)
test_final<-merge(test_final,COV,by="CUS_ID",all.x=T)
test_final<-test_final %>% left_join(cs.v10) %>% left_join(cs.v11)
head(test_final)

##변수추가 17 05 07 천천히...
a<-tr %>% filter(BACT_NM=="쇼핑")%>%group_by(CUS_ID,MACT_NM) %>% summarise(N=n()) %>% group_by(CUS_ID) %>% summarise(cov=sd(N)/mean(N))

test_final<-left_join(test_final,a)

cate<-data.frame(CUS_ID=2501:5000)

c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE11<-ifelse(is.na(cate$SITE11),"NONE",cate$SITE11)
cate$SITE11<-factor(cate$SITE11,labels=1:8)


test_final<-merge(test_final,cate,by="CUS_ID",all.x=T)
head(test_final)

#############테스트 키워드 정리################
test_searchkeywords<-read.delim("test_searchkeywords.tab", stringsAsFactors = F)
ts1<-test_searchkeywords
a<-aggregate(ts1[2],by=list(CUS_ID=ts1$CUS_ID),length)
colnames(a)[2]<-"TOTS"

SCOV<- ts1 %>% group_by(CUS_ID) %>% summarise(SCOV=sd(QRY_CNT)/mean(QRY_CNT))

test_final<-merge(test_final,a,by="CUS_ID",all=T)
test_final<-merge(test_final,SCOV,by="CUS_ID",all.x=T)

library(mice)
d<-mice(test_final[,-1],print=F)
d<-merge(test_final[,1,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
test_final<-d[,-1]
test_final<-test_final[order(test_final$CUS_ID),]
head(test_final)
sum(is.na(test_final)) ##0이어야함

########### 변수추가 05 09 ############
a<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID) %>% summarise(SH=sum(ST_TIME))
test_final<-left_join(test_final,a)
test_final$SH<-ifelse(is.na(test_final$SH),0,test_final$SH)


#######변수추가 0511######
v0<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))

#시간대별 체류시간 비율
v1<-cast(tr,CUS_ID~h,sum,value="ST_TIME")
v11<-v1[,2:25]/rowSums(v1[,2:25])*100
colnames(v11)<-paste("H",colnames(v11),sep="")

#카테고리별 체류시간 비율
v2<-cast(tr,CUS_ID~BACT_NM,sum,value="ST_TIME")
v21<-v2[,2:23]/rowSums(v2[,2:23])*100
colnames(v21)<-paste("CP",1:22,".2",sep="")


#시간대별 체류시간 변동계수
v3<-tr %>% group_by(CUS_ID,h) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))
v4<-tr %>% group_by(CUS_ID,BACT_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))


test_final<-test_final%>%left_join(v0) %>% left_join(v3) %>% left_join(v4)
test_final<-cbind(test_final,v11,v21)

####테스트 소분류 워투백 모델 적용#####
tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
model <- read.binary.vectors("vec7.bin")
df<-data.frame()
for (i in 2501:5000 ){
  itemfreq <- table(tr2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")
test_final<-merge(test_final,df,by="CUS_ID")
head(test_final)

sum(is.na(test_final))


####테스트 사이트 워투백 모델 적용####
tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
model <- read.binary.vectors("vec-site_nm.bin")
df<-data.frame()
for (i in 2501:5000 ){
  itemfreq <- table(tr2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")


test_final<-merge(test_final,df,by="CUS_ID")
head(test_final)

sum(is.na(test_final)) ###0인가요...

write.csv(test_final,"test_final_170513_2.csv",row.names = F)



##train 서치키워드 
cs.dt <- fread("train_profiles.csv")
ts<-fread("train_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(ts, CUS_ID) 
mg<-merge(cs.dt,ts)
mg$QRY_STR<-ifelse(regexpr("query",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,regexpr("query",mg$QRY_STR)+6,500),
                   mg$QRY_STR)
mg$QRY_STR<-ifelse(regexpr("acq",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,1,regexpr("&",mg$QRY_STR)-1),
                   mg$QRY_STR)

f <- function(x, t) {
  grp <- mg[CUS_ID==x, GROUP][1]
  itemfreq <- table(mg[CUS_ID==x,  QRY_STR])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:10, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items_s <- unlist(sapply(cs.dt$CUS_ID, f, 1))
write.table(items_s, "items-ts.txt", eol = " ", quote = F, row.names = F, col.names = F)

model = train_word2vec("items-ts.txt","vec-ts.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec-ts.bin") # reload the model.

b<-unique(mg$CUS_ID)

df<-data.frame()
for (i in b ){
  itemfreq <- table(mg[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
train_final<-read.csv("train_final_170513_2.csv")
train_final2<-left_join(train_final,df)
head(train_final2)
sum(is.na(train_final2))

library(mice)
d<-mice(train_final2[,123:128],print=F)
d<-merge(train_final2[,1:122,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

train_final2<-d[order(d$CUS_ID),]
write.csv(train_final2,"train_final.csv",row.names = F)

###test 키워드 모델 적용####
ts2<-fread("test_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(ts2, CUS_ID) 
model <- read.binary.vectors("vec-ts.bin") # reload the model.
ts2$QRY_STR<-ifelse(regexpr("query",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,regexpr("query",ts2$QRY_STR)+6,500),
                    ts2$QRY_STR)
ts2$QRY_STR<-ifelse(regexpr("acq",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,1,regexpr("&",ts2$QRY_STR)-1),
                    ts2$QRY_STR)
df<-data.frame()
b<-unique(ts2$CUS_ID)
for (i in b ){
  itemfreq <- table(ts2[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
test_final<-read.csv("test_final_170513_2.csv")
test_final2<-left_join(test_final,df)
head(test_final2)
sum(is.na(test_final2))

library(mice)
d<-mice(test_final2[,122:127],print=F)
d<-merge(test_final2[,1:121,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

test_final2<-d[order(d$CUS_ID),]
write.csv(test_final2,"test_final.csv",row.names = F)

#####0526변수추가#####

rm(list=ls())

train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]
tp<-train_profiles

train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams
test_clickstreams<-fread("test_clickstreams.tab");test_clickstreams[,CUS_ID:=as.numeric(CUS_ID)]
tr2<-test_clickstreams

train_final<-read.csv("train_final.csv")
test_final<-read.csv("test_final.csv")

library("dplyr")
library("reshape")
detach(package:plyr)
####TRAIN####
n0<-cast(tr,CUS_ID~MACT_NM,length,value="SITE_CNT")
n0[,-1]<-n0[,-1]/rowSums(n0[,-1])*100
colnames(n0)[-1]<-paste("CMP",1:(dim(n0)[2]-1),sep="")


n1<-cast(tr[tr$BACT_NM=="뉴스/미디어",],CUS_ID~ACT_NM,length,value="ST_TIME")
n1[,-1]<-n1[,-1]/rowSums(n1[,-1])*100
names(n1)[-1]<-paste("newA",1:(dim(n1)[2]-1),sep="")


n2<-cast(tr[tr$BACT_NM=="쇼핑",],CUS_ID~ACT_NM,length,value="ST_TIME")
n2[,-1]<-n2[,-1]/rowSums(n2[,-1])*100
names(n2)[-1]<-paste("shopA",1:(dim(n2)[2]-1),sep="")

train_final<- train_final %>% left_join(n0) %>% left_join(n1) %>% left_join(n2)
train_final[is.na(train_final)]<-0
write.csv(train_final,"train_final_170526.csv",row.names=F)

####TEST####
n0<-cast(tr2,CUS_ID~MACT_NM,length,value="SITE_CNT")
n0[,-1]<-n0[,-1]/rowSums(n0[,-1])*100
colnames(n0)[-1]<-paste("CMP",1:(dim(n0)[2]-1),sep="")


n1<-cast(tr2[tr2$BACT_NM=="뉴스/미디어",],CUS_ID~ACT_NM,length,value="ST_TIME")
n1[,-1]<-n1[,-1]/rowSums(n1[,-1])*100
names(n1)[-1]<-paste("newA",1:(dim(n1)[2]-1),sep="")


n2<-cast(tr2[tr2$BACT_NM=="쇼핑",],CUS_ID~ACT_NM,length,value="ST_TIME")
n2[,-1]<-n2[,-1]/rowSums(n2[,-1])*100
names(n2)[-1]<-paste("shopA",1:(dim(n2)[2]-1),sep="")

test_final<- test_final %>% left_join(n0) %>% left_join(n1) %>% left_join(n2)
test_final[is.na(test_final)]<-0
write.csv(test_final,"test_final_170526.csv",row.names=F)

######0528변수추가#### 
rm(list=ls())
library(xlsx)
train_profiles<-read.xlsx("train_profiles.xlsx",1)
train_profiles$CUS_ID<-as.numeric(as.character(train_profiles$CUS_ID))
train_profiles<-train_profiles[order(train_profiles$CUS_ID,decreasing = F),]
tp<-train_profiles
library(data.table)
train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams
test_clickstreams<-fread("test_clickstreams.tab");test_clickstreams[,CUS_ID:=as.numeric(CUS_ID)]
tr2<-test_clickstreams
mg<-merge(tp,tr,by="CUS_ID")

train_final<-read.csv("train_final_170526.csv")
test_final<-read.csv("test_final_170526.csv")
library(dplyr)
cate<-data.frame(CUS_ID=1:2500)
c1<- mg %>% filter(BACT_NM=="게임") %>% group_by(CUS_ID,SITE_NM) %>%  summarise(SITE5=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$SITE5)
c12<-c11[!c11$SITE5<=mean(c11$SITE5),]
am<-merge(c12,c1,by.x=c("CUS_ID","SITE5"),by.y = c("CUS_ID","SITE5"),all=F,all.x=T,all.y=F)
dim(table(am$SITE_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("피망","넷마블","한게임","다음 게임","인벤","넥슨","네이버 야구9단","던전앤파이터","루리웹닷컴")
am<-am[am$SITE_NM %in% b,]
colnames(am)[2]<-"SITE5"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE5<-ifelse(is.na(cate$SITE5),"NONE",cate$SITE5)
cate$SITE5<-factor(cate$SITE5,labels=1:10)
train_final<-merge(train_final,cate,by="CUS_ID",all.x=T)
write.csv(train_final,"train_final_170528.csv",row.names = F)

cate<-data.frame(CUS_ID=2501:5000)
c1<- tr2 %>% filter(BACT_NM=="게임") %>% group_by(CUS_ID,SITE_NM) %>%  summarise(SITE5=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$SITE5)
c12<-c11[!c11$SITE5<=mean(c11$SITE5),]
am<-merge(c12,c1,by.x=c("CUS_ID","SITE5"),by.y = c("CUS_ID","SITE5"),all=F,all.x=T,all.y=F)
dim(table(am$SITE_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("피망","넷마블","한게임","다음 게임","인벤","넥슨","네이버 야구9단","던전앤파이터","루리웹닷컴")
am<-am[am$SITE_NM %in% b,]
colnames(am)[2]<-"SITE5"
cate<-merge(cate,am,by="CUS_ID",all.x=T)
cate$SITE5<-ifelse(is.na(cate$SITE5),"NONE",cate$SITE5)
cate$SITE5<-factor(cate$SITE5,labels=1:10)
test_final<-merge(test_final,cate,by="CUS_ID",all.x=T)
write.csv(test_final,"test_final_170528.csv",row.names = F)


########40%테스트데이터 합치기#####
rm(list=ls())
train_final<-read.csv("train_final_170528.csv")
test_final<-read.csv("test_final_170528.csv")

test_pulic<-read.csv("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$F20.==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$F40.==1,"F40+",
                                       ifelse(test_pulic$M20.,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))

test_final2<-merge(test_pulic[,c(1,8)],test_final,by="CUS_ID",all.x=T)
train_final<-rbind(train_final,test_final2)
test_final<-test_final[!(test_final$CUS_ID %in% test_pulic$CUS_ID),]

write.csv(train_final,"train_final_170529.csv",row.names = F)
write.csv(test_final,"test_final_170529.csv",row.names = F)

#######워드투백 새로 생성하기####
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID)
###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))

write.table(items, "items-act_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Train site2vec model
model = train_word2vec("items-act_nm.txt","vec-act_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-act_nm.bin") # reload the model. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

train_final<-read.csv("train_final_170529.csv")
train_final[,111:116]<-NULL
train_final<-merge(train_final,df,by="CUS_ID")

#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")

test_final<-read.csv("test_final_170529.csv")
test_final[,110:115]<-NULL
test_final<-merge(test_final,df,by="CUS_ID")

#######방문한 SITE_NM모음#####
###### 트레인 모델 생성
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))
write.table(items, "items-site_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-site_nm.txt","vec35-site_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec35-site_nm.bin") # reload the model. 

df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")
train_final[,111:116]<-NULL
train_final<-merge(train_final,df,by="CUS_ID")
head(train_final)

sum(is.na(train_final)) 
write.csv(train_final,"train_final_170530.csv",row.names = F)  ##사용x


#####테스트적용#####
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% test_pulic$CUS_ID),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")
test_final[,110:115]<-NULL
test_final<-merge(test_final,df,by="CUS_ID")
sum(is.na(test_final))
write.csv(test_final,"test_final_170530.csv",row.names = F)  ##사용x


#############키워드 워투백##################
rm(list=ls())
cs.dt <- fread("train_profiles.csv")
ts<-fread("train_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(ts, CUS_ID) 
mg<-merge(cs.dt,ts)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
ts2<-fread("test_searchkeywords.tab", stringsAsFactors = F);ts[,CUS_ID:= as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(ts2, CUS_ID) 
mg2<-merge(test_pulic[,c(1,8)],ts2,by="CUS_ID")
mg<-rbind(mg,mg2)

mg$QRY_STR<-ifelse(regexpr("query",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,regexpr("query",mg$QRY_STR)+6,500),
                   mg$QRY_STR)
mg$QRY_STR<-ifelse(regexpr("acq",mg$QRY_STR)>0 ,
                   substr(mg$QRY_STR,1,regexpr("&",mg$QRY_STR)-1),
                   mg$QRY_STR)
cus<-unique(mg$CUS_ID)
f <- function(x, t) {
  grp <- mg[CUS_ID==x, GROUP][1]
  itemfreq <- table(mg[CUS_ID==x,  QRY_STR])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:10, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items_s <- unlist(sapply(cus, f, 1))
write.table(items_s, "items-ts-all.txt", eol = " ", quote = F, row.names = F, col.names = F)

model = train_word2vec("items-ts-all.txt","vec-ts-all.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec-ts-all.bin") # reload the model.

b<-unique(mg$CUS_ID)

df<-data.frame()
for (i in b ){
  itemfreq <- table(mg[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
train_final<-read.csv("train_final_170530.csv")  
train_final[111:116]<-NULL
train_final2<-merge(train_final,df,by="CUS_ID",all.x=T)

library(mice)
d<-mice(train_final2[,554:559],print=F)
d<-merge(train_final2[,1:553,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

train_final2<-d[order(d$CUS_ID),]
write.csv(train_final2,"train_final_170530_k.csv",row.names = F)  ##사용x

###테스트 키워드 모델 적용####
ts2<-ts2[!(ts2$CUS_ID %in% test_pulic$CUS_ID),]
ts2$QRY_STR<-ifelse(regexpr("query",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,regexpr("query",ts2$QRY_STR)+6,500),
                    ts2$QRY_STR)
ts2$QRY_STR<-ifelse(regexpr("acq",ts2$QRY_STR)>0 ,
                    substr(ts2$QRY_STR,1,regexpr("&",ts2$QRY_STR)-1),
                    ts2$QRY_STR)
df<-data.frame()
b<-unique(ts2$CUS_ID)
for (i in b ){
  itemfreq <- table(ts2[CUS_ID==i, QRY_STR])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","KF20","KF30","KF40","KM20","KM30","KM40")

library(dplyr)
test_final<-read.csv("test_final_170530.csv")
test_final[110:115]<-NULL
test_final2<-merge(test_final,df,by="CUS_ID",all.x = T)
head(test_final2)
sum(is.na(test_final2))

library(mice)
d<-mice(test_final2[,553:558],print=F)
d<-merge(test_final2[,1:552,drop=F],complete(d,2),by="row.names",all.x=T)
rownames(d)<-d$Row.names
head(d)
d<-d[,-1]

test_final2<-d[order(d$CUS_ID),]
write.csv(test_final2,"test_final_170530_k.csv",row.names = F) ##사용x



###변수 select### feature2model
#train()
rm(list=ls())

train <- read.csv("train_final_170530_k.csv")
test <- read.csv("test_final_170530_k.csv")

train$GROUP<-factor(train$GROUP,labels =c("F20","F30","F40","M20","M30","M40"))

weights <- chi.squared(GROUP~., train[,-1])
print(weights)

train_var = train[,-c(1,2)]
selvar = train_var[,weights$attr_importance !=0]
train_sel = cbind(train[,1:2], selvar)

write.csv(train_sel, file = "train_final_sel.csv", row.names = F)

#test
test_var = test[,-1]
selvar_test = test_var[,weights$attr_importance !=0]
test_sel = cbind(test[,1], selvar_test)
names(test_sel)[1]<-"CUS_ID"
write.csv(test_sel, file = "test_final_sel.csv", row.names = F)

##모델생성(feature1)

train<-read.csv("train_final_170528.csv")
test<-read.csv("test_final_170528.csv")

train$GROUP<-as.character(train$GROUP)
train$GROUP[train$GROUP=="F20-"]<-"F20"
train$GROUP[train$GROUP=="F40+"]<-"F40"
train$GROUP[train$GROUP=="M20-"]<-"M20"
train$GROUP[train$GROUP=="M40+"]<-"M40"
train$GROUP<-as.factor(train$GROUP)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("7_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

##-------------------------------------------------------------------#####

#2500개 변수선택 
rm(list=ls())

train <- read.csv("train_final_170528.csv")
test <- read.csv("test_final_170528.csv")

train$GROUP<-factor(train$GROUP,labels =c("F20","F30","F40","M20","M30","M40"))

weights <- chi.squared(GROUP~., train[,-1])
print(weights)

train_var = train[,-c(1,2)]
selvar = train_var[,weights$attr_importance !=0]
train_sel = cbind(train[,1:2], selvar)
train<-train_sel 
write.csv(train_sel, file = "train_final_sel.csv", row.names = F)

#test
test_var = test[,-1]
selvar_test = test_var[,weights$attr_importance !=0]
test_sel = cbind(test[,1], selvar_test)
names(test_sel)[1]<-"CUS_ID"
write.csv(test_sel, file = "test_final_sel.csv", row.names = F)
test<-test_sel

#3500개 변수선택 (사용x)
rm(list=ls())

train <- read.csv("train_final_170530_k.csv")
test <- read.csv("test_final_170530_k.csv")

train$GROUP<-factor(train$GROUP,labels =c("F20","F30","F40","M20","M30","M40"))

weights <- chi.squared(GROUP~., train[,-1])
print(weights)

train_var = train[,-c(1,2)]
selvar = train_var[,weights$attr_importance !=0]
train_sel = cbind(train[,1:2], selvar)
train<-train_sel 
write.csv(train_sel, file = "train_final_sel35.csv", row.names = F)

#test
test_var = test[,-1]
selvar_test = test_var[,weights$attr_importance !=0]
test_sel = cbind(test[,1], selvar_test)
names(test_sel)[1]<-"CUS_ID"
write.csv(test_sel, file = "test_final_sel35.csv", row.names = F)
test<-test_sel

#모델 적용(feature2) 

train<-read.csv("train_final_sel.csv",stringsAsFactors = F)
test<-read.csv("test_final_sel.csv",stringsAsFactors = F)

train$GROUP<-as.character(train$GROUP)
train$GROUP[train$GROUP=="F20-"]<-"F20"
train$GROUP[train$GROUP=="F40+"]<-"F40"
train$GROUP[train$GROUP=="M20-"]<-"M20"
train$GROUP[train$GROUP=="M40+"]<-"M40"
train$GROUP<-as.factor(train$GROUP)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","gbm")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}


# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("7selctfea_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

##-----------------------------------------------------------------------######

##대학원 코드(feature3 model) 
rm(list=ls())
setwd("C:/b")
###load data
cs.dt.main <- fread("train_profiles.csv")
tr.dt.main <- fread("train_clickstreams.tab"); tr.dt.main[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.main, CUS_ID); setkey(tr.dt.main, CUS_ID)
md.dt.main <- merge(cs.dt.main, tr.dt.main)

###40%test datat
cs.dt.sub <- fread("test_public.csv")
cs.dt.sub$GROUP<-ifelse(cs.dt.sub$`F20-`==1,"F20-",
                        ifelse(cs.dt.sub$F30==1,"F30",
                               ifelse(cs.dt.sub$`F40+`==1,"F40+",
                                      ifelse(cs.dt.sub$`M20-`,"M20-",
                                             ifelse(cs.dt.sub$M30,"M30","M40+")))))
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.sub, CUS_ID)
setkey(tr.dt.sub, CUS_ID) 
md.dt.sub <- merge(cs.dt.sub[,c(1,8)], tr.dt.sub)


###전처리 
cstr <- md.dt.main %>% select(-GROUP)
prof <- md.dt.main %>% select(CUS_ID,GROUP) %>% distinct(CUS_ID, .keep_all=T)


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

pf = prof %>% 
  mutate(gr=ifelse(GROUP=="M20-",0,
                   ifelse(GROUP=="M30",1,
                          ifelse(GROUP=="M40+",2, 
                                 ifelse(GROUP=="F20-",3,
                                        ifelse(GROUP=="F30",4,5)))))) %>% select(-GROUP)

cs.v1= pf %>% left_join(cs.t1)%>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= pf %>% left_join(cs.t2) %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = pf %>% left_join(cs.v1) %>% left_join(cs.v2)

cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))

cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())

cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 



cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- pf %>% 
  left_join(custsig1) %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig.csv",row.names = F)


## train_sub
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.dt.sub, CUS_ID) 

profsub =read.csv("test_public.csv", stringsAsFactors=F)

profsubtest= profsub %>%
  mutate(GROUP=ifelse(M20.==1,"M20-",
                      ifelse(M30==1,"M30",
                             ifelse(M40.==1,"M40+", 
                                    ifelse(F20.==1,"F20-",
                                           ifelse(F30==1,"F30",
                                                  ifelse(F40.==1,"F40+",NA))))))) %>% select(-F20.:-M40.)

# test data
tr.t.dt <- tr.dt.sub
setkey(tr.t.dt, CUS_ID)

cstr <- tr.t.dt


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

cs.v1= cs.t1 %>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= cs.t2 %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = cs.v1 %>% left_join(cs.v2)
rm(cs.t1);rm(cs.t2);rm(cs.v1);rm(cs.v2);rm(cs.c1);rm(cs.c2)


cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))


cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())


cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 


cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- custsig1 %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig_test.csv",row.names = F)


#feature selection
rm(list=ls())

sig <- read.csv("custsig.csv", stringsAsFactors=F)
sig = sig %>% mutate(weekend=sun+sat)

sigsubmit <- read.csv("custsig_test.csv", stringsAsFactors=F)
sigsubmit = sigsubmit %>% mutate(weekend=sun+sat)

cb.train=sig
cb.test= sigsubmit

#model train
cb.train$gr <- factor(cb.train$gr)
cb.train$gr = paste("C",cb.train$gr,sep="")
cb3.train = cb.train %>%
  select(CUS_ID,gr,#sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)

cb3.test = cb.test %>%
  select(CUS_ID, #sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)

cb3.train$gr<-NULL
profile <- read.csv("train_profiles.csv", stringsAsFactors = F)
cb3.train1<-merge(profile,cb3.train,by="CUS_ID")
length(unique(cb3.train1$CUS_ID))
write.csv(cb3.train1,"master1_train.csv",row.names = F)
write.csv(cb3.test,"master1_test.csv",row.names = F)


##모델생성(feature3model)

train<-read.csv("master1_train.csv", stringsAsFactors = F)
test<-read.csv("maste1_test.csv", stringsAsFactors = F)

train$GROUP<-as.character(train$GROUP)
train$GROUP[train$GROUP=="F20-"]<-"F20"
train$GROUP[train$GROUP=="F40+"]<-"F40"
train$GROUP[train$GROUP=="M20-"]<-"M20"
train$GROUP[train$GROUP=="M40+"]<-"M40"
train$GROUP<-as.factor(train$GROUP)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","gbm")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}


# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)


for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("master_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

###---------------------------------------------------------------------######



####대학원코드2(feature4 model)

rm(list=ls())
cs <- read.delim("train_clickstreams.tab", stringsAsFactors = F)
sk <- read.delim("train_searchkeywords.tab", stringsAsFactors = F)
profile <- read.csv("train_profiles.csv", stringsAsFactors = F, header = T)

cs_t <- read.delim("test_clickstreams.tab", stringsAsFactors = F)
sk_t <- read.delim("test_searchkeywords.tab", stringsAsFactors = F)

####총 페이지뷰 횟수
cs.V1 <- cs %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs.V2.1 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.1[,2:23] <- cs.V2.1[,2:23]/cs.V2.1[,24]
cs.V2.1 <- cs.V2.1[,-24]
names(cs.V2.1)[-1] <- paste("pv", names(cs.V2.1)[-1], sep="_")

cs.V2.2 <- cs %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.2[,2:208] <- cs.V2.2[,2:208]/cs.V2.2[,209]
cs.V2.2 <- cs.V2.2[,-209]
names(cs.V2.2)[-1] <- paste("mact", names(cs.V2.2)[-1], sep="_")

cs.V2 <- inner_join(cs.V2.1, cs.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs.V3 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs$TIME_ID <- ymd_h(cs$TIME_ID)
cs$date <- date(cs$TIME_ID)
cs$year <- year(cs$TIME_ID)
cs$month <- month(cs$TIME_ID)
cs$day <- day(cs$TIME_ID)
cs$time <- hour(cs$TIME_ID)
cs$wkday <- wday(cs$TIME_ID)

####총 방문 일수
cs.V4 <- cs %>%
  group_by(CUS_ID, date) %>%
  summarize(cs = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs.V5.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.mth[,2:13] <- cs.V5.mth[,2:13]/cs.V5.mth[,14]
cs.V5.mth <- cs.V5.mth[,-14]
names(cs.V5.mth)[-1] <- paste("mth", names(cs.V5.mth)[-1], sep="_")

#요일별
cs.V5.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs.V5.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.time[,2:25] <- cs.V5.time[,2:25]/cs.V5.time[,26]
cs.V5.time <- cs.V5.time[,-26]
names(cs.V5.time)[-1] <- paste("time", names(cs.V5.time)[-1], sep="_")

#merge
cs.V5 <- inner_join(cs.V5.mth, cs.V5.day, by=("CUS_ID")) %>%
  left_join(cs.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs.V6.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs.V6.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs.V6.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs.V6 <- inner_join(cs.V6.mth, cs.V6.day, by=("CUS_ID")) %>%
  left_join(cs.V6.time, by="CUS_ID")

####뉴스사이트 카테고리별 페이지뷰 비율
cs.V7 <- cs %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs.V7[,2:10] <- cs.V7[,2:10]/cs.V7[,11]
cs.V7 <- cs.V7[,-11]
names(cs.V7)[-1] <- paste("pv_news", names(cs.V7)[-1], sep="_")

####NA점검
anyNA(cs.V1)
anyNA(cs.V2)
anyNA(cs.V3)
anyNA(cs.V4)
anyNA(cs.V5)
anyNA(cs.V6)
anyNA(cs.V7)

#######최종merge
custsig.train <- profile %>%
  left_join (cs.V1) %>%
  left_join (cs.V2) %>%
  left_join (cs.V3) %>%
  left_join (cs.V4) %>%
  left_join (cs.V5) %>%
  left_join (cs.V6) %>%
  left_join (cs.V7) 

custsig.train[is.na(custsig.train)] <- 0

####변수명 조정
names(custsig.train) <- gsub(" ", "", names(custsig.train))
names(custsig.train) <- gsub("/", "", names(custsig.train))

names(custsig.train)[4:25]<-paste("pv",1:22,sep="")
names(custsig.train)[26:232]<-paste("mact",1:207,sep="")
names(custsig.train)[281:289]<-paste("pv_news",1:9,sep="")
write.csv(custsig.train, file="master2_train.csv",row.names = F)

#########Module 2 : Create Testing Set#########
####총 페이지뷰 횟수
cs_t.V1 <- cs_t %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs_t.V2.1 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.1[,2:23] <- cs_t.V2.1[,2:23]/cs_t.V2.1[,24]
cs_t.V2.1 <- cs_t.V2.1[,-24]
names(cs_t.V2.1)[-1] <- paste("pv", names(cs_t.V2.1)[-1], sep="_")

cs_t.V2.2 <- cs_t %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.2[,2:208] <- cs_t.V2.2[,2:208]/cs_t.V2.2[,209]
cs_t.V2.2 <- cs_t.V2.2[,-209]
names(cs_t.V2.2)[-1] <- paste("mact", names(cs_t.V2.2)[-1], sep="_")

cs_t.V2 <- inner_join(cs_t.V2.1, cs_t.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs_t.V3 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs_t$TIME_ID <- ymd_h(cs_t$TIME_ID)
cs_t$date <- date(cs_t$TIME_ID)
cs_t$year <- year(cs_t$TIME_ID)
cs_t$month <- month(cs_t$TIME_ID)
cs_t$day <- day(cs_t$TIME_ID)
cs_t$time <- hour(cs_t$TIME_ID)
cs_t$wkday <- wday(cs_t$TIME_ID)

####총 방문 일수
cs_t.V4 <- cs_t %>%
  group_by(CUS_ID, date) %>%
  summarize(cs_t = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs_t.V5.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.mth[,2:13] <- cs_t.V5.mth[,2:13]/cs_t.V5.mth[,14]
cs_t.V5.mth <- cs_t.V5.mth[,-14]
names(cs_t.V5.mth)[-1] <- paste("mth", names(cs_t.V5.mth)[-1], sep="_")

#요일별
cs_t.V5.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.day[,2:8] <- cs_t.V5.day[,2:8]/cs_t.V5.day[,9]
cs_t.V5.day <- cs_t.V5.day[,-9]
names(cs_t.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs_t.V5.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.time[,2:25] <- cs_t.V5.time[,2:25]/cs_t.V5.time[,26]
cs_t.V5.time <- cs_t.V5.time[,-26]
names(cs_t.V5.time)[-1] <- paste("time", names(cs_t.V5.time)[-1], sep="_")

#merge
cs_t.V5 <- inner_join(cs_t.V5.mth, cs_t.V5.day, by=("CUS_ID")) %>%
  left_join(cs_t.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs_t.V6.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs_t.V6.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs_t.V6.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs_t.V6 <- inner_join(cs_t.V6.mth, cs_t.V6.day, by=("CUS_ID")) %>%
  left_join(cs_t.V6.time, by="CUS_ID")


####뉴스사이트 카테고리별 페이지뷰 비율
cs_t.V7 <- cs_t %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs_t.V7[,2:10] <- cs_t.V7[,2:10]/cs_t.V7[,11]
cs_t.V7 <- cs_t.V7[,-11]
names(cs_t.V7)[-1] <- paste("pv_news", names(cs_t.V7)[-1], sep="_")


####NA점검
anyNA(cs_t.V1)
anyNA(cs_t.V2)
anyNA(cs_t.V3)
anyNA(cs_t.V4)
anyNA(cs_t.V5)
anyNA(cs_t.V6)
anyNA(cs_t.V7)

custsig.test <- cs_t.V1 %>%
  left_join (cs_t.V2) %>%
  left_join (cs_t.V3) %>%
  left_join (cs_t.V4) %>%
  left_join (cs_t.V5) %>%
  left_join (cs_t.V6) %>%
  left_join (cs_t.V7) 

custsig.test[is.na(custsig.test)] <- 0

####변수명 조정
names(custsig.test) <- gsub(" ", "", names(custsig.test))
names(custsig.test) <- gsub("/", "", names(custsig.test))

names(custsig.test)[3:24]<-paste("pv",1:22,sep="")
names(custsig.test)[25:231]<-paste("mact",1:207,sep="")
names(custsig.test)[280:288]<-paste("pv_news",1:9,sep="")
write.csv(custsig.test, file="master2_test.csv",row.names = F)

##대학원생2+word2vec 유사도(7조 코드 참조)
rm(list=ls())
custsig.train<-read.csv("master2_train.csv",stringsAsFactors = F)
custsig.test<-read.csv("master2_test.csv",stringsAsFactors = F)
sel.train<-read.csv("train_final_sel.csv",stringsAsFactors = F)
sel.test<-read.csv("test_final_sel.csv",stringsAsFactors = F)

sel.train<-sel.train[,c(1,47:64)]
sel.test<-sel.test[,c(1,46:63)]

train<-merge(custsig.train,sel.train,by="CUS_ID")
test<-merge(custsig.test,sel.test,by="CUS_ID")
write.csv(train,"master2_w2vsim_train.csv",row.names = F)
write.csv(test,"master2_w2vsim_test.csv",row.names = F)

####모델 적용(feature4) 
rm(list=ls())
train<-read.csv("master2_w2vsim_train.csv",stringsAsFactors = F)
test<-read.csv("master2_w2vsim_test.csv",stringsAsFactors = F)

train$GROUP<-as.character(train$GROUP)
train$GROUP[train$GROUP=="F20-"]<-"F20"
train$GROUP[train$GROUP=="F40+"]<-"F40"
train$GROUP[train$GROUP=="M20-"]<-"M20"
train$GROUP[train$GROUP=="M40+"]<-"M40"
train$GROUP<-as.factor(train$GROUP)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","gbm")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}


# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)



for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("master2_w2vsim_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}
##----------------------------------------------------------------------


#####8조 코드 응용(feature5 model)

### a 
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM의 총 ST_TIME 횟수  
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "ST_TIME")
ACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(ACTNM3)),1))

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "ST_TIME")
BACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(BACTNM3)),1))

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "ST_TIME")
MACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(MACTNM3)),1))

#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "ST_TIME")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))
#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$ST_TIME,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]


allD=merge(pf_all,BACTNM4,by="CUS_ID")
allD <- merge(allD,MACTNM4,by="CUS_ID")
allD <- merge(allD,ACTNM4,by="CUS_ID")
allD <- merge(allD,TIMETIME2,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)

write.csv(allD1,"allD1_st_prop.csv",row.names=F,quote=F)

allD1 <- read.csv("allD1_st_prop.csv")

train5 <- allD1[1:2500,]
test3 <- allD1[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"a.csv",row.names=F,quote=F)


##b
pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

all$count <- 1

#ACT_NM, BACT_NM, MACT_NM의 총 count 횟수  
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "count")

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "count")

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "count")

#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "count")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$count,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]



allD=merge(pf_all,BACTNM3,by="CUS_ID")
allD <- merge(allD,MACTNM3,by="CUS_ID")
allD <- merge(allD,ACTNM3,by="CUS_ID")
allD <- merge(allD,TIMETIME,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)

train5 <- allD1[1:2500,]
test3 <- allD1[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"b.csv",row.names=F,quote=F)

##c

pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM의 총 SITE_CNT 횟수  
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "SITE_CNT")
ACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(ACTNM3)),1))

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "SITE_CNT")
BACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(BACTNM3)),1))

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "SITE_CNT")
MACTNM4=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(MACTNM3)),1))

#시간대별 ST_TIEM
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "SITE_CNT")
TIMETIME2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(TIMETIME)),1))

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$SITE_CNT,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]



allD=merge(pf_all,BACTNM4,by="CUS_ID")
allD <- merge(allD,MACTNM4,by="CUS_ID")
allD <- merge(allD,ACTNM4,by="CUS_ID")
allD <- merge(allD,TIMETIME2,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)

train5 <- allD1[1:2500,]
test3 <- allD1[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)


predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"c.csv",row.names=F,quote=F)

##d
rm(list=ls())

pf <- read.xlsx("train_profiles.xlsx",header = T,1)
ck.train <- fread("train_clickstreams.tab")
ck.test <- fread("test_clickstreams.tab")
pft <- data.frame(CUS_ID=unique(ck.test$CUS_ID))
pft$GROUP <- NA
all <- rbind(ck.train,ck.test)
pf$CUS_ID <- as.character(pf$CUS_ID)
pft$CUS_ID <- as.character(pft$CUS_ID)
pf_all <- rbind(pf,pft)

#ACT_NM, BACT_NM, MACT_NM 
ACTNM3 <- cast(all, CUS_ID ~ ACT_NM, sum, value = "SITE_CNT")
write.csv(ACTNM3,"ACTNM3.csv",row.names = F)
ACTNM3<-read.csv("ACTNM3.csv",stringsAsFactors = F)

BACTNM3 <- cast(all, CUS_ID ~ BACT_NM, sum, value = "SITE_CNT")
write.csv(BACTNM3,"BACTNM3.csv",row.names = F)
BACTNM3<-read.csv("BACTNM3.csv",stringsAsFactors = F)

MACTNM3 <- cast(all, CUS_ID ~ MACT_NM, sum, value = "SITE_CNT")
write.csv(MACTNM3,"MACTNM3.csv",row.names = F)
MACTNM3<-read.csv("MACTNM3.csv",stringsAsFactors = F)

#시간대별 
all$HOUR <- substr(all$TIME_ID,9,10)
all$TIME=as.numeric(all$HOUR)
TIMETIME <- cast(all, CUS_ID ~ TIME, sum, value = "SITE_CNT")

#평일, 주말
all$DATE <- substr(all$TIME_ID,1,8)
all$DATE <- as.Date(as.character(all$DATE),"%Y%m%d")

day_levels <- c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일","일요일")
all$WDAY = as.numeric(factor(weekdays((all$DATE)), levels=day_levels, ordered=TRUE))

wd <- aggregate(all$SITE_CNT,by=list(CUS_ID=all$CUS_ID,WD=all$WDAY),sum)
wd1 <- cast(data = wd,CUS_ID~WD,value="x",sum)
wd2=as.data.frame.cast_matrix(prop.table(as.table(as.matrix(wd1)),1))

colnames(wd2) <- c("CUS_ID","MON","TUE","WED","TUR","FRI","SAT","SUN")

wd2$weekday<-wd2$MON+wd2$TUE+wd2$WED+wd2$TUR+wd2$FRI
wd2$weekend<-wd2$SAT+wd2$SUN

wd3<-wd2[,-8:-2]

allD=merge(pf_all,BACTNM3,by="CUS_ID")
allD <- merge(allD,MACTNM3,by="CUS_ID")
allD <- merge(allD,ACTNM3,by="CUS_ID")
allD <- merge(allD,TIMETIME,by="CUS_ID")
allD <- merge(allD,wd3,by="CUS_ID")

allD$CUS_ID <- as.numeric(allD$CUS_ID)
allD1 <- arrange(allD,CUS_ID)

train5 <- allD1[1:2500,]
test3 <- allD1[2501:5000,]
train5$GROUP<-substr(train5$GROUP,1,3)
test3<-test3[,-2]

train5<-na.omit(train5)
test3[is.na(test3)] <- 0
test3<-na.omit(test3)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(123)

xgb <- caret::train(GROUP ~ .,
                    data = subset(train5, select=-CUS_ID),
                    method = "xgbTree",
                    preProcess = NULL,
                    metric = "logLoss",
                    trControl = control)

predicted <- predict(xgb, subset(test3, select=-CUS_ID), type="prob")
pred <- cbind(test3$CUS_ID,predicted)
colnames(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"d.csv",row.names=F,quote=F)

####----------------------------------------------------------------

##feature6 model

#word2vec(sit_nm)


cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)


f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, SITE_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items1.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Build trans2vec model
set.seed(12345)
model = train_word2vec("items1.txt","vec1.bin",vectors=300,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec1.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, SITE_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

write.csv(train, "sitetvtrain.csv", row.names = F)
write.csv(test, "sitetvtest.csv", row.names = F)

train<-read.csv("sitetvtrain.csv")
test<-read.csv("sitetvtest.csv")
# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("gbm","rf","xgbTree","nnet","glmnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("sitenm2v_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#word2vec(site)

cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)


f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, SITE])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items_SITE.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Build trans2vec model
set.seed(12345)
model = train_word2vec("items_SITE.txt","vec_SITE.bin",vectors=300,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec1.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, SITE]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("gbm","rf","xgbTree","nnet","glmnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("site2v_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#word2vec(actnm)
rm(list=ls())

cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)


f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items_ACT.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Build trans2vec model
set.seed(12345)
model = train_word2vec("items_ACT.txt","vec_ACT.bin",vectors=500,threads=1,window=5,cbow=0,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec_ACT.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, ACT_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("gbm","rf","xgbTree","nnet","glmnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("act2v500cb0_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

##ACT_NM 300차원(Cbow=0)
set.seed(12345)
model = train_word2vec("items_ACT.txt","vec_ACT300.bin",vectors=300,threads=1,window=5,cbow=0,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec_ACT300.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, ACT_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("gbm","rf","xgbTree","nnet","glmnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("act2v300cb0_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

##ACTNM 300차원 Sapply40

f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:40, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items_ACT.txt", eol = " ", quote = F, row.names = F, col.names = F)




set.seed(12345)
model = train_word2vec("items_ACT.txt","vec_ACT300s40.bin",vectors=300,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec_ACT300s40.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, ACT_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("gbm","rf","xgbTree","nnet","glmnet")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)

for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("act2v300s40_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

##------------------------------------------------------------------------

##앙상블순서 

###1.같은 모델끼리 앙상블(괄호안에 왼쪽에 있는2개부터 순서대로 앙상블) 
#1.feature3 model앙상블(master_nnet,master_rf,master_gbm,master_xgbTree) 
#2.feature4 model앙상블(master2_w2vsim_gbm,master2_w2vsim_nnet,master2_w2vsim_xgbTree,master2_w2vsim_rf)
#3.feature1 model앙상블(7_rf,7_xgbTree) 
#4.feature2 model앙상블(7selctfea_gbm,7selctfea_xgbTree,7selctfea_rf)
#5.feature6 model앙상블 
###sitenm(sitenm2v_gbm,sitenm2v_nnet,sitenm2v_rf,sitenm2v_xgbTree,sitenm2v_glmnet)
###site(site2v_gbm,site2v__nnet,site2v_rf,site2v_xgbTree,site2v_glmnet)
###actnm((act2v500cb0_rf,act2v300cb0_xgbTree , act2v300s40_glmnet)
#6.feature5 model앙상블(b,c,d,a)

###2.로그로스가 높은 모델2개씩 앙상블
#####1.feature3,feature4앙상블
#####2.그후 feature1과 앙상블 
#####3.feature2와 앙상블
#####4.feature6과 앙상블
#####5.feature5와 앙상블 
#####6.8조꺼와 앙상블
#####7.3조꺼와 앙상블 
#####8.1조꺼와 앙상


nf <- 0
for (f in list.files(pattern="*.csv")) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}


nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}


pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "4조-final.csv", row.names = F)

###-----------------------------------------------------------------
#####그외 시도했지만 로그로스가 높아서 포기한 모델들
# IDA(로그로스 너무 뜀)

cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)

###### Make Corpus (sites sentences)

f <- function(x, dt) {
  itemfreq <- table(dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  act <- names(fitems)
  return(paste(act, collapse = " "))
}
md.dt$ACT_NM <- gsub(" ", "_", md.dt$ACT_NM); 
tr.t.dt$ACT_NM <- gsub(" ", "_", tr.t.dt$ACT_NM)
items <- unlist(sapply(cs.dt$CUS_ID, f, md.dt))
items <- c(items, unlist(sapply(unique(tr.t.dt$CUS_ID), f, tr.t.dt)))

##### create the document term matrix (DTM)
# DTM is a mathematical matrix that describes the frequency of terms that occur in a collection of documents.
# In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms. 

tic <- proc.time()
items.dtm <- DocumentTermMatrix(Corpus(VectorSource(items)))
print(proc.time() - tic)
# Reduce the number of terms 
#term_tfidf <- tapply(items.dtm$v/slam::row_sums(items.dtm)[items.dtm$i], items.dtm$j, mean) *
#  log2(tm::nDocs(items.dtm)/slam::col_sums(items.dtm > 0))
#summary(term_tfidf)
#items.reduced.dtm <- items.dtm[,term_tfidf >= 0.01]
#summary(slam::col_sums(items.reduced.dtm))

##### Run LDA model

tic <- proc.time()
lda.model <- LDA(items.dtm, k=20, method="Gibbs", control=list(burnin=1000, iter=1000, keep=50))
print(proc.time() - tic)
# Saving and loading lda model: 
# saveRDS(lda.model, "lda_model.rds") 
# lda.model <- readRDS("lda_model.rds")

##### Explore the model

items.topics <- topics(lda.model, 1)
# In this case I am returning the top 30 terms.
items.terms <- as.data.frame(terms(lda.model, 30), stringsAsFactors = FALSE)
#encoding for Hangeul handling
for (j in 1:length(items.terms)) Encoding(items.terms[,j]) <- "UTF-8"
items.terms[1:5]

##### Calculate the per document probabilities of the topics

items.theta <- as.data.frame(posterior(lda.model)$topics)
#head(theta[1:5])
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), items.theta[1:2500,], data.frame(GROUP=make.names(cs.dt$GROUP)))

# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
test <- cbind(data.frame(CUS_ID=test.CUS_ID), items.theta[2501:5000,])

##### Training & Prediction

# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
# Used models
methods <- c("gbm", "nnet","xgbTree","rf","glmnet") # # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
# saveRDS(models, "models.rds") 
# models <- readRDS("models.rds")

# Model comparison
results <- resamples(models)
summary(results)
dotplot(results)
modelCor(results)
splom(results)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("lda_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test.CUS_ID,pred[[1]]), fname, row.names = F)
}



##search keyword 300차원
sk <- read.delim("train_searchkeywords.tab", stringsAsFactors = F)
profile <- read.csv("train_profiles.csv", stringsAsFactors = F, header = T)

sk39<-profile 
s<-setdiff(unique(profile$CUS_ID),unique(sk$CUS_ID)) #search 안한고객 id 뽑아내기
for (i in s){
  sk39<- sk39[sk39$CUS_ID!=i,]
  
}
write.csv(sk39,"train39_profiles.csv",row.names = F)

#test전처리 
a<-read.csv("test_public.csv")
GROUP<-ifelse(a$F20.==1,"F20-",
              ifelse(a$F30==1,"F30",
                     ifelse(a$F40.==1,"F40+",
                            ifelse(a$M20.==1,"M20-",
                                   ifelse(a$M30==1,"M30","M40+")))))
a$GROUP<-GROUP
a<-a[,-2:-7]
write.csv(a,"test_profiles.csv",row.names = F)
profile_t<-read.csv("test_profiles.csv",stringsAsFactors = F, header = T)
sk_t <- read.delim("test_searchkeywords.tab", stringsAsFactors = F)
s_t<-intersect(unique(sk_t$CUS_ID),unique(profile_t$CUS_ID))
sk39_t<-data.frame()
for (i in s_t){
  a<- profile_t[profile_t$CUS_ID==i,]
  sk39_t<-rbind(sk39_t,a)
  
}
sk_tt<-data.frame() #테스트 프로필의 id와 검색한 고객아이디의 교집합test_searchkeywords
for (i in s_t){
  a<- sk_t[sk_t$CUS_ID==i,]
  sk_tt<-rbind(sk_tt,a)
  
}
length(unique(sk_tt$CUS_ID)) #확인 987
write.csv(sk_tt,"test_searchkeywords.csv",row.names = F)

#train data
tr.s<- fread("train_searchkeywords.tab")
tr.s[,CUS_ID:= as.numeric(CUS_ID)]
cs.dt <- fread("train39_profiles.csv")
setkey(cs.dt, CUS_ID); setkey(tr.s, CUS_ID) 

#test data
ts.s <- fread("test_searchkeywords.csv"); ts.s[,CUS_ID:= as.numeric(CUS_ID)]
setkey(ts.s, CUS_ID)

#불필요한 검색부분 삭제 
tr.s$QRY_STR <- gsub('&acr=0',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=1',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=2',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=3',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=4',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=5',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=6',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=7',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=8',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=9',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=10',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=11',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=12',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=13',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=14',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acr=15',' ',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&qdt=0','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&ie=utf8','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&query=','',tr.s$QRY_STR)

tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=0','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=1','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=2','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=3','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=4','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=5','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=6','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=7','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=8','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=9','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=10','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=11','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=12','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=13','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=14','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=15','',tr.s$QRY_STR)
tr.s$QRY_STR <- gsub('&acq=','',tr.s$QRY_STR)
#####################################################################
ts.s$QRY_STR <- gsub('&acr=0',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=1',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=2',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=3',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=4',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=5',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=6',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=7',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=8',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=9',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=10',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=11',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=12',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=13',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=14',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acr=15',' ',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&qdt=0','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&ie=utf8','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&query=','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=0','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=1','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=2','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=3','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=4','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=5','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=6','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=7','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=8','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=9','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=10','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=11','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=12','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=13','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=14','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&sm=top_sug.pre&fbm=15','',ts.s$QRY_STR)
ts.s$QRY_STR <- gsub('&acq=','',ts.s$QRY_STR)

md.dt <- merge(cs.dt, tr.s)#2461명 train 프로필과 서치키워드 머지

f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, QRY_STR])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "searchkeywords.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Build trans2vec model
set.seed(12345)
model = train_word2vec("searchkeywords.txt","searchkeywords.bin",vectors=300,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
#model <- read.binary.vectors("vec1.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x,QRY_STR ]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
write.csv(train, "searchtvtrain.csv", row.names = F)
# for test data
test.CUS_ID <- unique(ts.s$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, ts.s))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))
write.csv(test, "searchtvtest.csv", row.names = F)

train<-read.csv("searchtvtrain.csv")
test<-read.csv("searchtvtest.csv")
anyNA(train)
anyNA(test)
summary(is.na(train)) #na값 11개
summary(is.na(test))  #na값 12개
train<-na.omit(train)
test<-na.omit(test)

train_na<-c(255,515,802,1005,1679,1711,1858,1890,2110,2284,2312)
test_na<-c(2591,3098,3439,3478,3480,3553,3622,3904,4207,4437,4851,4947)


# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","pda")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("serachtv_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test.CUS_ID,pred[[1]]), fname, row.names = F)
}

#로그로스 확인용 test_profile 만들기

sk975_t<-sk39_t
for (i in test_na){
  sk975_t<- sk975_t[sk975_t$CUS_ID!=i,]
}
write.csv(sk975_t,"testsearch_public.csv",row.names = F) #na값 제외한 test고객 id ,group 

test.public <- read.csv("test_public.csv", stringsAsFactors = F)
test975<-data.frame()
for (i in test$CUS_ID){
  a<- test.public[test.public$CUS_ID==i,]
  test975<-rbind(test975,a)
}
write.csv(test975,"search_public.csv",row.names = F)



test.public <- read.csv("search_public.csv", stringsAsFactors = F)
actual <- as.matrix(test.public[,-1])

logLoss <- function(actual, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted,1-eps),eps)
  return(-1*sum(actual * log(predicted)) / nrow(actual))
}

eval <- data.frame(model=NULL,logloss=NULL)
path <- paste(getwd(),sep="")
for (f in list.files(path=path, pattern="*.csv")) {
  pred <- read.csv(paste(path, "/", f, sep=""))
  pred <- subset(pred, CUS_ID %in% test.public$CUS_ID)
  pred <- as.matrix(pred[,-1])
  pred <- pred / rowSums(pred) # required to sum to one
  acc <- logLoss(actual, pred)
  names(acc) <- NULL
  eval <- rbind(eval, data.frame(model=gsub(".csv","",f),logloss=acc))
}

eval <- eval[order(eval$logloss),]
eval$rank <- order(eval$logloss)
print(eval)



##200차원 
set.seed(12345)
model = train_word2vec("searchkeywords.txt","searchkeywords_vec200.bin",vectors=200,threads=2,window=5,cbow=1,negative_samples=10,iter=6,force = T)
#model <- read.binary.vectors("searchkeywords_vec200.bin") # reload the pre-trained word2vec model 


# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x,QRY_STR ]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
write.csv(train, "searchtvtrain_vec200.csv", row.names = F)
# for test data
test.CUS_ID <- unique(ts.s$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, ts.s))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))
write.csv(test, "searchtvtest_vec200.csv", row.names = F)

train<-read.csv("searchtvtrain_vec200.csv")
test<-read.csv("searchtvtest_Vec200.csv")
anyNA(train)
anyNA(test)
summary(is.na(train)) #na값 11개
summary(is.na(test))  #na값 12개
train<-na.omit(train)
test<-na.omit(test)

train_na<-c(255,515,802,1005,1679,1711,1858,1890,2110,2284,2312)
test_na<-c(2591,3098,3439,3478,3480,3553,3622,3904,4207,4437,4851,4947)


# Control parameters for model training
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","pda")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}



# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("serachtv_vec200_", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#로그로스 확인용 test_profile 만들기

test.public <- read.csv("search_public.csv", stringsAsFactors = F)
actual <- as.matrix(test.public[,-1])

logLoss <- function(actual, predicted) {
  eps <- 1e-15
  predicted <- pmax(pmin(predicted,1-eps),eps)
  return(-1*sum(actual * log(predicted)) / nrow(actual))
}

eval <- data.frame(model=NULL,logloss=NULL)
path <- paste(getwd(),sep="")
for (f in list.files(path=path, pattern="*.csv")) {
  pred <- read.csv(paste(path, "/", f, sep=""))
  pred <- subset(pred, CUS_ID %in% test.public$CUS_ID)
  pred <- as.matrix(pred[,-1])
  pred <- pred / rowSums(pred) # required to sum to one
  acc <- logLoss(actual, pred)
  names(acc) <- NULL
  eval <- rbind(eval, data.frame(model=gsub(".csv","",f),logloss=acc))
}

eval <- eval[order(eval$logloss),]
eval$rank <- order(eval$logloss)
print(eval)

#머지 
train1<-read.csv("searchtvtrain_vec200.csv")
test1<-read.csv("searchtvtest_Vec200.csv")
train2<-read.csv("searchtvtrain.csv")
test2<-read.csv("searchtvtest.csv")

train<-merge(train1,train2,by="CUS_ID")
test<-merge(test1,test2,by="CUS_ID")

train<-na.omit(train)
test<-na.omit(test)
train$GROUP.x<-NULL
names(train)[502]<-"GROUP"

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
methods <- c("rf","xgbTree","nnet","pda")
models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
}

# Model comparison
results <- resamples(models)
summary(results)
xyplot(results)
modelCor(results)
splom(results)
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("serachtv_merge200", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}
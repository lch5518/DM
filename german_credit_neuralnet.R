rm(list=ls())
setwd('C:/Users/dydeh/4학년2학기/DM')
getwd()

# install.packages("dplyr")
# install.packages("data.table")
# install.packages("caret")
# install.packages("ROCR")
# install.packages("neuralnet")
# install.packages("NeuralNetTools")
# install.packages('curl')
# install.packages('e1071', dependencies=TRUE)


library("dplyr")
library("data.table")

library("caret")
library("ROCR")

library("neuralnet")
# 위 인공신경망 모델에서 각 변수의 중요도 확인
library("NeuralNetTools")
library("curl")
library("e1071")

data.file <- "https://raw.githubusercontent.com/adriangasinski/datahacking_0001/master/german_credit.csv"

# stringsAsFactors : 문자열 그대로 읽어오기
data.raw <- fread(data.file, stringsAsFactors=T) 


write.csv(data.raw, "german_credit.csv", row.names=F)

data.raw %>% dim()
data.raw %>% head()
data.raw %>% summary()

data.raw %>% names()

data.raw %>% select(default) %>% class()
data.raw %>% select(default) %>% table()

data.raw %>% sapply(class)

### 숫자로 변환
data.use <- data.raw %>% sapply(as.numeric)
data.use %>% head()

### 데이터 분할
inTrain <- createDataPartition(y=data.use[,"default"], p=0.6, list=FALSE)  # 학습,테스트데이터 나누기
data.train <- data.use[inTrain,]
data.test <- data.use[-inTrain,]

### 볌위 [0, 1] 조정
data.train.pp <- data.train %>% preProcess(method="range")
data.train <- predict(data.train.pp, data.train)
data.test <- predict(data.train.pp, data.test)

### 모델링
nnet.model.1 <- neuralnet(default~., data=data.train, hidden=5, threshold=0.01)
nnet.model.2 <- neuralnet(default~., data=data.train, hidden=c(2,2),  threshold=0.01)

# threshold : error 감소분이 threshold 값보다 작으면 stop
# hidden : hidden node의 수
# hidden=c(10,5) : hidden layer 2개, 각각 hidden node 10, 5개
# linear.output: 활성함수('logistic' or 'tanh')가 출력 뉴런에 적용되지 않아야 하는 경우(즉, 회귀) TRUE로 설정(default)
# stepmax: 최대 반복 횟수

### 신경망 구조 다이어그램: 해석하지 않음
plot(nnet.model.1)

### 변수 중요도
garson(nnet.model.1) + coord_flip()

plot(nnet.model.2)
# Garsons algorithm not applicable for multiple hidden layers
garson(nnet.model.2)

### prediction - nnet.model.1
nnet.1.pred.prob <- compute(nnet.model.1, covariate=data.test)$net.result[,1]
nnet.1.pred <- ifelse(nnet.1.pred.prob > 0.5, 1, 0)
tb <- table(nnet.1.pred, data.test[,"default"])
confusionMatrix(tb)

### prediction - nnet.model.2
nnet.2.pred.prob <- compute(nnet.model.2, covariate=data.test)$net.result[,1]
nnet.2.pred <- ifelse(nnet.2.pred.prob > 0.5, 1, 0)
tb <- table(nnet.2.pred, data.test[,"default"])
confusionMatrix(tb)

### Model Comparison
## ROC curve (Receiver Operating Characteristic curve) - nnet.model.1
nnet.1.pred <- ROCR::prediction(nnet.1.pred.prob, data.test[,"default"])
nnet.model.1.roc <- performance(nnet.1.pred, "tpr", "fpr")

## ROC curve - nnet.model.2
nnet.2.pred <- ROCR::prediction(nnet.2.pred.prob, data.test[,"default"])
nnet.model.2.roc <- performance(nnet.2.pred, "tpr", "fpr")

## ROC curve plot
plot(nnet.model.1.roc, col="red")
plot(nnet.model.2.roc, col="green", add=T)
abline(0,1)

## AUC - the Area Under a ROC Curve
performance(nnet.1.pred, "auc")
performance(nnet.2.pred, "auc")


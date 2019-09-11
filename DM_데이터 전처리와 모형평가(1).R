
#############
#영-과 영근처-분산 예측변수의 처리

install.packages("dplyr")
install.packages("caret")

library(dplyr)
library(caret)


data(mdrr)
dim(mdrrDescr)
names(mdrrDescr)  #변수이름 확인하기
data.frame(table(mdrrDescr$nR11))
class(mdrrDescr)

summary(mdrrDescr)  #변수들의 요약값

tb <- table(mdrrDescr$nR11) 
var(mdrrDescr$nR11)

barplot(tb)


nzv <- nearZeroVar(mdrrDescr)
nzv  # 영 근처 분산을 가지는 변수
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)


###상관된 예측변수의 식별:중복변수 제거

##
data <- iris[-5]
data.cor <- cor(data)
up <- upper.tri(data.cor)
data.cor[up]
##

descrCor <- cor(filteredDescr)
upper.tri(descrCor)
(highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999))  # 상관계수가 0.999 이상인 경우의 개수
summary(descrCor[upper.tri(descrCor)])

cor.value <- descrCor[upper.tri(descrCor)]

boxplot(cor.value)

hist(cor.value)

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
filteredDescr <- filteredDescr[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])



### 예측변수의 변환

##중심화와 척도화
set.seed(200)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)   # == sample(1:528, 528/2), 학습0.5 테스트0.5
training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain, ]
trainMDRR <- mdrrClass[inTrain]
testMDrr <- mdrrClass[-inTrain]

#중심화 척도화 수행
preProcValues <- preProcess(training, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

sapply(trainTransformed,mean)
sapply(testTransformed,sd)

# 박스-콕스 변환
preProcValues2 <- preProcess(training, method = "BoxCox")
trainBC <- predict(preProcValues2, training)
testBC <- predict(preProcValues2, test)
preProcValues2

hist(training$AMW)
hist(trainBC$AMW)



install.packages("party")

library(dplyr)
library(rpart)
library(party)

data(stagec)  # stagec는 {rpart}에서 제공
stagec %>% dim()
stagec %>% head()
?stagec
str(stagec)
summary(stagec)

# 결측값 제거
stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))

stagec3 <- na.omit(stagec) # na값을 모두 지워준다.

stagec3 %>% dim()

set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7,0.3))
table(ind)

trainData <- stagec3[ind==1,]
testData <- stagec3[ind==2,]

trainData %>% dim()
testData %>% dim()

tree <- ctree(ploidy~., data=trainData) # trainData를 이용해서 ploidy를 제외한 7개의 변수를 사용
tree
plot(tree)

testPred <- predict(tree, newdata=testData)
testPred

# 예측을 잘 했는지 교차표만들어보기
table(testData$ploidy, testPred)

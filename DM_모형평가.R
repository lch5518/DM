install.packages("dplyr")
install.packages("caret")

library(dplyr)
library(caret)

##### 모형평가
head(iris)
plot(iris[-5],col=iris[[5]])

### data
data.use <- subset(iris, Species == "setosa" | Species == "Virginica")
data.use$Sqecies <- factor(data.use$Species)
str(data.use)
table(data.use$Species)


### Logistic regression
res.glm <- glm(Species~Sepal.Length, data= data.use, family=binomial)
summary(res.glm)

pred.glm <- predict(res.glm, data.use, type="response")
pred.glm.str <- ifelse(pred.glm<0.5, "setosa","virginica")
pred.glm.factor <- factor(pred.glm.str)

table(data.use$Species, pred.glm.factor)


### SVM
install.packages("e1071")
library(e1071)

res.svm <- svm(Species~Sepal.Length, data=data.use)
summary(res.svm)

pred.svm <- predict(res.svm, data.use, type="response")
table(data.use$Species, pred.svm)

#### ROC & AUC
install.packages("pROC")
library(pROC)

### SVM
response.value <- as.integer(data.use$Species)-1
predict.value <- as.integer(pred.svm)-1
roc.svm <- roc(response.value)

### ROC test
roc.test(roc.glm,roc.svm)





##### 로지스틱 회귀 모형
data(iris)
a <- subset(iris, Species == "setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
as.numeric(a$Species)

str(a)


b <- glm(Species~Sepal.Length, data=a, family=binomial)
summary(b)

coef(b)

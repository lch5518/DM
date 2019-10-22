###### 모형평가
head(iris)

### Data
data.use <- subset(iris, Species=="setosa" | Species=="virginica")
data.use$Species <- factor(data.use$Species)
str(data.use)
table(data.use$Species)


### Logistic regression
res.glm <- glm(Species~Sepal.Length, data=data.use, family=binomial)
summary(res.glm)

pred.glm <- predict(res.glm, data.use, type="response")
pred.glm.str <- ifelse(pred.glm<0.05,"setosa","virginica")
pred.glm.factor <- factor(pred.glm.str)

table(data.use$Species, pred.glm.factor)

### SVM
# install.packages("e1071")
library("e1071")

res.svm <- svm(Species~Sepal.Length, data=data.use)
summary(res.svm)

pred.svm <- predict(res.svm, data.use, type="response")

table(data.use$Species, pred.svm)

###### ROC & AUC
# install.packages("pROC")
library("pROC")

### Logistic regression
response.value <- as.integer(data.use$Species)-1
predict.value <- as.integer(pred.glm.factor)-1
roc.glm <- roc(response.value, predict.value)
plot(roc.glm, col="red", print.auc=T)

### SVM
response.value <- as.integer(data.use$Species)-1
predict.value <- as.integer(pred.svm)-1
roc.svm <- roc(response.value, predict.value)
plot(roc.svm, col="blue", print.auc=T, add=T, print.auc.adj=c(1.11,1.2))

### ROC test
roc.test(roc.glm,roc.svm)


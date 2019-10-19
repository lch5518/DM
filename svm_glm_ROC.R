rm(list=ls())
# 20140736 이창현

install.packages("pROC")
install.packages("dplyr")
install.packages("e1071")
library(pROC)
library(dplyr)
library(e1071)

### 0. 데이터가 위치하고있는 디렉토리 설정
getwd()
choose.dir()
setwd("C:/Users/dydeh/Downloads")


### 1. “Loan payments data.csv” 자료를 data.raw 변수에 저장
data.raw <- read.csv('Loan payments data.csv', head=T)
data.raw %>% head() 


### 2. 분석에 필요한 목표 변수와 입력 변수를 선택하여 data.use에 저장
data.use <- data.raw %>% select(loan_status, Principal, terms, age, education, Gender)
data.use %>% head()


### 3. 로지스틱 회귀모형을 구축
data.use$loan_status <- as.numeric(data.use$loan_status)
data.use$education <- as.factor(data.use$education)
data.use$Gender <- as.numeric(data.use$Gender)

out.var.name <- "loan_status"
data.use[[out.var.name]] <- data.use[[out.var.name]]-1

result.glm <- glm(loan_status~., data=data.use, family=binomial)
result.glm %>% summary()



### 4. 로지스틱 회귀모형에 대한 정오분류표
pred.glm <- predict(result.glm, data.use, type="response", probability=TRUE)
pred.glm.class <- ifelse(pred.glm<0.05,0,1)

table(data.use$loan_status, pred.glm.class)  



### 서포트벡터머신(SVM) 모형 구축
result.svm <- svm(loan_status ~ ., data = data.use,
                  probability=TRUE, 
                  type = "C-classification",
                  kernel = "radial", 
                  gamma = 1, 
                  cost=100)

pred.svm <- predict(result.svm, data.use)


### SVM 모형에 대한 정오분류표
table(data.use$loan_status, pred.svm)



### ROC curve
pred.svm <- predict(result.svm, data.use,probability=TRUE) # 예측값 -> 예측확률
predict.value <- attr(pred.svm, "probabilities") # 메타데이터 지정
predict.value.svm <- predict.value %>% as.data.frame()


roc.svm <- roc(data.use$loan_status, 
               predict.value.svm$`1`, # svm모형
               plot=T,
               percent=TRUE, # 퍼센트로 표현
               print.auc=TRUE, # auc출력
               print.auc.adj=c(1,-4), # 글자위치
               col='blue'
               )

roc.glm <- roc(data.use$loan_status,
               pred.glm, # glm모형(로지스틱)
               plot=T,
               percent=TRUE,
               print.auc=TRUE,
               print.auc.adj=c(0,-1),
               col='red',
               add=T
               )


### ROC test
roc.test(roc.svm,roc.glm)



library(dplyr)
library(e1071)

data(iris)

x <- c(1:20)
y <- c(3,4,8,4,6,9,8,12,15,26,35,40,45,54,49,59,60,62,63,68)
data <- data.frame(x,y)

plot(data, pch=16)  

## 회귀모형
model <- lm(y ~ x, data)
model %>% summary()
abline(model, col = 'red')

model %>% mode()
model %>% names()


#lm.error <- model$residuals
#(lmRMSE <- sqrt(mean(lm.error^2)))
lmRMSE <- model$residuals^2 %>% mean() %>% sqrt()


## SVM
model <- svm(y ~ x, data=data)
model %>% mode()
model %>% names()

svmRMSE <- model$residuals^2 %>% mean() %>% sqrt()

# lm보다 svm의 RMSE의 값이 더 작으므로 svm모델이 더 좋아! 오류는 작은게 좋지 암

# cost 값과 gamma를 조정하여 최적의 모형을 만들 수 있다아
# tune함수는 튜닝을하여 최적의 모수를 찾아줌
tune.svm.res <- tune.svm(y ~ x, data = data, gamma = c(0.1,0.5,1), cost = 2^(2:9))
tune.svm.res

model <- svm(y ~ x, data = data, gamma = 0.5, cost=256)
model$residuals^2 %>% mean() %>% sqrt()

svmRMSE
# 튜닝하여 얻은 감마와 코스트를 사용하여 모델을 만들었더니 RMSE 값이 1정도 줄었다
# cost값이 너무 크면 과적합이 될 수 있으므로 주의할것!

pred.svm <- predict(model, data)
plot(x,pred.svm, type="o")
lines(x,y,type="o", col="red") # 이전의 선 추가로 긋기

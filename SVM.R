install.packages("e1071")
install.packages("dplyr")

library(e1071)
library(dplyr)

data(iris)
iris %>% head()

# gamma : 선형을 제외한 모든 커널에 요구되는 모수, 기본값은 1/(데이터 차원)
# cost : 제약 위배의 비용, 기본값은 1
# kernel : linear/polynomial/radial/sigmoid
svm.e1071 <- svm(Species ~. , data = iris, type = "C-classification",
                kernel = "radial", cost = 10, gamma = 0.1) 
?svm
summary(svm.e1071)

plot(svm.e1071, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred <- predict(svm.e1071, iris, decision.values = T)

plot(svm.e1071, iris, Sepal.Width ~ Sepal.Length, slice = list(Petal.Width = 2.5, Petal.Length = 3))

(acc <- table(pred, iris$Species))


rm(list = ls())

library(dplyr)

wd <- "C:/Users/Admin/Downloads"
setwd(wd)
getwd()

data.file <- "german_05.txt"
data.raw <- read.table(data.file,header=T, encoding = "UTF-8")
data.raw %>% dim()
data.raw %>% head()

data.use <- data.raw

####### Out variable
out.var.name <- "class"

data.use[[out.var.name]] <- data.use[[out.var.name]] - 1
data.use %>% head()
data.use[[out.var.name]] %>% table()

####### training / test data 나누기
training.ratio <- 0.5

### training
training.data <- data.use[0,]  ## 구조만 복사하기
training.data

data.size <- data.use %>% nrow()
data.size

training.data <- data.size * training.ratio
training.data %>% table()

training.index <- sample(1:data.size, data.size/2) ## 1:1000, 500 : 1부터 1000사이의 값 500개
training.data <- rbind(training.data, data.use[training.index,])

training.data$class %>% table()

test.data <- data.use[-training.index,]
test.data$class %>% table()


### Logistic regression
res.glm <- glm(class~., data=data.use, family=binomial)
summary(res.glm)

res.glm.step <- step(res.glm)  #arc 필요없는 변수 제거하기
summary(res.glm.step)


pred.glm <- predict(res.glm.step, test.data, type="response")  # 0~1사이의 값.
pred.glm.class <- ifelse(pred.glm<0.05,0,1)

table(test.data$class, pred.glm.class)

### 정확도, 민감도, 특이도


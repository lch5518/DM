rm(list = ls())

library(dplyr)

choose.dir()
wd <- "C:/Users/dydeh/4학년2학기/DM"
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

###### Over sampling
if(F){
  tb <- training.data$class %>% table()
  tb
  over.n <- tb[1]-tb[2]
  training.over.idx <- sample(1:tb[2], over.n, T)
  training.data.bad <- training.data %>% filter(class==1)
  training.data <- rbind(training.data, training.data.bad[training.over.idx,])
  training.data$class %>% table()
}


### test
test.data <- data.use[-training.index,]
test.data$class %>% table()


### Logistic regression
res.glm <- glm(class~., data=data.use, family=binomial) # class~. : class변수만 제외하고 모두 다
summary(res.glm)

res.glm.step <- step(res.glm)  #aic 필요없는 변수 제거하기
summary(res.glm.step)


pred.glm <- predict(res.glm.step, test.data, type="response")  # 0~1사이의 값.
pred.glm.class <- ifelse(pred.glm<0.05,0,1)


### 정확도, 민감도, 특이도
###### cross table
tb<-table(test.data$class, pred.glm.class)
tptn <- sum(diag(tb)) # 제대로 예측한 값
n <- sum(tb) # 테이블값을 모두 더한 값

tptn/n # 정분류율(정확도)
tb[1,1]/(tb[1,1]+tb[1,2]) # 민감도
tb[2,2]/(tb[2,1]+tb[2,2]) # 특이도


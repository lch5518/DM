rm(list=ls())

library(dplyr)

###### Set Working Directory
wd <- "C:/data"

setwd(wd)

list.files()

###### Data file
data.file <- "german_05.txt"

data.raw <- read.table(data.file, header=T, sep="", encoding="UTF-8")
data.raw %>% dim()
data.raw %>% head()

data.use <- data.raw

###### Out variable
out.var.name <- "class"

data.use[[out.var.name]] <- data.use[[out.var.name]]-1
data.use[[out.var.name]] %>% table()

###### training/test data
training.ratio <- 0.5

### training
training.data <- data.use[0,]

data.size <- nrow(data.use)
training.size <- data.size * training.ratio
training.idx <- sample(1:data.size, training.size)
training.data <- rbind(training.data, data.use[training.idx,])

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
test.data <- data.use[0,]

test.data <- rbind(test.data, data.use[-training.idx,])
test.data$class %>% table()

###### Logistic regression
res.glm <- glm(class~., data=training.data, family=binomial)
res.glm.summ <- summary(res.glm)
res.glm.summ

###### step - AIC
res.glm.step <-  step(res.glm)
res.glm.step.summ <- summary(res.glm.step)
res.glm.step.summ

pred.glm <- predict(res.glm, test.data, type="response")
pred.glm.class <- ifelse(pred.glm<0.5,0,1)

###### cross table
table(test.data$class, pred.glm.class)

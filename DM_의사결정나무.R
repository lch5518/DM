rm(list=ls())

#### 의사결정나무
install.packages("rpart")
library(rpart)
library(dplyr)

head(iris)
c <- rpart(Species~., data=iris)
c

plot(c, compress=T, margin=0.3)
text(c, cex=1.2)

c.pred <- predict(c, newdata=iris, type = "class")
table(iris$Species, c.pred)

install.packages("rpart.plot")
library(rpart.plot)

prp(c, type=4, extra = 2)

ls(c)
  
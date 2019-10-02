install.packages("nnet")
install.packages("devtools")

library(nnet)
library(dplyr)

head(iris)
# size : 노드 개수(hidden layer/층수), maxit : 반복횟수, decay : 5e-04가 디폴트, rang : [-rang:rang] 무작위 가중
nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
?nnet
nn.iris  %>% summary()



library(devtools)

source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn.iris)


pred.iris <- predict(nn.iris, iris, type="class")
table(iris$Species, pred.iris)



### <ex2>
data(infert, package="datasets")
str(infert)
infert %>% dim()
infert %>% head()

install.packages("neuralnet")
library(neuralnet)
?neuralnet
net.infert <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2, err.fct="ce", 
                        linear.output=FALSE, likelihood=TRUE)

net.infert
plot(net.infert)

names(net.infert)

net.infert$result.matrix

# 적합값은 $net.result에 제공
out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity", "induced", "spontaneous", "nn-output"))
head(out)

# 가중치의 초깃값과 적합값은 $startweights와 $weights에 제공
# 일반화 가중치
head(net.infert$generalized.weights[[1]])

# 일반화 가중치 시각화
par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate = "age", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "parity", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "induced", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "spontaneous", min=-2.5, max=5)
par(mfrow=c(1,1))

new.output <- compute(net.infert, covariate=matrix(c(22,1,0,0,
                                                     22,1,1,0,
                                                     22,1,0,1,
                                                     22,1,1,1),
                                                   byrow=TRUE, ncol=4))

# 공변량 조합에 대한 예측결과, 사전 낙태의 수에 따라 예측확률이 증가함을 보여줌
new.output$net.result


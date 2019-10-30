# 계층분석
library(dplyr)

## ex01
data(USArrests)
str(USArrests)
USArrests %>% head()

?dist
# method - euclidean, maximum, manhattan, binary, minkowski
d <- dist(USArrests, method = "euclidean")
d %>% str()

?hclust
# method- complete(완전연결법), ward, single, average, centroid
fit <- hclust(d,method = "average")
fit

par(mfrow=c(1,2))
# 높이가 높으면 거리가 멀리 떨어져 있는것
plot(fit)
plot(fit, hang = -1)
par(mfrow=c(1,1))

# 군집 만들기
groups <- cutree(fit, k=5)
groups

# 시각화를 해보자
plot(fit, hang = -1)
rect.hclust(fit, k=4, border="red")

plot(fit, hang = -1)
rect.hclust(fit, h=60, border="red")

plot(fit, hang = -1)
rect.hclust(fit, k=4, border=rainbow(4))

plot(fit, hang = -1)
rect.hclust(fit, h=60, which= c(1:3), border=3:4) # which : n번째 군집만 표시



## ex02
install.packages("ape")
library(ape)
library(cluster)

agn1 <- agnes(USArrests, metric = "mangattan", stand = TRUE) # stand: 표준화 여부
agn1
par(mfrow=c(1,2))
plot(agn1, hang=-1)
par(mfrow=c(1,1))

?daisy # 비유사성 행렬을 계산
x11()
agn2 <- agnes(daisy(USArrests), diss=T, method = "complete")
plot(agn2, hang=-1)



## ex03
fit <- hclust(d, method="ave")
clus6 <- cutree(fit, 6)

colors <- rainbow(6) # 색상 선ㅌ

plot(as.phylo(fit), type= "fan", tip.color = colors[clus6], label.offset = 1, cex = 0.7)

plot(as.phylo(fit), type = "cladogram", cex = 0.6, label.offset=0.5)

plot(as.phylo(fit), type = "unrooted", cex = 0.6, no.margin=TRUE)

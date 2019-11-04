# K-평균군집

install.packages("rattle")
library(dplyr)
library(rattle)

data(wine)

# K-means에는 여러가지 라이브러리가 있다. 
install.packages("NbClust")
library(NbClust)

wine %>% head()
wine %>% summary()  # type의 컬럼만 이산형데이터로 구성됨

df <- scale(wine[-1]) # 첫 번째(type컬럼) 데이터 제거
df

set.seed(1234)
# nc : number of clust
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")  # 결과창을 보면 3개의 군집이 적당함을 알 수 있음
table(nc$Best.nc[1,])

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25) # nstart : 다중의 초깃값 수
fit.km
fit.km$size
fit.km$centers

#par(mfrow=c(1,1))
plot(df, col=fit.km$cluster)
points(fit.km$centers, col=1:3, pch=8, cex=1.5) # pch : 점종류, cex : 점크기

# 각 군집별로 변수의 요약값을 측정단위의 척도로 표시
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# 정오분류표
ct.km <- table(wine$Type, fit.km$cluster)
ct.km

# 군집 간의 일지도(agreement)
install.packages("flexclust")
library(flexclust)
randIndex(ct.km)

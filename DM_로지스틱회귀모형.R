##### 로지스틱 회귀 모형
data(iris)
a <- subset(iris, Species == "setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
as.numeric(a$Species)

str(a)


b <- glm(Species~Sepal.Length, data=a, family=binomial)
summary(b)

coef(b)
exp(coef(b))

#신뢰구간 확인하기
confint.sl <- confint(b, parm = "Sepal.Length")
exp(confint.sl)


fitted(b)

predict(b, newdata=a[c(1,50,51,100),],type = "response")
predict(b, newdata=a[1:5,], type = "response")
predict(b, newdata=a[95:100,], type = "response")

# cdplot() 함수는 번주형 변수의 조건부분포를 보여줌
cdplot(Species~Sepal.Length, data=a)

plot(a$Sepal.Length, a$Species, xlab="Sepal.Length")
x=seq(min(a$Sepal.Length), max(a$Sepal.Length),0.1)
lines(x, 1+(1/(1+(1/exp(-27.831+5.140*x)))),type="l",col="red")



### 예제2
attach(mtcars)
str(mtcars)
glm.vs <- glm(vs~mpg+am, data = mtcars, family=binomial)
summary(glm.vs)


# direction = c("both", "backward", "forward")
# both : 전진후진 반복, backward : 후진, forwad : 전진
step.vs <- step(glm.vs, direction="backward") 
summary(step.vs)

ls(glm.vs)
str(glm.vs)

anova(glm.vs, test="Chisq")

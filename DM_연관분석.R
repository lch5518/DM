## 연관분석

install.packages("arules")
install.packages("arulesViz")

library(dplyr)
library(arules)
library(arulesViz)

data.uk <- choose.files() %>% read.csv()
data.uk %>% dim()
data.uk %>% head()
data.uk %>% summary()

Customer.Desc <- split(data.uk$Description, data.uk$CustomerID)
Customer.Desc.trans <- as(Customer.Desc, "transactions")
Customer.Desc.trans %>% summary()

?apriori
Customer.Desc.rules <- apriori(Customer.Desc.trans, 
                               parameter = list(minlen=2, support=0.07,confidence=0.7))
Customer.Desc.rules %>% summary()

# 텍스트형태로 연관분석을 보여줌
inspect(Customer.Desc.rules)


# 시각화
plot(Customer.Desc.rules)
plot(Customer.Desc.rules, method='grouped')
plot(Customer.Desc.rules, method='graph')
plot(Customer.Desc.rules, method='graph', engine='interactive')

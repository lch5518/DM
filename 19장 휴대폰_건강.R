### 사례 분석: 한국어 자료
install.packages("tm")
install.packages("KoNLP")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("rJava")
# 패키지 로딩
library(tm)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)
library(rJava)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_231')


## (1) 데이터 불러오기
휴대폰 <- read.csv(choose.files(), header=F)
str(휴대폰)

휴대폰 <- 휴대폰&body
str(휴대폰)

## (2) 불용어 제거
휴대폰 <- gsub("[[:space:]]", " ", 휴대폰)
휴대폰 <- gsub("\n", "", 휴대폰)
휴대폰 <- gsub("\r", "", 휴대폰)
휴대폰 <- gsub("\\n", "", 휴대폰)
휴대폰 <- gsub("\\n)", "", 휴대폰)
휴대폰 <- gsub("http://[[:graph:]]*","", 휴대폰)
휴대폰 <- gsub("https///[[:graph:]]*","", 휴대폰)
휴대폰 <- gsub("///[[:graph:]]*","", 휴대폰)
휴대폰 <- gsub("@[[:graph:]]*","",휴대폰)
휴대폰 <- gsub("RT","", 휴대폰)
휴대폰 <- gsub("@","", 휴대폰)
휴대폰 <- gsub("\\(","",휴대폰)
휴대폰 <- gsub("\\d)","",휴대폰)

## (3) 형태소 분류 -> 명사만 추출하기 위해
# 세종 사전 불러오기: 한글 처리에 필요
useSejongDic()
# mergeUserDic(data.frame(c("대한민국"), c("ncn"))) # 필요시 추가
tran <- Map(extractNoun, 휴대폰) # 명사만 추출
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y) {
nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) } # 한글 자료만 추출
,x)} )
tran <- Filter(function(x){length(x) >= 2}, tran) # 길이가 2 이상인
단어만 추출
names(tran) <- paste("Tr", 1:length(tran), sep="")

tran[1]

# 모든 명사를 추출, 저장
휴대폰word <- data.frame(unlist(tran))
write.csv(휴대폰word, "C:/휴대폰word.csv")
# 저장된 csv 파일의 형태(휴대폰word.csv)

## (4) 워드클라우드 분석
word <- read.csv("C:/휴대폰word.csv", header = T)
# word <- melt(tran)
wordcount <- table(word[,2]) # 단어 카운트
head(sort(wordcount, decreasing = T), 100)
names(head(sort(wordcount,decreasing = T), 100)) # 상위 100개만 보기
palete <- brewer.pal(8,"Dark2") # 글자색 지정
wordcloud(names(wordcount), freq=wordcount, scale=c(3,0.5), rot.per=0.25, min.freq=30, random.order=F, random.color=T, colors=palete)










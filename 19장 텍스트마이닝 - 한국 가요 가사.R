### install rJava
install.packages("rJava")
source("https://install-github.me/talgalili/installr")
installr::install.java()
library(rJava)

###### 한국 가요 50년, 가사

install.packages("KoNLP")
install.packages("wordcloud2")
install.packages("stringr")
install.packages("ggplot2")

install.packages("dplyr")
install.packages("data.table")

library("KoNLP")
library("wordcloud2")
library("stringr")
library("ggplot2")

library("dplyr")
library("data.table")

# dir <- choose.dir()
# setwd(dir)
# getwd()

### SimplePos09 함수로 명사 추출하는 함수
fn.extractNoun.sp09 <- function(str){
	str.sp <- SimplePos09(str)
	value.noun <- regmatches(str.sp, regexpr('(([가-힣]+)/N)', str.sp))
	value.noun <- gsub("/N$","",value.noun)
	value.noun
}
fn.extractNoun.sp09("오빤 강남스타일 강남스타일 낮에는 따사로운 인간적인 여자 Eh Sexy Lady 오오오오 ")

### 가사에서 추출된 명사 중 중복 제거
fn.extractNoun <- function(x){
	str.lyric <- x
	#value.noun <- extractNoun(str.lyric)
	value.noun <- fn.extractNoun.sp09(str.lyric)
	value.noun.uni <- value.noun %>% unique()
}

### use Insighter and Woorimalsam dictionary
useNIADic()

choose.files()
data.file <- choose.files()
#data.raw <- fread(data.file, header=T, sep="\t", stringsAsFactors=F)
data.raw <- fread(data.file, header=T, sep="\t", stringsAsFactors=F)
data.raw %>% dim		# 행(=자료)과 열(=변수)의 수
data.raw %>% head(10)	# 상위 10개 자료 출력
data.raw %>% names		# 변수명 출력

### 대상 연도
table(data.raw$year)
target.year <- 2016
#target.year <- 2000:2009

### 대상 연도 및 가사가 있는 자료만 추출/필터
data.lyric <- 
	data.raw %>% 
	filter(!is.na(lyric)) %>% 
	filter(year %in% target.year) %>% 
	select(lyric)

### 추출된 자료 확인
data.lyric %>% class()
data.lyric %>% dim()
data.lyric %>% head()

### data.lyric 각 자료에 fn.extractNoun 함수를 실행
data.lyric.noun <- apply(data.lyric,1,fn.extractNoun)
data.lyric.noun %>% head()

### list 자료형을 vector로 변환
data.lyric.noun.value <- 
	data.lyric.noun %>% 
	unlist()

### 단어의 개수 %>% 1글자 이상 추출 %>% 상위 100개 선택
data.lyric.noun.df <- 
	data.frame(noun=data.lyric.noun.value,stringsAsFactors=F) %>%
	count(noun, sort=T) %>%
	filter(nchar(noun)>1) %>%
	head(100)

### 막대그래프 (기본)
data.lyric.noun.df.top20 <- data.lyric.noun.df %>% head(20) %>% as.data.frame
barplot(n~noun, data=data.lyric.noun.df.top20, horiz=T)

### 막대그래프
data.lyric.noun.df.top20 %>%
  ggplot(aes(reorder(noun, n), n)) +
  geom_bar(stat = "identity", fill = "#1B687E") +
  labs(title = "Number of Noun", y = NULL, x = NULL) +
  geom_text(aes(label = n) , hjust = -0.25) +
  coord_flip()

### wordcloud
data.lyric.noun.df %>% wordcloud2()


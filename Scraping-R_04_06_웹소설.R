
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

#################
## 제4장 XPath ##
#################

##############
## 4.5 사례 ##
##############

## 웹소설 @네이버

library(tidyverse)
library(xml2)
library(XML)

#장르 URL - 베스트리그 랭킹 로맨스 장르
url.genre <- "https://novel.naver.com/best/ranking?genre=101&periodType=DAILY"
html.genre <- read_html(url.genre)
html.genre.parsed <- htmlParse(html.genre)

#[웹소설] 장르 내 모든 소설 URL - 로맨스 장르 
xpathSApply(html.genre.parsed, "//div[@class='ranking_wrap_left']//a", 
            xmlGetAttr, "href")

url.novels <- xpathSApply(html.genre.parsed, "//div[@class='ranking_wrap_left']//a", 
                          xmlGetAttr, "href") %>% 
  str_c("https://novel.naver.com", .)
url.novels

#[웹소설] 장르 내 첫 번째(1위) 소설 URL - 로맨스 장르 - 1위 소설
url.novel <- url.novels[1]
url.novel
html.novel <- read_html(url.novel)
html.novel.parsed <- htmlParse(html.novel)

#[웹소설] 제목: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
title <- xpathSApply(html.novel.parsed, "//h2[@class='title']", xmlValue)
title

#[웹소설] 작가: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
author <- xpathSApply(html.novel.parsed, 
                      "//span[@class='item'][2]/a", xmlValue) %>% 
  paste(collapse=" ")
author

#[웹소설] 장르: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
genre <- xpathSApply(html.novel.parsed, "//span[@class='item'][1]", xmlValue)
genre

#[웹소설] 평점: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
rating <- xpathSApply(html.novel.parsed, 
                      "//div[@class='info_group']/span[@class='score_area']", 
                      xmlValue, recursive=FALSE) %>%
  as.numeric()
rating

#[에피소드] 마지막 페이지 모든 에피소드 URL: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
xpathSApply(html.novel.parsed, "//li[@class='volumeComment']/a", 
            xmlGetAttr, "href")

url.vols <- xpathSApply(html.novel.parsed, "//li[@class='volumeComment']/a", 
                        xmlGetAttr, "href") %>% 
  str_c("https://novel.naver.com", .)
url.vols

#[에피소드] 마지막 페이지 내 첫 번째 에피소드 URL: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설 
url.vol <- url.vols[1]
url.vol
html.vol <- read_html(url.vol)
html.vol.parsed <- htmlParse(html.vol)

#[에피소드] 마지막 페이지 내 첫 번째 에피소드 제목 및 내용: 로맨스 장르 - 1위 소설 - 1페이지 1번 에피소드
xpathSApply(html.vol.parsed, "//h2[@class='detail_view_header']",
            xmlValue, recursive=TRUE, trim=TRUE)
episode.vol <- xpathSApply(html.vol.parsed, "//h2[@class='detail_view_header']",
                           xmlValue, recursive=FALSE, trim=TRUE)
episode.vol

content.vol <- xpathSApply(html.vol.parsed, 
                           "//div[@class='detail_view_content ft15']/p", xmlValue) %>%
  str_c(collapse=" ")
content.vol

#[에피소드] 에피소드 총페이지 개수: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
xpathSApply(html.novel.parsed, "//span[@class='past_number']", xmlValue)

vol.no <- xpathSApply(html.novel.parsed, "//span[@class='past_number']", xmlValue) %>% 
  parse_number()
vol.no

page <- ceiling(vol.no/10)
page

#[전체 에피소드] 모든 페이지 모든 에피소드 URL: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
url.vols <- vector("list", length(page))
for (i in c(1:page)) {
  url.page <- str_c(url.novel, "&page=", i)
  html.page <- read_html(url.page)
  html.page.parsed <- htmlParse(html.page)
  url.vols[[i]] <- xpathSApply(html.page.parsed, "//li[@class='volumeComment']/a",
                               xmlGetAttr, "href") %>% 
    str_c("https://novel.naver.com", .)
}
url.vols <- unlist(url.vols)
url.vols

url.vols <- str_sort(url.vols, numeric=TRUE)
url.vols

#[전체 에피소드] 모든 페이지 모든 에피소드 데이터 수집: 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
episode.vol <- vector("list", length(url.vols))
content.vol <- vector("list", length(url.vols))
j <- 0
for (url.vol in url.vols) {
  j <- j + 1
  html.vol <- read_html(url.vol)
  html.vol.parsed <- htmlParse(html.vol)
  episode.vol[[j]] <- xpathSApply(html.vol.parsed, 
                                  "//h2[@class='detail_view_header']",
                                  xmlValue, recursive=FALSE, trim=TRUE)
  content.vol[[j]] <- xpathSApply(html.vol.parsed,
                                  "//div[@class='detail_view_content ft15']/p", 
                                  xmlValue) %>% 
    str_c(collapse=" ")
}
str(episode.vol)
str(content.vol)

#[수집 데이터 저장] 장르 내 첫 번째(1위) 소설 - 로맨스 장르 - 1위 소설
navernovel <- tibble(title=title, author=author, genre=genre, rating=rating, 
                     episode=unlist(episode.vol), content=unlist(content.vol))
navernovel

# 카테고리 URL
url <- "https://novel.naver.com/webnovel/ranking" #시리즈에디션 랭킹
url <- "https://novel.naver.com/best/ranking" #베스트리그 랭킹
url <- "https://novel.naver.com/challenge/ranking" #챌린지리그 랭킹
library(tidyverse)
library(xml2)
library(XML)
html <- read_html(url)
html.parsed <- htmlParse(html)

# 장르 코드
url <- "https://novel.naver.com/webnovel/ranking"
html <- read_html(url)
html.parsed <- htmlParse(html)
xpathSApply(html.parsed, "//ul[@class='select_box NE=a:ran']//a", xmlGetAttr, "class")

genre.code <- xpathSApply(html.parsed, "//ul[@class='select_box NE=a:ran']//a", 
                          xmlGetAttr, "class") %>% 
  parse_number()
genre.code

# 장르 이름
genre.name <- xpathSApply(html.parsed, "//ul[@class='select_box NE=a:ran']/li", 
                          xmlValue)
genre.name

# 장르 이름에 코드 부여
names(genre.name) <- genre.code
genre.name

# 네이버 웹소설 데이터 추출 함수 naverNovelRanking()
# ...

# 분석용 데이터 수집
navernovel.ranking <- naverNovelRanking(type="best", genre=c(101, 102, 103, 104), n=10)
navernovel.ranking

save(navernovel.ranking, file="navernovel-ranking.rda")
load("navernovel-ranking.rda")

# 시각화 및 분석
navernovel.ranking %>% 
  count(genre)

summary(navernovel.ranking$rating)
navernovel.ranking %>% 
  group_by(genre) %>% 
  summarise(median=median(rating, na.rm=TRUE))

# [그림 4-44]
windows(width=7.0, height=5.5)
ggplot(navernovel.ranking, aes(x=factor(genre, levels=unique(genre)), y=rating)) + 
  geom_boxplot(fill="plum2", color="cornflowerblue", notch=TRUE) +
  geom_point(position="jitter", pch=21, bg="red", color="dimgray", alpha=0.2) +
  scale_y_continuous(limits=c(9.5, 10)) +
  labs(x="", y="평점", 
       title="웹소설",
       subtitle="평점 분포",
       caption="출처: 네이버") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=15),
        axis.text.x=element_text(face="bold", size=12))

# RmecabKo 한글 형태소 분석기 이용 

# RmecabKo 설치
install.packages("RmecabKo")

install.packages("remotes")
remotes:: install_github("junhewk/RmecabKo")

library(RmecabKo)
install_mecab("C:/mecab")

# 명사 추출: 샘플 데이터셋에 적용 
some <- navernovel.ranking %>% 
  group_by(genre) %>% 
  slice_head(n=2) %>% 
  ungroup()
some

some.clean <- some %>% 
  mutate(content=str_replace_all(content, "[[:punct:]]", " ")) %>%
  mutate(content=str_replace_all(content, "[[:digit:]]", " ")) %>% 
  mutate(content=str_replace_all(content, "[^가-힣 ]", " ")) %>% 
  mutate(content=str_replace_all(content, "[[:space:]]{2,}", " ")) %>% 
  mutate(content=str_trim(content)) %>% 
  mutate(content=iconv(content, "UTF-8")) 
some.clean

library(RmecabKo)
txt <- c("넓은 벌 동쪽 끝으로 옛 이야기 지줄대는 실개천이 휘돌아 나가고", 
         "얼룩배기 황소가 해설피 금빛 게으른 울음을 우는 곳", 
         "그 곳이 차마 꿈엔들 잊힐리야")
token_nouns(txt)

token_nouns(txt) %>% 
  map_chr(paste, collapse=" ")

word <- token_nouns(some.clean$content) %>% 
  map(unlist) %>% 
  map(~.x[str_length(.x) >= 2]) %>% 
  map_chr(paste, collapse=" ")
str(word)
word[1]

some.word <- some %>% 
  mutate(content=str_replace_all(content, "[[:punct:]]", " ")) %>%
  mutate(content=str_replace_all(content, "[[:digit:]]", " ")) %>% 
  mutate(content=str_replace_all(content, "[^가-힣 ]", " ")) %>% 
  mutate(content=str_replace_all(content, "[[:space:]]{2,}", " ")) %>% 
  mutate(content=str_trim(content)) %>% 
  mutate(content=iconv(content, "UTF-8")) %>% 
  mutate(content=map(content, token_nouns)) %>% 
  mutate(content=map(content, unlist)) %>% 
  mutate(content=map(content, ~.x[str_length(.x) >= 2])) %>% 
  mutate(content=map_chr(content, paste, collapse=" "))
some.word
some.word$content[1]

some.doc <- some.word %>% 
  group_by(genre, title) %>% 
  summarise(content=paste(content, collapse=" ")) %>% 
  ungroup()
some.doc
some.doc$content[1]

# 명사 추출: 전체 데이터셋에 적용
words <- navernovel.ranking %>% 
  mutate(content=str_replace_all(content, "[[:punct:]]", " ")) %>%
  mutate(content=str_replace_all(content, "[[:digit:]]", " ")) %>% 
  mutate(content=str_replace_all(content, "[^가-힣 ]", " ")) %>% 
  mutate(content=str_replace_all(content, "[[:space:]]{2,}", " ")) %>% 
  mutate(content=str_trim(content)) %>% 
  mutate(content=iconv(content, "UTF-8")) %>% 
  mutate(content=map(content, token_nouns)) %>% 
  mutate(content=map(content, unlist)) %>% 
  mutate(content=map(content, ~.x[str_length(.x) >= 2])) %>% 
  mutate(content=map_chr(content, paste, collapse=" "))
words

docs <- words %>% 
  group_by(genre, rank, title, rating) %>% 
  summarise(content=paste(content, collapse=" ")) %>% 
  ungroup()
docs

docs %>% 
  mutate(word.count=str_count(.$content, "\\w+"))

library(stringi)
docs %>% 
  mutate(word.count=stri_count_words(.$content))

# 문서-용어행렬
library(tm)
getSources()

docs <- docs %>% 
  mutate(doc_id=1:n()) %>% 
  select(doc_id, text=content, everything())
docs 

corp <- VCorpus(DataframeSource(docs))
class(corp)
corp

dtm <- DocumentTermMatrix(corp)
rownames(dtm) <- paste0(docs$doc_id, "-", docs$genre)
dtm
inspect(dtm[1:6, 1:6])

library(slam)
summary(col_sums(dtm))

col_sums(dtm)[order(col_sums(dtm), decreasing=TRUE)][1:10]
findFreqTerms(dtm, lowfreq=500)

term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean)*
  log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term.tfidf)

dtm <- dtm[, term.tfidf >= median(term.tfidf)]

dtm <- dtm[row_sums(dtm) > 0,]
dtm

col_sums(dtm)[order(col_sums(dtm), decreasing=TRUE)][1:10]
findFreqTerms(dtm, lowfreq=500)

# 동시 출현 단어 네트워크
library(tidytext)
dtm.long <- tidy(dtm)
dtm.long

?tidy
methods(tidy)

library(widyr)
word.pair <- dtm.long %>%
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE)
word.pair

library(igraph)
word.pair <- filter(word.pair, n >= 20)
word.pair.g <- graph_from_data_frame(word.pair)
word.pair.g

# [그림 4-45]
library(ggraph)
set.seed(123)
windows(width=7.0, height=5.5)
ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="tomato", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="네이버 웹소설",
       subtitle="동시 출현 단어 네트워크",
       caption="출처: 네이버") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())

word.pair.g <- dtm.long %>%
  filter(str_detect(document, "판타지")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 9) %>% 
  graph_from_data_frame()
word.pair.g

# 로맨스 동시 출현 단어 네트워크 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "로맨스")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 15) %>% 
  graph_from_data_frame()
word.pair.g

library(ggraph) 
set.seed(123)
windows(width=7.0, height=5.5)
p1 <- ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="firebrick", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="로맨스") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p1

# 판타지 동시 출현 단어 네트워크 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "판타지")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 9) %>% 
  graph_from_data_frame()
word.pair.g

library(ggraph) 
set.seed(123)
windows(width=7.0, height=5.5)
p2 <- ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="darkolivegreen", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="판타지") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p2

# 무협 동시 출현 단어 네트워크 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "무협")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 9) %>% 
  graph_from_data_frame()
word.pair.g

library(ggraph) 
set.seed(123)
windows(width=7.0, height=5.5)
p3 <- ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="goldenrod", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="무협") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p3

# 미스터리 동시 출현 단어 네트워크 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "미스터리")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 15) %>% 
  graph_from_data_frame()
word.pair.g

library(ggraph) 
set.seed(123)
windows(width=7.0, height=5.5)
p4 <- ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="cornflowerblue", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="미스터리") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p4

# 통합 [그림 4-46]
windows(width=9.0, height=9.0)
library(patchwork)
p1 + p2 + p3 + p4 + 
  plot_annotation(title="네이버 웹소설",
                  subtitle="동시 출현 단어 네트워크",
                  caption="출처: 네이버",
                  theme=theme(plot.title=element_text(face="bold", size=18),
                              plot.subtitle=element_text(size=15),
                              plot.caption=element_text(size=12)))

# 토픽모델링
library(topicmodels)
webnovel.lda <- LDA(dtm, method="Gibbs", k=4, control=list(seed=123))
webnovel.lda

topics(webnovel.lda)[1:8]

table(topics(webnovel.lda))

terms(webnovel.lda, 10)

str(webnovel.lda, max.level=2, nchar.max=50)

webnovel.lda@beta[1:4, 1:5]
exp(webnovel.lda@beta[1:4, 1:5])

library(tidytext)
webnovel.term <- tidy(webnovel.lda, matrix="beta")
webnovel.term

webnovel.term.top <- webnovel.term %>%
  group_by(topic) %>%
  slice_max(order_by=beta, n=10) %>%
  ungroup() %>%
  arrange(topic, desc(beta))
webnovel.term.top

# [그림 4-47]
windows(width=7.0, height=7.0)
ggplot(webnovel.term.top, 
       aes(reorder_within(x=term, by=beta, within=topic), 
           beta, fill=factor(topic))) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~paste0("토픽", topic), scales="free") +
  scale_x_reordered() +
  scale_fill_viridis_d(end=0.75, direction=-1, option="plasma") +
  coord_flip() +
  theme_minimal() +
  labs(x="단어", y="베타(단어-토픽 확률)", title="네이버 웹소설",
       subtitle="토픽별 단어의 발생 확률 분포",
       caption="출처: 네이버") +
  theme(strip.text.x=element_text(face="bold", size=12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=15))

webnovel.lda@gamma[1:5, 1:4]

webnovel.doc <- tidy(webnovel.lda, matrix="gamma")
webnovel.doc

webnovel.doc <- webnovel.doc %>%
  separate(col=document, into=c("id", "genre"), sep="-", convert=TRUE)
webnovel.doc

# [그림 4-48]
windows(width=7.0, height=7.0)
ggplot(webnovel.doc, aes(factor(topic), gamma, fill=genre)) +
  geom_boxplot(color="gray50", show.legend=FALSE) +
  facet_wrap(~factor(genre, 
                     levels=c("로맨스", "판타지", "무협", "미스터리")), nrow=2) +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal() +
  labs(x="토픽", y="감마(문서-토픽 확률)", 
       title="네이버 웹소설",
       subtitle="토픽별 웹소설의 배정 확률 분포",
       caption="출처: 네이버") +
  theme(strip.text.x=element_text(face="bold", size=12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=15))

# 머신러닝: Naive Bayes
webnovel <- as_tibble(as.matrix(dtm)) %>% 
  mutate(id_genre=rownames(as.matrix(dtm))) %>% 
  separate(col=id_genre, into=c("id", "genre"), sep="-") %>% 
  select(-id) %>% 
  mutate(genre=factor(genre,
                      levels=c("로맨스", "판타지", "무협", "미스터리"))) %>% 
  select(genre, everything())
webnovel

toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("n", "y"))
  return(x)
}

webnovel <- webnovel %>% 
  mutate(across(.cols=where(is.numeric), .fns=toFactor))
webnovel

set.seed(123)
train <- sample(nrow(webnovel), 0.7*nrow(webnovel))
webnovel.train <- webnovel[train,]
webnovel.test <- webnovel[-train,]
table(webnovel.train$genre); sum(table(webnovel.train$genre))
table(webnovel.test$genre); sum(table(webnovel.test$genre))

library(e1071)
webnovel.nb <- naiveBayes(x=webnovel.train[-1], y=webnovel.train$genre, type="class")

webnovel.nb.pred <- predict(webnovel.nb, newdata=webnovel.test[-1])
head(webnovel.nb.pred)

table(webnovel.test$genre, webnovel.nb.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(webnovel.test$genre, webnovel.nb.pred)))
mean(webnovel.nb.pred==webnovel.test$genre)


##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

####################
## 제6장 셀레니움 ##
####################

##############
## 6.4 사례 ##
##############

## 영화 리뷰 @IMDb

library(tidyverse)
library(RSelenium)
library(rvest)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.imdb.com/title/tt1718924/reviews?ref_=tt_urv")

body <- remDr$findElement(using="css selector", value="body")
body$highlightElement()

body$sendKeysToElement(list(key="end"))

btn.loadmore <- remDr$findElement(using="css selector", value="#load-more-trigger")
btn.loadmore$highlightElement()
btn.loadmore$getElementText()

btn.loadmore$clickElement()

btn.loadmore$isElementDisplayed()[[1]]

btn.loadmore$clickElement()
body$sendKeysToElement(list(key="end"))

btn.loadmore$isElementDisplayed()[[1]]

remDr$navigate("https://www.imdb.com/title/tt1718924/reviews?ref_=tt_urv")
btn.loadmore <- remDr$findElement(using="css selector", value="#load-more-trigger")
body <- remDr$findElement(using="css selector", value="body")
body$sendKeysToElement(list(key="end"))
while (btn.loadmore$isElementDisplayed()[[1]]) {
  btn.loadmore$clickElement()
  Sys.sleep(3)
  body$sendKeysToElement(list(key="end"))
  btn.loadmore <- remDr$findElement(using="css selector", 
                                    value="#load-more-trigger")
}

html <- remDr$getPageSource()[[1]]

title <- read_html(html) %>% 
  html_elements(".lister-item-content > .title") %>% 
  html_text() %>% 
  str_trim() 
title

name <- read_html(html) %>% 
  html_elements(".display-name-link") %>% 
  html_text() 
name

date <- read_html(html) %>% 
  html_elements(".review-date") %>% 
  html_text() %>% 
  parse_date("%d %B %Y")
date

library(lubridate)
date <- read_html(html) %>% 
  html_elements(".review-date") %>% 
  html_text() %>% 
  dmy()
date

read_html(html) %>% 
  html_elements(".rating-other-user-rating") %>% 
  html_text() %>% 
  str_trim() %>% 
  str_extract("\\d+") %>% 
  as.numeric()

rating <- read_html(html) %>% 
  html_elements(".lister-item-content") %>% 
  html_element(".rating-other-user-rating") %>% 
  html_text() %>% 
  str_trim() %>% 
  str_extract("\\d+") %>% 
  as.numeric()
rating

content <- read_html(html) %>% 
  html_elements(".text.show-more__control") %>% 
  html_text() 
content

imdb.familyman <- tibble(title=title, name=name, 
                         date=date, rating=rating, content=content)
imdb.familyman

save(imdb.familyman, file="imdb-familyman.rda")
load("imdb-familyman.rda")

summary(imdb.familyman$rating)
review <- imdb.familyman %>% 
  drop_na(rating) %>% 
  mutate(class=cut(rating, breaks=c(0, 7, 10), labels=c("low", "high")))
table(review$class)

review <- review %>% 
  add_column(doc_id=1:nrow(.)) %>% 
  rename(text=content) %>% 
  select(doc_id, text, class) %>% 
  mutate(text=iconv(text, to="ascii", sub=""))
review
library(tm)
docs <- VCorpus(DataframeSource(review))
docs

lapply(docs, content)[c(25, 50)]
meta(docs)$class[c(25, 50)]

docs <- tm_map(docs, content_transformer(tolower))
mystopwords <- c(stopwords("english"), 
                 c("also", "among", "around", "but", "can", "done", "else", "even", 
                   "ever", "either", "one", "may", "might", "must", "said","movie", 
                   "movies", "film", "films", "gerard", "butler", "gretchen", "mol",
                   "elise", "allison", "alison", "brie", "willem", "dafoe", "jenkins",
                   "alfred", "kher", "dane", "dan", "jensen", "ryan", "lou" ))
docs <- tm_map(docs, removeWords, mystopwords)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
lapply(docs, content)[c(25, 50)]

dtm <- DocumentTermMatrix(docs)
dtm
inspect(dtm)

term.freq <- colSums(as.matrix(dtm))
term.freq[order(term.freq, decreasing=TRUE)][1:7]

comparison <- as.matrix(dtm)
rownames(comparison) <- review$class
comparison[1:5, 1:7]
comparison <- rowsum(comparison, group=rownames(comparison))
comparison[, 1:7]

# [그림 6-21]
library(wordcloud)
windows(width=6.5, height=6.5)
set.seed(123)
comparison.cloud(t(comparison), colors=c("cornflowerblue", "tomato"), title.size=2, 
                 title.colors=c("blue", "red"), title.bg.colors=c("wheat"), 
                 rot.per=0, scale=c(5, 0.4), max.words=75)

# [그림 6-22]
windows(width=5.0, height=5.0)
set.seed(123)
commonality.cloud(t(comparison), scale=c(5, 0.4), max.words=100,
                  rot.per=0, random.order=FALSE, random.color=FALSE, 
                  colors=brewer.pal(6, "Dark2"))

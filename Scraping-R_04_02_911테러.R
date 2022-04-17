
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

## 911 테러 @뉴욕타임즈

library(httr)
library(XML)
url <- "https://archive.nytimes.com/www.nytimes.com/learning/general/onthisday/big/0911.html"
html <- GET(url)
html.parsed <- htmlParse(html)

save(html, file="nytimes-911.rda")
load("nytimes-911.rda")

text <- xpathSApply(html.parsed, path="//p", fun=xmlValue)
str(text)

text <- text[text != ""]
text <- paste(text, collapse=" ")
text

library(tm)
getSources()

doc <- VCorpus(VectorSource(text))
class(doc)
inspect(doc)

doc <- tm_map(doc, content_transformer(tolower))
mystopwords <- c(stopwords("english"), 
                 c("also", "among", "but", "even", "four", "get", "one", "said",
                   "the", "there", "two", "three"))
doc <- tm_map(doc, removeWords, mystopwords)
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, stemDocument)

dtm <- DocumentTermMatrix(doc)
dtm
inspect(dtm[, 1:10])

term.freq <- colSums(as.matrix(dtm))
term.freq[order(term.freq, decreasing=TRUE)][1:10]

# [그림 4-23] 
library(wordcloud)
set.seed(123)
term.freq <- colSums(as.matrix(dtm))
windows(width=6.5, height=6.5)
wordcloud(words=names(term.freq), freq=term.freq, scale=c(4, 0.2), min.freq=3, 
          rot.per=0, random.order=FALSE, random.color=FALSE, 
          colors=brewer.pal(5, "Set1"))

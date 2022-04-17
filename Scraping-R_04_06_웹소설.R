
##########################################
## RÀ» ÀÌ¿ëÇÑ À¥½ºÅ©·¹ÀÌÇÎ°ú µ¥ÀÌÅÍºÐ¼® ##
## (°û±â¿µ, µµ¼­ÃâÆÇ Ã»¶÷)              ## 
##########################################

#################
## Á¦4Àå XPath ##
#################

##############
## 4.5 »ç·Ê ##
##############

## À¥¼Ò¼³ @³×ÀÌ¹ö

library(tidyverse)
library(xml2)
library(XML)

#Àå¸£ URL - º£½ºÆ®¸®±× ·©Å· ·Î¸Ç½º Àå¸£
url.genre <- "https://novel.naver.com/best/ranking?genre=101&periodType=DAILY"
html.genre <- read_html(url.genre)
html.genre.parsed <- htmlParse(html.genre)

#[À¥¼Ò¼³] Àå¸£ ³» ¸ðµç ¼Ò¼³ URL - ·Î¸Ç½º Àå¸£ 
xpathSApply(html.genre.parsed, "//div[@class='ranking_wrap_left']//a", 
            xmlGetAttr, "href")

url.novels <- xpathSApply(html.genre.parsed, "//div[@class='ranking_wrap_left']//a", 
                          xmlGetAttr, "href") %>% 
  str_c("https://novel.naver.com", .)
url.novels

#[À¥¼Ò¼³] Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ URL - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
url.novel <- url.novels[1]
url.novel
html.novel <- read_html(url.novel)
html.novel.parsed <- htmlParse(html.novel)

#[À¥¼Ò¼³] Á¦¸ñ: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
title <- xpathSApply(html.novel.parsed, "//h2[@class='book_title']", xmlValue)
title

#[À¥¼Ò¼³] ÀÛ°¡: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
author <- xpathSApply(html.novel.parsed, 
                      "//p[@class='writer']/a[@class='NPI=a:writer']", xmlValue)
author

#[À¥¼Ò¼³] Àå¸£: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
genre <- xpathSApply(html.novel.parsed, "//span[@class='genre']", xmlValue)
genre

#[À¥¼Ò¼³] ÆòÁ¡: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
rating <- xpathSApply(html.novel.parsed, "//p[@class='grade_area']/em", xmlValue) %>%
  as.numeric()
rating

#[¿¡ÇÇ¼Òµå] ¸¶Áö¸· ÆäÀÌÁö ¸ðµç ¿¡ÇÇ¼Òµå URL: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
xpathSApply(html.novel.parsed, "//li[@class='volumeComment']/a", 
            xmlGetAttr, "href")

url.vols <- xpathSApply(html.novel.parsed, "//li[@class='volumeComment']/a", 
                        xmlGetAttr, "href") %>% 
  str_c("https://novel.naver.com", .)
url.vols

#[¿¡ÇÇ¼Òµå] ¸¶Áö¸· ÆäÀÌÁö ³» Ã¹ ¹øÂ° ¿¡ÇÇ¼Òµå URL: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³ 
url.vol <- url.vols[1]
url.vol
html.vol <- read_html(url.vol)
html.vol.parsed <- htmlParse(html.vol)

#[¿¡ÇÇ¼Òµå] ¸¶Áö¸· ÆäÀÌÁö ³» Ã¹ ¹øÂ° ¿¡ÇÇ¼Òµå Á¦¸ñ ¹× ³»¿ë: ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³ - 1ÆäÀÌÁö 1¹ø ¿¡ÇÇ¼Òµå
xpathSApply(html.vol.parsed, "//h2[@class='detail_view_header']",
            xmlValue, recursive=TRUE, trim=TRUE)
episode.vol <- xpathSApply(html.vol.parsed, "//h2[@class='detail_view_header']",
                           xmlValue, recursive=FALSE, trim=TRUE)
episode.vol

content.vol <- xpathSApply(html.vol.parsed, 
                           "//div[@class='detail_view_content ft15']/p", xmlValue) %>%
  str_c(collapse=" ")
content.vol

#[¿¡ÇÇ¼Òµå] ¿¡ÇÇ¼Òµå ÃÑÆäÀÌÁö °³¼ö: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
xpathSApply(html.novel.parsed, "//span[@class='total']", xmlValue)

vol.no <- xpathSApply(html.novel.parsed, "//span[@class='total']", xmlValue) %>% 
  parse_number()
vol.no

page <- ceiling(vol.no/10)
page

#[ÀüÃ¼ ¿¡ÇÇ¼Òµå] ¸ðµç ÆäÀÌÁö ¸ðµç ¿¡ÇÇ¼Òµå URL: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
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

#[ÀüÃ¼ ¿¡ÇÇ¼Òµå] ¸ðµç ÆäÀÌÁö ¸ðµç ¿¡ÇÇ¼Òµå µ¥ÀÌÅÍ ¼öÁý: Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
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

#[¼öÁý µ¥ÀÌÅÍ ÀúÀå] Àå¸£ ³» Ã¹ ¹øÂ°(1À§) ¼Ò¼³ - ·Î¸Ç½º Àå¸£ - 1À§ ¼Ò¼³
navernovel <- tibble(title=title, author=author, genre=genre, rating=rating, 
                     episode=unlist(episode.vol), content=unlist(content.vol))
navernovel

# Ä«Å×°í¸® URL
url <- "https://novel.naver.com/webnovel/ranking" #¿À´ÃÀÇ À¥¼Ò¼³ ·©Å·
url <- "https://novel.naver.com/best/ranking" #º£½ºÆ®¸®±× ·©Å·
url <- "https://novel.naver.com/challenge/ranking" #Ã§¸°Áö¸®±× ·©Å·
library(tidyverse)
library(xml2)
library(XML)
html <- read_html(url)
html.parsed <- htmlParse(html)

# Àå¸£ ÄÚµå
url <- "https://novel.naver.com/webnovel/ranking"
html <- read_html(url)
html.parsed <- htmlParse(html)
xpathSApply(html.parsed, "//ul[@id='rankingSelectedItem']//a", xmlGetAttr, "class")

genre.code <- xpathSApply(html.parsed, "//ul[@id='rankingSelectedItem']//a", 
                          xmlGetAttr, "class") %>% 
  parse_number()
genre.code

# Àå¸£ ÀÌ¸§
genre.name <- xpathSApply(html.parsed, "//ul[@id='rankingSelectedItem']/li", 
                          xmlValue)
genre.name

# Àå¸£ ÀÌ¸§¿¡ ÄÚµå ºÎ¿©
names(genre.name) <- genre.code
genre.name

# ³×ÀÌ¹ö À¥¼Ò¼³ µ¥ÀÌÅÍ ÃßÃâ ÇÔ¼ö naverNovelRanking()
# ...

# ºÐ¼®¿ë µ¥ÀÌÅÍ ¼öÁý
navernovel.ranking <- naverNovelRanking(type="best", genre=c(101, 102, 103, 104), n=10)
navernovel.ranking

save(navernovel.ranking, file="navernovel-ranking.rda")
load("navernovel-ranking.rda")

# ½Ã°¢È­ ¹× ºÐ¼®
navernovel.ranking %>% 
  count(genre)

summary(navernovel.ranking$rating)
navernovel.ranking %>% 
  group_by(genre) %>% 
  summarise(median=median(rating, na.rm=TRUE))

# [±×¸² 4-44]
windows(width=7.0, height=5.5)
ggplot(navernovel.ranking, aes(x=factor(genre, levels=unique(genre)), y=rating)) + 
  geom_boxplot(fill="plum2", color="cornflowerblue", notch=TRUE) +
  geom_point(position="jitter", pch=21, bg="red", color="dimgray", alpha=0.2) +
  scale_y_continuous(limits=c(9.5, 10)) +
  labs(x="", y="ÆòÁ¡", 
       title="À¥¼Ò¼³",
       subtitle="ÆòÁ¡ ºÐÆ÷",
       caption="ÃâÃ³: ³×ÀÌ¹ö") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=15),
        axis.text.x=element_text(face="bold", size=12))

# RmecabKo ÇÑ±Û ÇüÅÂ¼Ò ºÐ¼®±â ÀÌ¿ë 

# RmecabKo ¼³Ä¡
install.packages("RmecabKo")

install.packages("remotes")
remotes:: install_github("junhewk/RmecabKo")

library(RmecabKo)
install_mecab("C:/mecab")

# ¸í»ç ÃßÃâ: »ùÇÃ µ¥ÀÌÅÍ¼Â¿¡ Àû¿ë 
some <- navernovel.ranking %>% 
  group_by(genre) %>% 
  slice_head(n=2) %>% 
  ungroup()
some

some.clean <- some %>% 
  mutate(content=str_replace_all(content, "[[:punct:]]", " ")) %>%
  mutate(content=str_replace_all(content, "[[:digit:]]", " ")) %>% 
  mutate(content=str_replace_all(content, "[^°¡-ÆR ]", " ")) %>% 
  mutate(content=str_replace_all(content, "[[:space:]]{2,}", " ")) %>% 
  mutate(content=str_trim(content)) %>% 
  mutate(content=iconv(content, "UTF-8")) 
some.clean

library(RmecabKo)
txt <- c("³ÐÀº ¹ú µ¿ÂÊ ³¡À¸·Î ¿¾ ÀÌ¾ß±â ÁöÁÙ´ë´Â ½Ç°³ÃµÀÌ ÈÖµ¹¾Æ ³ª°¡°í", 
         "¾ó·è¹è±â È²¼Ò°¡ ÇØ¼³ÇÇ ±Ýºû °ÔÀ¸¸¥ ¿ïÀ½À» ¿ì´Â °÷", 
         "±× °÷ÀÌ Â÷¸¶ ²Þ¿£µé ÀØÈú¸®¾ß")
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
  mutate(content=str_replace_all(content, "[^°¡-ÆR ]", " ")) %>% 
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

# ¸í»ç ÃßÃâ: ÀüÃ¼ µ¥ÀÌÅÍ¼Â¿¡ Àû¿ë
words <- navernovel.ranking %>% 
  mutate(content=str_replace_all(content, "[[:punct:]]", " ")) %>%
  mutate(content=str_replace_all(content, "[[:digit:]]", " ")) %>% 
  mutate(content=str_replace_all(content, "[^°¡-ÆR ]", " ")) %>% 
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

# ¹®¼­-¿ë¾îÇà·Ä
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

# µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ©
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

# [±×¸² 4-45]
library(ggraph)
set.seed(123)
windows(width=7.0, height=5.5)
ggraph(word.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="cyan4") +
  geom_node_point(shape=21, color="gray", fill="tomato", size=5, stroke=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(0.2, "lines")) +
  labs(title="³×ÀÌ¹ö À¥¼Ò¼³",
       subtitle="µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ©",
       caption="ÃâÃ³: ³×ÀÌ¹ö") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())

word.pair.g <- dtm.long %>%
  filter(str_detect(document, "ÆÇÅ¸Áö")) %>% 
  pairwise_count(item=term, feature=document, sort=TRUE, upper=FALSE) %>% 
  filter(n >= 9) %>% 
  graph_from_data_frame()
word.pair.g

# ·Î¸Ç½º µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ© 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "·Î¸Ç½º")) %>% 
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
  labs(title="·Î¸Ç½º") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p1

# ÆÇÅ¸Áö µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ© 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "ÆÇÅ¸Áö")) %>% 
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
  labs(title="ÆÇÅ¸Áö") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p2

# ¹«Çù µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ© 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "¹«Çù")) %>% 
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
  labs(title="¹«Çù") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p3

# ¹Ì½ºÅÍ¸® µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ© 
word.pair.g <- dtm.long %>%
  filter(str_detect(document, "¹Ì½ºÅÍ¸®")) %>% 
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
  labs(title="¹Ì½ºÅÍ¸®") +
  theme_void() +
  theme(plot.title=element_text(face="bold", size=15),
        plot.caption=element_text(size=12),
        legend.title=element_blank())
p4

# ÅëÇÕ [±×¸² 4-46]
windows(width=9.0, height=9.0)
library(patchwork)
p1 + p2 + p3 + p4 + 
  plot_annotation(title="³×ÀÌ¹ö À¥¼Ò¼³",
                  subtitle="µ¿½Ã ÃâÇö ´Ü¾î ³×Æ®¿öÅ©",
                  caption="ÃâÃ³: ³×ÀÌ¹ö",
                  theme=theme(plot.title=element_text(face="bold", size=18),
                              plot.subtitle=element_text(size=15),
                              plot.caption=element_text(size=12)))

# ÅäÇÈ¸ðµ¨¸µ
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

# [±×¸² 4-47]
windows(width=7.0, height=7.0)
ggplot(webnovel.term.top, 
       aes(reorder_within(x=term, by=beta, within=topic), 
           beta, fill=factor(topic))) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~paste0("ÅäÇÈ", topic), scales="free") +
  scale_x_reordered() +
  scale_fill_viridis_d(end=0.75, direction=-1, option="plasma") +
  coord_flip() +
  theme_minimal() +
  labs(x="´Ü¾î", y="º£Å¸(´Ü¾î-ÅäÇÈ È®·ü)", title="³×ÀÌ¹ö À¥¼Ò¼³",
       subtitle="ÅäÇÈº° ´Ü¾îÀÇ ¹ß»ý È®·ü ºÐÆ÷",
       caption="ÃâÃ³: ³×ÀÌ¹ö") +
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

# [±×¸² 4-48]
windows(width=7.0, height=7.0)
ggplot(webnovel.doc, aes(factor(topic), gamma, fill=genre)) +
  geom_boxplot(color="gray50", show.legend=FALSE) +
  facet_wrap(~factor(genre, 
                     levels=c("·Î¸Ç½º", "ÆÇÅ¸Áö", "¹«Çù", "¹Ì½ºÅÍ¸®")), nrow=2) +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal() +
  labs(x="ÅäÇÈ", y="°¨¸¶(¹®¼­-ÅäÇÈ È®·ü)", 
       title="³×ÀÌ¹ö À¥¼Ò¼³",
       subtitle="ÅäÇÈº° À¥¼Ò¼³ÀÇ ¹èÁ¤ È®·ü ºÐÆ÷",
       caption="ÃâÃ³: ³×ÀÌ¹ö") +
  theme(strip.text.x=element_text(face="bold", size=12),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=15))

# ¸Ó½Å·¯´×: Naive Bayes
webnovel <- as_tibble(as.matrix(dtm)) %>% 
  mutate(id_genre=rownames(as.matrix(dtm))) %>% 
  separate(col=id_genre, into=c("id", "genre"), sep="-") %>% 
  select(-id) %>% 
  mutate(genre=factor(genre,
                      levels=c("·Î¸Ç½º", "ÆÇÅ¸Áö", "¹«Çù", "¹Ì½ºÅÍ¸®"))) %>% 
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

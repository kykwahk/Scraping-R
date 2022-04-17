
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

######################
## 제5장 CSS 선택자 ##
######################

##############
## 5.3 사례 ##
##############

## 노벨상 수상 @노벨재단

library(tidyverse)
library(rvest)
url <- "https://www.nobelprize.org"
html <- read_html(url) 
html

# 분야 URL
html_elements(html, "#menu-item-28259 > ul a") %>% 
  html_attr(name="href")

url.areas <- html_elements(html, "#menu-item-28259 > ul a") %>% 
  html_attr(name="href") %>% 
  .[2:7] %>% 
  str_c(url, .)
url.areas

url.areas[1]

# 분야명
area <- str_sub(url.areas[1], start=str_locate(url.areas[1], "prizes/")[,2]+1, end=-2)
area

# 분야 수상 URL
html <- read_html(url.areas[1])
url.winner <- html_elements(html, "div.text p:last-child a:first-child") %>% 
  html_attr(name="href")
url.winner

html <- read_html(url.winner)

# 수상명
html_elements(html, "h3 a") %>% 
  html_text() 

title <- html_elements(html, "h3 a") %>% 
  html_text() %>% 
  word(end=-2)
title

# 수상 연도
year <- html_elements(html, "h3 a") %>% 
  html_text() %>% 
  word(start=-1) %>% 
  as.numeric()
year

html_elements(html, "h3 a") %>% 
  html_text() %>% 
  parse_number()

# 수상자 및 공적
html_elements(html, "p a") %>% 
  html_text()

year[1]
str_c("p a[href*='", year[1], "']")

year[1]
winner.year <- html_elements(html, str_c("p a[href*='", year[1], "']")) %>% 
  html_text()
winner.year

html_elements(html, "p") %>% 
  html_text()

achievement.node <- html_elements(html, "p") 
achievement.node

year[1]
str_detect(as.character(achievement.node), str_c("/", year[1], "/"))
achievement.node[str_detect(as.character(achievement.node), str_c("/", year[1], "/"))]

achievement.year <- achievement.node[str_detect(as.character(achievement.node), 
                                                str_c("/", year[1], "/"))] %>% 
  html_text()
achievement.year

start <- achievement.year %>% 
  str_locate("“|”") %>% 
  .[,1]
start
str_sub(achievement.year, start=start)

year[1]
achievement.node <- html_elements(html, "p") 
achievement.year <- achievement.node[str_detect(as.character(achievement.node), 
                                                str_c("/", year[1], "/"))] %>% 
  html_text()%>% 
  str_sub(str_locate(., "“|”")[,1]) %>% 
  str_replace_all("“|”", "")
achievement.year

achievement.node <- html_elements(html, "p") 
winner <- vector("list", length(year))
achievement <- vector("list", length(year))
i <- 0
for (y in year) {
  i <- i + 1
  winner[[i]] <- html_elements(html, str_c("p a[href*='", y, "']")) %>% 
    html_text()
  achievement[[i]] <- achievement.node[str_detect(as.character(achievement.node), 
                                                  str_c("/", year[i], "/"))] %>% 
    html_text()%>% 
    str_sub(str_locate(., "“|”")[,1]) %>% 
    str_replace_all("“|”", "")
}

winner
achievement

winner %>% 
  map_chr(~str_c(., collapse="; "))
achievement %>% 
  map_chr(~str_c(., collapse="; "))

# 노벨상 데이터 추출 함수
getNobelPrize <- function() {
  library(tidyverse)
  library(rvest)
  url <- "https://www.nobelprize.org"
  html <- read_html(url) 
  url.areas <- html_elements(html, "#menu-item-28259 > ul a") %>% 
    html_attr(name="href") %>% 
    .[2:7] %>% 
    str_c(url, .)
  nobel.prize <- tibble()
  for (url.area in url.areas) {
    area <- str_sub(url.area, start=str_locate(url.area, "prizes/")[,2]+1, end=-2)
    html <- read_html(url.area)
    url.winner <- html_elements(html, "div.text p:last-child a:first-child") %>% 
      html_attr(name="href")
    html <- read_html(url.winner)
    title <- html_elements(html, "h3 a") %>% 
      html_text() %>% 
      word(end=-2)
    year <- html_elements(html, "h3 a") %>% 
      html_text() %>% 
      word(start=-1) %>% 
      as.numeric()
    achievement.node <- html_elements(html, "p") 
    winner <- vector("list", length(year))
    achievement <- vector("list", length(year))
    i <- 0
    for (y in year) {
      i <- i + 1
      winner[[i]] <- html_elements(html, str_c("p a[href*='", y, "']")) %>% 
        html_text()
      achievement[[i]] <- achievement.node[str_detect(as.character(achievement.node), 
                                                      str_c("/", year[i], "/"))] %>% 
        html_text()%>% 
        str_sub(str_locate(., "“|”")[,1]) %>% 
        str_replace_all("“|”", "")
    }
    prize <- tibble(area=area, title=title, year=year, 
                    winner=map_chr(winner, ~str_c(., collapse="; ")), 
                    achievement=map_chr(achievement, ~str_c(., collapse="; ")))
    nobel.prize <- bind_rows(nobel.prize, prize)
    Sys.sleep(sample(10,1)*0.1)
  }
  nobel.prize <- bind_cols(id=1:nrow(nobel.prize), nobel.prize) %>% 
    mutate(area=factor(area, 
                       levels=c("physics", "chemistry", "medicine", 
                                "literature", "peace", "economics")))
  return(nobel.prize)
}
nobel.prize <- getNobelPrize()
nobel.prize

save(nobel.prize, file="nobel-prize.rda")
load("nobel-prize.rda")

# 탐색적 분석
num.area <- nobel.prize %>% 
  group_by(area) %>% 
  filter(winner != "") %>% 
  summarise(num_prizes=n())
num.area

num.laureates <- nobel.prize %>% 
  group_by(area) %>% 
  filter(winner != "") %>% 
  mutate(n=map_int(winner, ~length(unlist(str_split(., ";"))))) %>% 
  summarise(num_laureates=sum(n))
num.laureates

# [그림 5-14]
num.laureates$fraction <- with(num.laureates, num_laureates/sum(num_laureates))
num.laureates$ymax <- cumsum(num.laureates$fraction)
num.laureates$ymin <- c(0, head(num.laureates$ymax, n=-1))
num.laureates$label_position <- (num.laureates$ymax+num.laureates$ymin)/2
num.laureates$label <- str_c(num.laureates$area, "\n", num.laureates$num_laureates)
num.laureates
windows(width=5.5, height=5.5)
ggplot(num.laureates, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=area)) +
  geom_rect() +
  geom_text(x=2, aes(y=label_position, label=label, color=area), size=5) + 
  scale_fill_viridis_d(option="viridis") +
  scale_color_viridis_d(option="viridis") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  labs(x=NULL, y=NULL,
       title="The Nobel Prize",
       subtitle="Number of the Nobel Prize laureates by area",
       caption="Source: The Nobel Foundation") +
  theme_void() +
  theme(plot.title=element_text(face="bold"),
        legend.position="none",
        legend.title=element_blank())   

# 머신러닝: 의사결정나무, 나이브베이즈
library(tidyverse)
library(tm)
nobel.prize
docs <- nobel.prize %>% 
  select(doc_id=id, text=achievement, everything())
docs 
corp <- VCorpus(DataframeSource(docs))
corp

lapply(corp, as.character)[c(1:3)]

corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, stemDocument)

dtm <- DocumentTermMatrix(corp)
dtm
inspect(dtm[1:10, 1:10])

nobel <- as_tibble(as.matrix(dtm)) %>% 
  mutate(nobel_area=nobel.prize$area) %>% 
  select(nobel_area, everything())
nobel

toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("n", "y"))
  return(x)
}

nobel <- nobel %>% 
  mutate(across(where(is.numeric), toFactor))
nobel

set.seed(123)
train <- sample(nrow(nobel), 0.7*nrow(nobel))
nobel.train <- nobel[train,]
nobel.test <- nobel[-train,]
table(nobel.train$nobel_area); sum(table(nobel.train$nobel_area))
table(nobel.test$nobel_area); sum(table(nobel.test$nobel_area))

library(rpart)
nobel.dtree <- rpart(nobel_area ~ ., data=nobel.train, method="class",
                     parms=list(split="gini"), control=list(cp=0))
nobel.dtree

nobel.dtree.pred <- predict(nobel.dtree, newdata=nobel.test, type="class")
head(nobel.dtree.pred)

table(nobel.test$nobel_area, nobel.dtree.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(nobel.test$nobel_area, nobel.dtree.pred)))
mean(nobel.test$nobel_area==nobel.dtree.pred)

library(e1071)
nobel.nb <- naiveBayes(x=nobel.train[-1], y=nobel.train$nobel_area, type="class")

nobel.nb.pred <- predict(nobel.nb, newdata=nobel.test[-1])
head(nobel.nb.pred)

table(nobel.test$nobel_area, nobel.nb.pred, dnn=c("Actual", "Predicted"))
sum(diag(table(nobel.test$nobel_area, nobel.nb.pred)))
mean(nobel.nb.pred==nobel.test$nobel_area)

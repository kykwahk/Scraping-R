
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

## 레스토랑 리뷰 @오픈테이블

library(httr)
library(XML)
url <- "https://www.opentable.com/planet-hollywood-times-square"
html <- GET(url) 
html.parsed <- htmlParse(html)
library(tidyverse)

# 레스토랑 이름
name <- xpathSApply(html.parsed, "//h1", xmlValue)
name <- str_replace_all(iconv(name, to="ascii", sub=""), "\\s+", " ")
name

# 리뷰글 작성자
author <- xpathSApply(html.parsed, 
                      "//ol[@id='restProfileReviewsContent']/li/section[1]/p[1]", 
                      xmlValue)
author

# 작성 일자
date <- xpathSApply(html.parsed, 
                    "//ol[@id='restProfileReviewsContent']/li/section[2]/section[1]/p", 
                    xmlValue)
date

library(lubridate)
Sys.setlocale("LC_TIME", "English")
mdy(date)
Sys.setlocale()

dateExtract <- function(date) {
  ifelse(str_detect(date, "hours | today"), as.character(Sys.Date()),
         (ifelse(str_detect(date, "(a|1) day"), as.character(Sys.Date() - 1),
                 (ifelse(str_detect(date, "days"), 
                         as.character(Sys.Date() - parse_number(date)),
                         as.character(mdy(date)))))))
}

date.sample <- c("Dined on January 29, 2022", "Dined 11 hours ago", "Dined today",
                 "Dined a day ago", "Dined 1 day ago", "Dined 2 days ago")
Sys.setlocale("LC_TIME", "English")
map_chr(date.sample, dateExtract) %>% 
  as.Date()
Sys.setlocale()

Sys.setlocale("LC_TIME", "English")
date <- map_chr(date, dateExtract) %>% 
  as.Date()
date
Sys.setlocale()

# 리뷰글
review <- xpathSApply(html.parsed, 
                      "//ol[@id='restProfileReviewsContent']/li/section[2]/div/span[1]", 
                      xmlValue)
review

# 평점
overall <- xpathSApply(html.parsed, 
                       "//ol[@id='restProfileReviewsContent']/li/section[2]/span[2]", 
                       xmlValue) %>% 
  as.numeric()
overall

food <- xpathSApply(html.parsed, 
                    "//ol[@id='restProfileReviewsContent']/li/section[2]/span[4]", 
                    xmlValue) %>% 
  as.numeric()
food

service <- xpathSApply(html.parsed, 
                       "//ol[@id='restProfileReviewsContent']/li/section[2]/span[6]", 
                       xmlValue) %>% 
  as.numeric()
service

ambience <- xpathSApply(html.parsed, 
                        "//ol[@id='restProfileReviewsContent']/li/section[2]/span[8]", 
                        xmlValue) %>% 
  as.numeric()
ambience

# 총페이지 개수 
total.pages <- xpathSApply(html.parsed, 
                           "//footer[@data-test='reviews-pagination']//ul/li[last()]", 
                           xmlValue) %>% 
  as.numeric()
total.pages

# 오픈테이블 리뷰 데이터 추출 함수 - 현재 버전(2023.4.25)
opentableReview <- function(baseurl, n=NULL) {
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(XML)
  html <- GET(baseurl)
  html.parsed <- htmlParse(html)
  if (is.null(n)) {
    total.pages <- xpathSApply(html.parsed, 
                               "//footer[@data-test='reviews-pagination']//ul/li[last()]", 
                               xmlValue) %>% 
      as.numeric()
    n <- total.pages
  }
  opentable.review <- tibble()
  name <- xpathSApply(html.parsed, "//h1", xmlValue)
  name <- str_replace_all(iconv(name, to="ascii", sub=""), "\\s+", " ")
  dateExtract <- function(date) {
    ifelse(str_detect(date, "hours | today"), as.character(Sys.Date()),
           (ifelse(str_detect(date, "(a|1) day"), as.character(Sys.Date() - 1),
                   (ifelse(str_detect(date, "days"), 
                           as.character(Sys.Date() - parse_number(date)),
                           as.character(mdy(date)))))))
  }
  Sys.setlocale("LC_TIME", "English")
  for (i in c(1:n)) {
    if (i==1) {url <- baseurl}
    else {
      url <- str_c(baseurl, "?page=", i)
    }
    html <- GET(url)
    html.parsed <- htmlParse(html)
    author <- xpathSApply(html.parsed, 
                          "//ol[@id='restProfileReviewsContent']/li/section[1]/p[1]", 
                          xmlValue)
    date <- xpathSApply(html.parsed, 
                        "//ol[@id='restProfileReviewsContent']/li/section[2]/section[1]/p", 
                        xmlValue) %>% 
      map_chr(dateExtract) %>% 
      as.Date()
    review <- xpathSApply(html.parsed, 
                          "//ol[@id='restProfileReviewsContent']/li/section[2]/div/span[1]", 
                          xmlValue)
    overall <- xpathSApply(html.parsed, 
                           "//ol[@id='restProfileReviewsContent']/li/section[2]/span[2]", 
                           xmlValue) %>% 
      as.numeric()
    food <- xpathSApply(html.parsed, 
                        "//ol[@id='restProfileReviewsContent']/li/section[2]/span[4]", 
                        xmlValue) %>% 
      as.numeric()
    service <- xpathSApply(html.parsed, 
                           "//ol[@id='restProfileReviewsContent']/li/section[2]/span[6]", 
                           xmlValue) %>% 
      as.numeric()
    ambience <- xpathSApply(html.parsed, 
                            "//ol[@id='restProfileReviewsContent']/li/section[2]/span[8]", 
                            xmlValue) %>% 
      as.numeric()
    if (length(date) > 0 ) {
      opentable.r <- tibble(name=name, author=author, date=date, review=review, 
                            rating.overall=overall, rating.food=food,
                            rating.service=service, rating.ambience=ambience)
      opentable.review <- bind_rows(opentable.review, opentable.r)
    }
    else break
    Sys.sleep(sample(10, 1)*0.2)
  }
  Sys.setlocale()
  opentable.review <- bind_cols(id=1:nrow(opentable.review), opentable.review)
  return(opentable.review)
}

baseurl <- "https://www.opentable.com/planet-hollywood-times-square"
opentable.planet <- opentableReview(baseurl=baseurl, n=10)
opentable.planet

baseurl <- "https://www.opentable.com/planet-hollywood-times-square"
opentable.planet <- opentableReview(baseurl=baseurl)
opentable.planet

save(opentable.planet, file="opentable-planet.rda")
load("opentable-planet.rda")

baseurl <- "https://www.opentable.com/tavern-on-the-green"
opentable.tavern <- opentableReview(baseurl=baseurl)
opentable.tavern

save(opentable.tavern, file="opentable-tavern.rda")
load("opentable-tavern.rda")

# 시각화 및 분석
opentable.review <- opentable.planet
map_dfc(select(opentable.review, starts_with("rating")), table) %>% 
  rownames_to_column(var="rating.category") 

ratings <- map_dfc(select(opentable.review, starts_with("rating")), table) %>% 
  rownames_to_column(var="rating.category") %>% 
  pivot_longer(cols=rating.overall:rating.ambience) %>% 
  mutate(name=factor(.$name, levels=c("rating.overall", "rating.food",
                                      "rating.service", "rating.ambience"),
                     labels=c("Overall", "Food", "Service", "Ambience")),
         value=as.numeric(value))
ratings

# [그림 4-28]
windows(width=7.0, height=5.5)
ggplot(ratings, aes(x=name, y=value, fill=rating.category)) +
  geom_bar(position=position_dodge2(width=-0.9), color="dimgray", stat="identity") +
  scale_fill_brewer(name="Rating", palette="Blues") +
  labs(x="", y="Number of Ratings", 
       title="Review of Planet Hollywood",
       subtitle="Distribution of rating",
       caption="Source: OpenTable") +
  theme_bw() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        panel.grid=element_blank(),
        panel.border=element_blank())

summary(nchar(opentable.review$review))

# [그림 4-29]
windows(width=7.0, height=5.5)
library(scales)
ggplot(opentable.review, aes(x=nchar(review))) +
  geom_histogram(color="brown", fill="coral") + 
  scale_x_continuous(labels=comma) +
  scale_y_continuous(labels=comma) +
  labs(x="Review Length (Number of Review Characters)", y="Frequency",
       title="Review of Planet Hollywood",
       subtitle="Distribution of review length",
       caption="Source: OpenTable") +
  theme_gray() + 
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

nrow(filter(opentable.review, nchar(review) >= 1000))

# [그림 4-30]
windows(width=7.0, height=5.5)
library(scales)
ggplot(filter(opentable.review, nchar(review) < 1000), 
       aes(x=as.factor(rating.overall), y=nchar(review))) + 
  geom_boxplot(fill="goldenrod", color="dimgray") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5)) +
  scale_y_continuous(labels=comma) +
  labs(x="Rating", y="Review Length",
       title="Review of Planet Hollywood",
       subtitle="Distribution of review length by rating",
       caption="Source: OpenTable") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

library(tidytext)
install.packages("textdata") # get_sentiments() 함수를 처음 사용하기 전에 설치
get_sentiments(lexicon="afinn")
summary(get_sentiments(lexicon="afinn"))

# [그림 4-31]
windows(width=7.0, height=5.5)
library(scales)
ggplot(get_sentiments(lexicon="afinn"), aes(x=value)) +
  geom_bar(color="darkblue", fill="cornflowerblue", width=0.8) + 
  scale_x_continuous(breaks=c(-5:5)) +
  scale_y_continuous(labels=comma) +
  labs(x="Sentiment Score", y="Frequency",
       title="AFINN Lexicon Dictionary",
       subtitle="Distribution of negative to positive sentiment score",
       caption="Source: tidytext package in R") +
  theme_minimal() + 
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

opentable.review.words <- opentable.review %>%
  mutate(review=iconv(review, to="ascii", sub="")) %>% 
  unnest_tokens(output=word, input=review, token="words") %>%
  anti_join(stop_words, by="word") %>%
  select(id, rating.overall, word)

opentable.review.words

count(opentable.review.words, word, sort=TRUE)

inner_join(opentable.review.words, get_sentiments(lexicon="afinn"), by="word")

opentable.sent.review <- opentable.review.words %>%
  inner_join(get_sentiments(lexicon="afinn"), by="word") %>%
  group_by(id, rating.overall) %>%
  summarise(score_avg=mean(value, na.rm=TRUE)) %>%
  ungroup()
opentable.sent.review

# [그림 4-32]
windows(width=7.0, height=5.5)
ggplot(opentable.sent.review, aes(x=as.factor(rating.overall), y=score_avg)) + 
  geom_boxplot(fill="lavenderblush", color="black") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5)) +
  labs(x="Rating", y="Average Sentiment Score of Words",
       title="Review of Planet Hollywood",
       subtitle="Distribution of average sentiment score by review rating",
       caption="Source: OpenTable") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

opentable.sent.word <- opentable.review.words %>%
  count(id, rating.overall, word) %>%
  group_by(word) %>%
  summarise(in_review=n(),
            in_use=sum(n),
            rating_avg=mean(rating.overall)) %>%
  filter(in_review >= 2) %>%
  arrange(rating_avg) %>%
  inner_join(get_sentiments(lexicon="afinn"), by="word") %>%
  ungroup()
opentable.sent.word

# [그림 4-33]
windows(width=7.0, height=5.5)
ggplot(opentable.sent.word, aes(x=as.factor(value), y=rating_avg)) + 
  geom_boxplot(fill="aquamarine", color="black") +
  scale_y_continuous(breaks=c(1, 2, 3, 4, 5), limits=c(1, 5)) +
  labs(x="Sentiment Score of Words", y="Average Rating of Reviews",
       title="Review of Planet Hollywood",
       subtitle="Distribution of average review rating by sentimet score of word",
       caption="Source: OpenTable") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

# 감성어휘 사전: bing
# lemmatization: 단어의 표제어로 변환
library(tidytext)
get_sentiments(lexicon="bing")
table(get_sentiments(lexicon="bing")$sentiment)

library(textstem)
opentable.review.words <- opentable.review %>% 
  mutate(review=lemmatize_strings(iconv(review, to="ascii", sub=""))) %>% 
  unnest_tokens(output=word, input=review, token="words") %>%
  anti_join(stop_words, by="word") %>%
  select(id, date, rating.overall, word) 
opentable.review.words

opentable.sent.review <- opentable.review.words %>%
  inner_join(get_sentiments(lexicon="bing"), by="word") %>%
  count(id, date, rating.overall, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>%
  mutate(sentiment=positive - negative) %>%
  ungroup()

opentable.sent.review

summary(opentable.sent.review$sentiment)

# [그림 4-34] 
outlier <- quantile(opentable.sent.review$sentiment, c(0.01, 0.99))
outlier
windows(width=7.0, height=5.5)
ggplot(filter(opentable.sent.review, sentiment > outlier[1], sentiment < outlier[2]), 
       aes(x=as.factor(rating.overall), y=sentiment)) + 
  geom_violin(fill="wheat", width=1.1) +
  geom_boxplot(fill="tomato", color="dimgray", width=0.2) +
  scale_x_discrete(breaks=c(1:5)) +
  scale_y_continuous(breaks=seq(from=trunc(outlier[1]), to=trunc(outlier[2]), by=2)) +
  labs(x="Rating", y="Sentiment Score (# of positives - # of negatives)", 
       title="Review of Planet Hollywood",
       subtitle="Distribution of sentiment score by review rating",
       caption="Source: OpenTable") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold")) +
  coord_flip() 

opentable.review.words
opentable.sent.words <- opentable.review.words %>%
  inner_join(get_sentiments(lexicon="bing"), by="word") %>%
  count(sentiment, word) %>%
  ungroup() %>%
  filter(n >= 15) %>%
  mutate(nsign=ifelse(sentiment=="negative", -n, n))

opentable.sent.words

# [그림 4-35]  
windows(width=7.0, height=5.5)
library(scales)
ggplot(opentable.sent.words,
       aes(x=reorder(word, nsign), y=nsign,
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_bar(stat="identity", color="lightslategray", width=0.8) +
  geom_text(aes(label=n), size=3, color="black",
            hjust=ifelse(opentable.sent.words$nsign < 0, 1.1, -0.1)) +
  scale_fill_manual(values=c("cornflowerblue", "tomato")) +
  scale_y_continuous(breaks=pretty(opentable.sent.words$nsign),
                     labels=abs(pretty(opentable.sent.words$nsign))) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold", size=10)) +
  labs(x=NULL, y="Count",
       title="Review of Planet Hollywood",
       subtitle="Top words contributing to sentiment",
       caption="Source: OpenTable") +
  coord_flip()

library(lubridate)
opentable.review$ym <- floor_date(x=opentable.review$date, unit="month")
head(opentable.review$ym)

opentable.agg <- opentable.review %>%
  group_by(name, ym) %>%
  summarise(rating.overall=mean(rating.overall, na.rm=TRUE), n=n()) %>%
  ungroup()
opentable.agg

# [그림 4-36]
windows(width=8.0, height=5.0)
ggplot(opentable.agg, aes(x=ym, y=rating.overall)) +
  geom_line(color="khaki4", size=1) +
  geom_smooth(method="loess", color="dodgerblue", size=1.5) +
  scale_x_date(date_labels="%Y", date_breaks="year") +
  scale_y_continuous(limits=c(1, 5), breaks=c(1:5)) +
  theme_gray() +
  theme(text=element_text(family="sans"),
        plot.title=element_text(face="bold"))+
  labs(x=NULL, y="Rating",
       title=sprintf("Review of %s", opentable.agg$name),
       subtitle="Rating scores over time",
       caption="Source: OpenTable")

# [그림 4-37]
windows(width=8.0, height=5.0)
ggplot(opentable.agg, aes(x=ym, y=n)) +
  geom_line(color="plum4", size=1) +
  geom_smooth(method="loess", color="tomato", size=1.5) +
  scale_x_date(date_labels="%Y", date_breaks="year") +
  theme_gray() +
  theme(text=element_text(family="sans"),
        plot.title=element_text(face="bold"))+
  labs(x=NULL, y="Number of Reviews",
       title=sprintf("Review of %s", opentable.agg$name),
       subtitle="Number of reviews over time",
       caption="Source: OpenTable")  

# 여러 레스토랑 비교
baseurl.benjamin <- "https://www.opentable.com/r/benjamin-prime-new-york"
baseurl.royal <- "https://www.opentable.com/r/royal-35-steakhouse-new-york"
baseurl.stk <- "https://www.opentable.com/r/stk-rooftop-new-york"
opentable.benjamin <- opentableReview(baseurl=baseurl.benjamin)
opentable.royal <- opentableReview(baseurl=baseurl.royal)
opentable.stk <- opentableReview(baseurl=baseurl.stk)
nrow(opentable.benjamin); nrow(opentable.royal); nrow(opentable.stk)

save(opentable.benjamin, file="opentable-benjamin.rda")
save(opentable.royal, file="opentable-royal.rda")
save(opentable.stk, file="opentable-stk.rda")
load("opentable-benjamin.rda")
load("opentable-royal.rda")
load("opentable-stk.rda")

opentable.steak <- rbind(opentable.benjamin, opentable.royal, opentable.stk)
opentable.steak
View(opentable.steak)

save(opentable.steak, file="opentable-steak.rda")
load("opentable-steak.rda")

opentable.review <- opentable.steak
levels(factor(opentable.review$name))
stime <- 
  max(min(opentable.review[opentable.review$name==levels(factor(opentable.review$name))[1],]$date), 
      min(opentable.review[opentable.review$name==levels(factor(opentable.review$name))[2],]$date),
      min(opentable.review[opentable.review$name==levels(factor(opentable.review$name))[3],]$date))
stime

opentable.review <- filter(opentable.review, date >= stime)

library(lubridate)
opentable.review$ym <- floor_date(x=opentable.review$date, unit="month")
opentable.agg <- opentable.review %>%
  group_by(name, ym) %>%
  summarise(rating.overall=mean(rating.overall, na.rm=TRUE), n=n()) %>%
  ungroup()
opentable.agg

# [그림 4-38]
windows(width=8.0, height=5.0)
ggplot(opentable.agg, aes(x=ym, y=rating.overall, color=name)) +
  geom_line(linetype="dotted") +
  geom_point(shape=21, fill="gray30") +
  geom_smooth(method="loess") +
  scale_x_date(date_labels="%Y", date_breaks="year") +
  scale_y_continuous(limits=c(1, 5), breaks=c(1:5)) +
  theme_bw() +
  theme(text=element_text(family="sans"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(size=8, face="bold"),
        legend.position="bottom",
        legend.title=element_blank()) +
  labs(x=NULL, y="Rating",
       title="Review of Steak Restaurants in New York",
       subtitle=sprintf("Rating scores of %s, %s, and %s over time",
                        levels(factor(opentable.review$name))[1], 
                        levels(factor(opentable.review$name))[2], 
                        levels(factor(opentable.review$name))[3]),
       caption="Source: OpenTable")

library(tidytext)
library(textstem)
opentable.review <- opentable.steak
opentable.review.words <- opentable.review %>% 
  mutate(review=lemmatize_strings(iconv(review, to="ascii", sub=""))) %>% 
  unnest_tokens(output=word, input=review, token="words") %>%
  anti_join(stop_words, by="word") %>%
  select(id, name, date, rating.overall, word) 
opentable.review.words

opentable.sent.review <- opentable.review.words %>%
  inner_join(get_sentiments(lexicon="bing"), by="word") %>%
  count(name, id, date, rating.overall, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>%
  mutate(sentiment=positive - negative) %>%
  ungroup()
opentable.sent.review

names <- levels(factor(opentable.sent.review$name))
names
stime <- 
  max(min(opentable.sent.review[opentable.sent.review$name==names[1],]$date), 
      min(opentable.sent.review[opentable.sent.review$name==names[2],]$date),
      min(opentable.sent.review[opentable.sent.review$name==names[3],]$date))
stime

# [그림 4-39]
windows(width=9.0, height=7.0)
ggplot(opentable.sent.review[opentable.sent.review$date >= stime,], 
       aes(date, sentiment, color=name)) +
  geom_line(size=1, show.legend=FALSE) +
  facet_wrap(~name, ncol=1) +
  scale_x_date(date_labels="%Y", date_breaks="year") +
  theme_minimal() +
  labs(x=NULL, y="Sentiment",
       title="Review of Steak Restaurants in New York",
       subtitle=sprintf("Sentiment scores of %s, %s, and %s over time",
                        levels(factor(opentable.review$name))[1], 
                        levels(factor(opentable.review$name))[2], 
                        levels(factor(opentable.review$name))[3]),
       caption="Source: OpenTable") +
  theme(strip.text.x=element_text(face="italic"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.title=element_text(face="bold"))

# 머신러닝: RTextTools
opentable.review <- opentable.steak
set.seed(123)
opentable.review <- slice_sample(opentable.review, n=nrow(opentable.review))
opentable.review

library(RTextTools)
opentable.dtm <- create_matrix(textColumns=opentable.review$review, language="english",
                               removeNumbers=TRUE, removePunctuation=TRUE,
                               removeStopwords=FALSE, stemWords=FALSE,
                               stripWhitespace=TRUE, toLower=TRUE,
                               removeSparseTerms=0.99)
opentable.dtm
opentable.dtm <- as_tibble(as.matrix(opentable.dtm))
opentable.dtm

train.end <- round(nrow(opentable.dtm)*0.7, 0)
test.end <- nrow(opentable.dtm)
train.end; test.end - train.end
my.container <- create_container(matrix=opentable.dtm, 
                                 labels=opentable.review$rating.overall,
                                 trainSize=1:train.end, 
                                 testSize=(train.end+1):test.end,
                                 virgin=FALSE)

set.seed(123)
my.classifier <- train_models(container=my.container, algorithms=c("RF", "SVM"))

print_algorithms()

my.prediction <- classify_models(container=my.container, models=my.classifier)

str(my.prediction)
head(my.prediction)

table(opentable.review$rating.overall[(train.end+1):test.end], 
      my.prediction$FORESTS_LABEL, dnn=c("Actual", "Predcited"))
mean(opentable.review$rating.overall[(train.end+1):test.end]==my.prediction$FORESTS_LABEL)
table(opentable.review$rating.overall[(train.end+1):test.end], 
      my.prediction$SVM_LABEL, dnn=c("Actual", "Predcited"))
mean(opentable.review$rating.overall[(train.end+1):test.end]==my.prediction$SVM_LABEL)

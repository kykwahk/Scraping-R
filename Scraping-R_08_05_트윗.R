
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제8장. API ##
################

##############
## 8.4 사례 ##
##############

## 트윗 @트위터

# rtweet 패키지
install.packages("rtweet")
library(rtweet)

devtools::install_github("ropensci/rtweet", build_vignettes=TRUE)
library(rtweet)
vignette("rtweet")
vignette("auth")

bearer.token <- "YourBearerToken"
auth <- rtweet_app(bearer_token=bearer.token)
class(auth)

tweets <- search_tweets(q="metaverse", token=auth)

auth_as(auth)
tweets <- search_tweets(q="metaverse")

auth_save(auth, name="myTwitterAuth")
auth_as("myTwitterAuth")

auth_list()

api.key <- "YourAPIKey"
api.key.secret <- "YourAPIKeySecret"
access.token <- "YourAccessToken"
access.token.secret <- "YourAccessTokenSecret"
auth <- rtweet_bot(api_key=api.key, api_secret=api.key.secret,
                   access_token=access.token, access_secret=access.token.secret)
class(auth)

library(rtweet)
library(tidyverse)
auth_as("myTwitterAuth")

tweets <- search_tweets(q="artificial intelligence", n=300)
tweets <- tweets %>% 
  as_tibble()
tweets
names(tweets)
tweets$full_text[1:3]

length(unique(tweets$full_text))

tweets <- search_tweets(q="artificial intelligence", n=300, include_rts=FALSE)
nrow(tweets)
length(unique(tweets$full_text))

tweets <- search_tweets(q="artificial intelligence -filter:retweets", n=300)

tweets.newer <- search_tweets(q="artificial intelligence", n=100, include_rts=FALSE,
                              since_id=tweets)
tweets.newer <- tweets.newer %>% 
  as_tibble()
tweets.newer

tweets.older <- search_tweets(q="artificial intelligence", n=100, include_rts=FALSE,
                              max_id=tweets)
tweets.older <- tweets.older %>% 
  as_tibble()
tweets.older

tweets.users <- users_data(tweets) %>% 
  as_tibble()
tweets.users
names(tweets.users)
tweets.users$screen_name[1:3]

tweets <- search_tweets(q="artificial intelligence", n=300, include_rts=FALSE,
                        parse=FALSE)
class(tweets)
str(tweets, max.level=2)

tweets <- tweets %>% 
  map_dfr("statuses") %>% 
  as_tibble()
tweets
names(tweets)
tweets$user$screen_name[1:3]

tweets <- search_tweets(q="artificial intelligence", n=50000, include_rts=FALSE, 
                        parse=FALSE, retryonratelimit=TRUE)
tweets <- tweets %>% 
  map_dfr("statuses") %>% 
  as_tibble()
nrow(tweets)

tweets.kor <- search_tweets(q="인공지능", n=300, include_rts=FALSE)
tweets.kor$full_text[1:3]

tweets.multquery <- search_tweets2(q=c("BLACKPINK", "BTS"), n=1000)
nrow(tweets.multquery)
table(tweets.multquery$query)

tweets <- search_tweets2(q=c("apple iphone", "samsung galaxy"),
                         n=20000, lang="en", include_rts=FALSE, 
                         retryonratelimit=TRUE, parse=FALSE)

Sys.setlocale("LC_TIME", "English")
tweets.smartp <- tweets %>% 
  map(~map_dfr(.x, "statuses")) %>%
  map(as_tibble) %>% 
  bind_rows(.id="query") %>% 
  mutate(created_at=as.POSIXct(created_at, "%a %b %d %H:%M:%S +0000 %Y", 
                               tz="UTC"))
Sys.setlocale()

save(tweets.smartp, file="twitter-smartp.rda")
load("twitter-smartp.rda")

nrow(tweets.smartp)
nrow(filter(tweets.smartp, query=="apple iphone"))
nrow(filter(tweets.smartp, query=="samsung galaxy"))
min(tweets.smartp$created_at); max(tweets.smartp$created_at)

stime.iphone <- min(filter(tweets.smartp, query=="apple iphone")$created_at)
stime.galaxy <- min(filter(tweets.smartp, query=="samsung galaxy")$created_at)
stime.iphone; stime.galaxy

stime.smartp <- max(stime.iphone, stime.galaxy)
stime.smartp

library(lubridate)
tweets.agg <- filter(tweets.smartp, created_at >= stime.smartp) %>%
  group_by(query) %>%
  mutate(time=floor_date(x=created_at, unit="hours")) %>%
  count(query, time) %>%
  slice(2:(n()-1)) %>%
  ungroup()
tweets.agg

# [그림 8-29]
windows(width=8.0, height=5.5)
Sys.setlocale("LC_TIME", "English")
ggplot(tweets.agg, aes(x=time, y=n, fill=query, color=query)) +
  geom_area(position="identity", alpha=0.3) +
  geom_line(size=1.5) + 
  scale_fill_manual(labels=c("Apple iPhone", "Samsung Galaxy"),
                    values=c("orangered", "deepskyblue2")) +
  scale_color_manual(labels=c("Apple iPhone", "Samsung Galaxy"),
                     values=c("orangered", "deepskyblue2")) +
  scale_x_datetime(date_labels="%b %d %H:%M", date_breaks="days") +
  labs(x=NULL, y="Number of Tweets",
       title="Twitter Statuses over Time",
       subtitle="Tweets on topics of Apple iPhone and Samsung Galaxy",
       caption="Source: Twitter") + 
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.text.x=element_text(size=8, vjust=0.5),
        panel.grid.minor=element_blank(),
        legend.position="bottom", 
        legend.title=element_blank()) 
Sys.setlocale()

library(tm)
text <- "has anyone else’s phone be dying for no reason but when you turn it back on it has like 80% ?? Apple get your shit together im not buying an iphone https://t.co/6APiSWtXsg"
text <- text %>%
  gsub("https?[:.]+\\S+", "", .) %>%
  removeWords(stopwords("english")) %>% 
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  iconv(., to="ascii", sub="") %>% 
  trimws()
text
words <- strsplit(text, " ")
words

library(tidytext)
get_sentiments(lexicon="bing")
table(get_sentiments("bing")$sentiment)

pos.words <- get_sentiments("bing")$word[get_sentiments("bing")$sentiment=="positive"]
neg.words <- get_sentiments("bing")$word[get_sentiments("bing")$sentiment=="negative"]

words[[1]][words[[1]] %in% pos.words]
words[[1]][words[[1]] %in% neg.words]
pos.match <- sum(words[[1]] %in% pos.words)
neg.match <- sum(words[[1]] %in% neg.words)
pos.match; neg.match
scores <- pos.match - neg.match
scores

cleanText <- function(text) {
  text <- text %>%
    gsub("https?[:.]+\\S+", "", .) %>%
    removeWords(stopwords("english")) %>% 
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    iconv(., to="ascii", sub="") %>% 
    trimws()
  return(text)
}
scoreWords <- function(words, pos.words, neg.words) {
  pos.match <- sum(words %in% pos.words)
  neg.match <- sum(words %in% neg.words)
  return(pos.match - neg.match)
}
evalTweets <- function(tweets, pos.words, neg.words, keyword) {
  keyword.idx <- grep(keyword, tweets$query, ignore.case=TRUE)
  text <- cleanText(tweets$full_text[keyword.idx])
  words <- strsplit(text, " ")
  scores <- sapply(words, scoreWords, pos.words, neg.words)
  n <- length(scores)
  positive <- round(sum(scores > 0)/n * 100, 1)
  negative <- round(sum(scores < 0)/n * 100, 1)
  neutral <- 100 - (positive + negative)
  sprintf("%d tweets about %s: %.1f%% positive, %.1f%% negative, %.1f%% neutral", 
          n, keyword, positive, negative, neutral)
}

evalTweets(tweets.smartp, pos.words, neg.words, keyword="samsung galaxy")
evalTweets(tweets.smartp, pos.words, neg.words, keyword="apple iphone")

library(lubridate)
tweets.smartp$clean_text <- cleanText(tweets.smartp$full_text)
tweets.sentiment <- tweets.smartp %>%
  select(created_at, clean_text, query) %>%
  unnest_tokens(output=word, input=clean_text, token="words") %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  mutate(created_at=round_date(x=created_at, unit="hours")) %>%
  count(query, created_at, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>%
  mutate(sentiment=positive - negative)

tweets.sentiment
summary(tweets.sentiment$sentiment)
by(tweets.sentiment[c("negative", "positive", "sentiment")], 
   INDICES=tweets.sentiment$query, FUN=summary)

stime.smartp <- 
  max(min(filter(tweets.sentiment, query=="apple iphone")$created_at),
      min(filter(tweets.sentiment, query=="samsung galaxy")$created_at))
stime.smartp

# [그림 8-30]
windows(width=8.0, height=7.0)
Sys.setlocale("LC_TIME", "English")
ggplot(filter(tweets.sentiment, created_at >= stime.smartp), 
       aes(created_at, sentiment, fill=query)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~factor(query, labels=c("Apple iPhone", "Samsung Galaxy")), nrow=2) +
  scale_fill_viridis_d(begin=0.1, end=0.65, direction=-1, option="plasma") +
  scale_x_datetime(date_labels="%b %d", date_breaks="days", expand=c(0, 0)) +
  labs(x="", y="Sentiment Score (aggregated by hours)", 
       title="Sentiment Analysis of Twitter Statuses over Time",
       subtitle="Sentiment scores on topics of Apple iPhone and Samsung Galaxy",
       caption="Source: Twitter") +
  theme_minimal() +
  theme(strip.background=element_blank(), 
        strip.text=element_text(color="turquoise4", face="bold"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        panel.grid.minor=element_blank())
Sys.setlocale()

library(syuzhet)
sentiment <- get_nrc_sentiment(tweets.smartp$clean_text)
as_tibble(sentiment)

tweets.smartp <- cbind(tweets.smartp, sentiment)

tweets.sentiment <- tweets.smartp %>%
  select(query, anger:positive) %>%
  pivot_longer(cols=!query, names_to="sentiment", values_to="score") %>%
  group_by(query, sentiment) %>%
  summarise(score=mean(score, na.rm=TRUE), n=n()) %>%
  ungroup()

tweets.sentiment

tweets.sentiment$sentiment <- 
  factor(tweets.sentiment$sentiment,
         levels=c("anger", "anticipation", "disgust", "fear", "joy",
                  "sadness", "surprise", "trust", "negative", "positive"))

# [그림 8-31]
windows(width=7.0, height=5.5)
ggplot(tweets.sentiment, aes(x=sentiment, y=score, fill=query)) +
  geom_bar(position=position_dodge(width=-0.9), stat="identity", color="gray50") +
  geom_text(aes(label=format(round(score, 2), nsmall=2)), size=3, fontface="bold",
            position=position_dodge(width=-0.9), hjust=-0.1) +
  scale_fill_manual(labels=c("Apple iPhone", "Samsung Galaxy"),
                    values=c("violet", "slateblue")) +
  scale_x_discrete(limits=rev(levels(factor(tweets.sentiment$sentiment)))) +
  coord_flip() +
  labs(x=NULL, y="Sentiment Score (average)", 
       title="Sentiment Analysis of Twitter Statuses",
       subtitle="Sentiment scores on topics of Apple iPhone and Samsung Galaxy",
       caption="Source: Twitter") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line.x=element_line(color="gray"),
        panel.grid.minor=element_blank(),
        legend.position="bottom", 
        legend.title=element_blank())

library(lubridate)
tweets.sentiment <- tweets.smartp %>%
  select(created_at, query, anger:positive) %>%
  mutate(created_at=round_date(x=created_at, unit="3 hours")) %>%
  pivot_longer(cols=!c(created_at, query), names_to="sentiment", values_to="score") %>%
  group_by(created_at, query, sentiment) %>%
  summarise(score=mean(score, na.rm=TRUE), n=n()) %>%
  ungroup()
tweets.sentiment

stime.smartp <- 
  max(min(filter(tweets.sentiment, query=="apple iphone")$created_at),
      min(filter(tweets.sentiment, query=="samsung galaxy")$created_at))
stime.smartp

tweets.sentiment$sentiment <- 
  factor(tweets.sentiment$sentiment,
         levels=c("anger", "anticipation", "disgust", "fear", "joy",
                  "sadness", "surprise", "trust", "negative", "positive"))

# [그림 8-32]
windows(width=7.0, height=9.0)
Sys.setlocale("LC_TIME", "English")
ggplot(filter(tweets.sentiment, created_at >= stime.smartp), 
       aes(x=created_at, y=score, color=query)) +
  geom_point() +
  geom_smooth(method="loess") +
  scale_x_datetime(date_labels="%b %d", date_breaks="days", expand=c(0, 0)) +
  scale_color_discrete(labels=c("Apple iPhone", "Samsung Galaxy")) +
  facet_wrap(~ sentiment, scale="free_y", nrow=5) +
  labs(x=NULL, y="Sentiment Score (aggregated and averaged by 3 hours)",
       title="Sentiment Analysis of Twitter Statuses over Time",
       subtitle="Sentiment scores on topics of Apple iPhone and Samsung Galaxy",
       caption="Source: Twitter") +
  theme_bw() +
  theme(strip.background=element_blank(), 
        strip.text=element_text(color="slategray", face="bold"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(size=8, face="bold"),
        legend.position="bottom",
        legend.title=element_blank())
Sys.setlocale()

# academictwitteR 패키지

install.packages("academictwitteR")
devtools::install_github("cjbarrie/academictwitteR", build_vignettes=TRUE) 
library(academictwitteR)

bearer.token <- "YourBearerToken"

tweets <- get_all_tweets(query="인공지능",
                         start_tweets="2021-12-05T00:00:00Z",
                         end_tweets="2021-12-20T23:59:59Z",
                         bearer_token=bearer.token, n=30000)

save(tweets, file="twitter-ai.rda")
load("twitter-ai.rda")

class(tweets)
nrow(tweets)
names(tweets)

library(tidyverse)
library(lubridate)
tweets <- tweets %>% 
  as_tibble() %>% 
  mutate(created_at=ymd_hms(created_at))
tweets
min(tweets$created_at); max(tweets$created_at)

tweets.agg <- tweets %>% 
  mutate(time=floor_date(x=created_at, unit="hours")) %>% 
  count(time) %>%
  slice(2:(n()-1))
tweets.agg

# [그림 8-33]
windows(width=8.0, height=5.5)
Sys.setlocale("LC_TIME", "English")
ggplot(tweets.agg, aes(x=time, y=n)) +
  geom_ribbon(aes(ymin=n-30, ymax=n+30), fill="gray90") +
  geom_line(size=1.5, color="darkturquoise") + 
  scale_x_datetime(date_labels="%b %d\n(%a)", date_breaks="days") +
  labs(x=NULL, y="트윗 개수",
       title="트윗 발생량 추이",
       subtitle="인공지능 주제 트윗",
       caption="출처: 트위터") + 
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=16),
        plot.caption=element_text(size=13),
        axis.text=element_text(face="bold"),
        axis.text.x=element_text(size=8),
        axis.line=element_line(color="gray"),
        panel.grid.minor=element_blank()) 
Sys.setlocale()

tweets <- get_all_tweets(query="metaverse",
                         start_tweets="2021-12-05T00:00:00Z",
                         end_tweets="2021-12-20T23:59:59Z",
                         bearer_token=bearer.token, n=3000,
                         file="twitter-metaverse.rds")
tweets <- readRDS("twitter-metaverse.rds")
class(tweets)
nrow(tweets)
names(tweets)

tweets <- get_all_tweets(query="metaverse",
                         start_tweets="2021-12-05T00:00:00Z",
                         end_tweets="2021-12-20T23:59:59Z",
                         bearer_token=bearer.token, n=3000, 
                         data_path="twitterdata/",
                         bind_tweets=FALSE)

tweets <- bind_tweets(data_path="twitterdata/", output_format="raw")
class(tweets)
str(tweets, max.level=1)
names(tweets$tweet.main)
names(tweets$user.main)

tweets <- bind_tweets(data_path="twitterdata/", output_format="tidy")
tweets
names(tweets)

tweets <- get_all_tweets(users="elonmusk",
                         start_tweets="2021-01-01T00:00:00Z",
                         end_tweets="2021-12-20T23:59:59Z",
                         bearer_token=bearer.token, n=Inf)
tweets <- tweets %>% 
  as_tibble()
tweets
head(tweets[c("created_at", "text")], 3)
tail(tweets[c("created_at", "text")], 3)

tweets <- get_all_tweets(query="space",
                         users="elonmusk",
                         start_tweets="2021-01-01T00:00:00Z",
                         end_tweets="2021-12-20T23:59:59Z",
                         bearer_token=bearer.token, n=Inf)
tweets <- tweets %>% 
  as_tibble()
head(tweets[c("created_at", "text")], 3)
tail(tweets[c("created_at", "text")], 3)

tweets <- get_all_tweets(query="BTS has:geo -is:retweet -is:nullcast",
                         start_tweets="2021-01-01T00:00:00Z",
                         end_tweets="2021-12-31T23:59:59Z",
                         bearer_token=bearer.token, n=300000)

save(tweets, file="twitter-bts.rda")
load("twitter-bts.rda")

tweets <- tweets %>% 
  as_tibble()
tweets
names(tweets)

round(sort(table(tweets$lang), decreasing=TRUE)[1:10]/sum(table(tweets$lang))*100, 1)

library(reshape2)
tweets.geo <- tweets$geo$coordinates$coordinates %>% 
  map(~paste(.x, collapse=" ")) %>% 
  map(~colsplit(.x, pattern=" ", names=c("lng", "lat"))) %>% 
  map_dfr(bind_rows) %>% 
  as_tibble()
tweets.geo

filter(tweets.geo, !is.na(tweets.geo$lng | tweets.geo$lat))
sum(!is.na(tweets.geo$lng | tweets.geo$lat))

# [그림 8-34]
map.data <- map_data(map="world")
windows(width=7.0, height=5.0)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="honeydew", color="dimgray", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(xlim=c(-180, 180), ylim=c(-55, 90), ratio=1.3) +
  geom_point(data=tweets.geo, position="jitter",
             aes(x=lng, y=lat), shape=21, size=1, stroke=1, 
             color="black", fill="red", alpha=0.5) +
  labs(title="Tweets Distribution",
       subtitle="Tweets on BTS",
       caption="Source: Twitter") +
  theme_void() +
  theme(plot.title=element_text(face="bold"))

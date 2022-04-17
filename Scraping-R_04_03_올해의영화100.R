
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

## 올해의 영화 100 @IMDB

library(xml2)
library(XML)
url <- "https://www.imdb.com/search/title/?title_type=feature&release_date=2005-01-01,2005-12-31&count=100"
html <- read_html(url)
html.parsed <- htmlParse(html)

# 순위
rank <- xpathSApply(html.parsed, 
                    "//span[@class='lister-item-index unbold text-primary']", 
                    xmlValue)
rank
rank <- as.numeric(rank)
rank

library(dplyr)
library(readr)
rank <- xpathSApply(html.parsed, 
                    "//span[@class='lister-item-index unbold text-primary']", 
                    xmlValue) %>% 
  parse_number()
rank  

# 제목
title <- 
  xpathSApply(html.parsed, 
              "//span[@class='lister-item-index unbold text-primary']
              /following-sibling::a", 
              xmlValue)
title

xpathSApply(html.parsed, "//h3[@class='lister-item-header']/a", xmlValue)

# 줄거리
description <- xpathSApply(html.parsed, "//p[@class='text-muted']", xmlValue)
library(stringr)
description <- str_trim(description)
description

xpathSApply(html.parsed, "//p[@class='text-muted']", xmlValue, trim=TRUE)

# 상영 시간
runtime <- xpathSApply(html.parsed, "//span[@class='runtime']", xmlValue)
runtime
runtime <- as.numeric(gsub(" min", "", runtime))
runtime

runtime <- xpathSApply(html.parsed, "//span[@class='runtime']", xmlValue) %>% 
  parse_number()
runtime

# 장르
genre <- xpathSApply(html.parsed, "//span[@class='genre']", xmlValue)
genre

genre <- xpathSApply(html.parsed, "//span[@class='genre']", xmlValue, trim=TRUE)
genre

genre <- gsub(",.*", "", genre)
genre <- as.factor(genre)
genre

library(stringr)
genre <- xpathSApply(html.parsed, "//span[@class='genre']", xmlValue, trim=TRUE) %>% 
  str_extract("\\w+") %>% 
  as.factor()
genre

# 평점
rating <- xpathSApply(html.parsed, "//div[@name='ir']/strong", xmlValue)
rating

xpathSApply(html.parsed, "//div[@name='ir']", xmlGetAttr, "data-value")

rating <- as.numeric(rating)
rating

rating <- xpathSApply(html.parsed, "//div[@name='ir']/strong", xmlValue) %>% 
  as.numeric()
rating

summary(rating)

# 감독
director <- xpathSApply(html.parsed, 
                        "//div[@class='lister-item-content']/p[@class='']/a[1]", 
                        xmlValue)
director

xpathSApply(html.parsed, 
            "//div[@class='lister-item-content']/p[3]/a[1]", xmlValue)

# 투표수
vote <- xpathSApply(html.parsed, 
                    "//p[@class='sort-num_votes-visible']/span[@name='nv'][1]", 
                    xmlValue)
vote
vote <- as.numeric(gsub(",", "", vote))
vote

vote <- xpathSApply(html.parsed, 
                    "//p[@class='sort-num_votes-visible']/span[@name='nv'][1]", 
                    xmlValue) %>% 
  parse_number()
vote

summary(vote)

xpathSApply(html.parsed, 
            "//p[@class='sort-num_votes-visible']/span[2]", xmlValue) %>% 
  parse_number()

xpathSApply(html.parsed, 
            "//p[@class='sort-num_votes-visible']/span[2]", 
            xmlGetAttr, "data-value") %>% 
  parse_number()

# 수익
gross <- xpathSApply(html.parsed, 
                     "//p[@class='sort-num_votes-visible']/span[@name='nv'][2]", 
                     xmlGetAttr, "data-value")
gross <- xpathSApply(html.parsed, 
                     "//p[@class='sort-num_votes-visible']/span[5]", 
                     xmlGetAttr, "data-value")
gross
length(gross)

gross.ndoes <- getNodeSet(html.parsed, "//p[@class='sort-num_votes-visible']")
gross.ndoes

gross <- 
  sapply(gross.ndoes, 
         function(x) {
           val <- unlist(xpathSApply(x, "./span[5]", xmlGetAttr, "data-value"))
           if (is.null(val)) val <- NA 
           else val
           }
         )
gross
length(gross)

gross <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']", 
                     function(x) {
                       val <- unlist(xpathSApply(x, "./span[5]", 
                                                 xmlGetAttr, "data-value"))
                       if (is.null(val)) val <- NA 
                       else val
                       }
                     )
gross
length(gross)

xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']", 
            function(x) {
              if (xpathSApply(x, "boolean(./span[5])"))
               {xpathSApply(x, "./span[5]" ,xmlGetAttr, "data-value")}
              else {NA}
              }
            )

gross <- as.numeric(gsub(",", "", gross))
gross

gross <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']", 
                     function(x) {
                       val <- unlist(xpathSApply(x, "./span[5]", 
                                                 xmlGetAttr, "data-value"))
                       if (is.null(val)) val <- NA 
                       else val
                       }
                     ) %>% 
  parse_number()
summary(gross, na.rm=TRUE)

library(tibble)
imdb.movie100 <- tibble(rank=rank, title=title, description=description,
                        runtime=runtime, genre=genre, rating=rating, 
                        director=director, vote=vote, gross=gross)
imdb.movie100

save(imdb.movie100, file="imdb-movie100.rda")
load("imdb-movie100.rda")

# [그림 4-25] 
library(ggplot2)
library(scales)
library(tidyr)
windows(width=7.0, height=5.5)
ggplot(drop_na(imdb.movie100, gross), aes(x=genre, y=gross/1000000)) + 
  geom_boxplot(fill="plum2", color="black") +
  geom_point(position="jitter", color="blue", alpha=0.5) +
  geom_text(aes(label=ifelse(rank <= 3, paste(rank, "-", title),"")), 
            col="red", fontface="bold", size=3, hjust=0, vjust=0) +
  scale_y_continuous(labels=comma) +
  labs(x="Genre", y="Gross (Million Dollars)", 
       title="Most Popular Films 100 of the Year",
       subtitle="Distribution of gross with rank top 3 films",
       caption="Source: IMDb") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

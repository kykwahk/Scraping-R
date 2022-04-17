
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제7장 JSON ##
################

##############
## 7.3 사례 ##
##############

## 연구 키워드 @NASA

library(jsonlite)
url <- "https://data.nasa.gov/data.json"
nasa <- fromJSON(url)
names(nasa)
names(nasa$dataset)

class(nasa$dataset$identifier)
nasa$dataset$identifier[1:3]
class(nasa$dataset$keyword)
nasa$dataset$keyword[1:3]

library(tidyverse)
tibble(id=nasa$dataset$identifier, keyword=nasa$dataset$keyword)
nasa.keyword <- tibble(id=nasa$dataset$identifier,
                       keyword=nasa$dataset$keyword) %>%
  unnest(keyword)
nasa.keyword

sum(is.na(nasa.keyword$id))

nasa.keyword %>%
  count(keyword, sort=TRUE)

library(widyr)
keyword.pair <- nasa.keyword %>%
  pairwise_count(item=keyword, feature=id, sort=TRUE, upper=FALSE)
keyword.pair

keyword.pair <- filter(keyword.pair, n >= 1000)
library(igraph)
keyword.pair.g <- graph_from_data_frame(keyword.pair)
keyword.pair.g

# [그림 7-4]
library(ggraph)
set.seed(12)
windows(width=7.0, height=5.5)
ggraph(keyword.pair.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=n, edge_width=n), edge_colour="royalblue") +
  geom_node_point(shape=21, color="gray", fill="orange", size=4, stroke=1) +
  geom_node_text(aes(label=name), repel=TRUE, 
                 point.padding=unit(0.2, "lines"), size=3) +
  labs(title="NASA Metadata",
       subtitle="Keyword cooccurrence network",
       caption="Source: NASA") +
  theme_void() +
  theme(plot.title=element_text(face="bold"),
        legend.title=element_blank())

keyword.cor <- nasa.keyword %>%
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(item=keyword, feature=id, sort=TRUE, upper=FALSE)
keyword.cor

# [그림 7-5]
set.seed(12)
windows(width=7.0, height=5.5)
keyword.cor %>%
  filter(correlation >= 0.75) %>%
  graph_from_data_frame() %>%
  ggraph(layout="kk") +
  geom_edge_arc(aes(edge_alpha=correlation, edge_width=correlation), 
                edge_colour="olivedrab", strength=0.2) +
  geom_node_point(shape=21, color="black", fill="maroon", size=4, stroke=1) +
  geom_node_text(aes(label=name), repel=TRUE, max.overlaps=Inf,
                 point.padding=unit(0.2, "lines"), size=3) +
  labs(title="NASA Metadata",
       subtitle="Keyword correlation network",
       caption="Source: NASA") +
  theme_void() +
  theme(plot.title=element_text(face="bold"),
        legend.title=element_blank())

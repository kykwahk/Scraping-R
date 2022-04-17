
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

## 레미제라블 @GitHub

library(jsonlite)
url <- "https://gist.githubusercontent.com/mbostock/4062045/raw/5916d145c8c048a6e3086915a6be464467391c62/miserables.json"
lesmiser <- fromJSON(url)

str(lesmiser)
head(lesmiser$nodes)
head(lesmiser$links)

download.file(url, "github-lesmiser.json")

library(igraph)
lesmiser.g <- graph_from_data_frame(lesmiser$links)
lesmiser.g

# [그림 7-3]
library(ggraph)
set.seed(12)
windows(width=8.0, height=6.5)
ggraph(lesmiser.g, layout="fr") +
  geom_edge_link(aes(edge_alpha=value, edge_width=value), edge_colour="darkred") +
  geom_node_point(shape=21, color="gray", fill="cyan4", size=3, stroke=1) +
  geom_node_text(aes(label=name), repel=TRUE, 
                 point.padding=unit(0.2, "lines"), size=3) +
  labs(title="Les Miserables",
       subtitle="Character cooccurrence network",
       caption="Source: GitHub") +
  theme_void() +
  theme(plot.title=element_text(face="bold"),
        legend.title=element_blank())

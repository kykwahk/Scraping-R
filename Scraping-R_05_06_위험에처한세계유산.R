
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

## 위험에 처한 세계유산 @위키피디아/UNESCO

library(tidyverse)
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"
read_html(url) %>%
  html_elements("table")

Sys.setlocale("LC_ALL", "English")
read_html(url) %>%
  html_table() %>% 
  .[[2]] 
Sys.setlocale()

Sys.setlocale("LC_ALL", "English")
wiki.heritage <- read_html(url) %>%
  html_table() %>% 
  .[[2]] %>% 
  select(c(1,3,4,6,7)) %>% 
  rename_with(.fn=function(x) 
    c("name", "location", "criteria", "year_ins", "year_end"))

wiki.heritage
Sys.setlocale()

save(wiki.heritage, file="wiki-heritage.rda")
load("wiki-heritage.rda")

head(wiki.heritage$name, 3)

head(wiki.heritage$criteria, 3)
wiki.heritage$criteria <- ifelse(str_detect(wiki.heritage$criteria, "Cultural"), 
                                 "cultural", "natural") %>% 
  factor(.)
head(wiki.heritage$criteria, 3)
table(wiki.heritage$criteria)

head(wiki.heritage$year_ins, 3); head(wiki.heritage$year_end, 3)
wiki.heritage$year_end <- wiki.heritage$year_end %>% 
  parse_number()
head(wiki.heritage$year_ins, 3); head(wiki.heritage$year_end, 3)

head(wiki.heritage$location, 3)

pattern.lat <- "\\/ \\-?\\d*\\.?\\d*\\;"
pattern.lon <- "\\; \\-?\\d*\\.?\\d*"

str_extract(wiki.heritage$location, pattern.lat)[1:3]
str_extract(wiki.heritage$location, pattern.lon)[1:3]

str_extract(wiki.heritage$location, pattern.lat)[1:3] %>% 
  parse_number()
str_extract(wiki.heritage$location, pattern.lon)[1:3] %>% 
  parse_number()

wiki.heritage$lat <- str_extract(wiki.heritage$location, pattern.lat) %>% 
  parse_number()
wiki.heritage$lon <- str_extract(wiki.heritage$location, pattern.lon) %>% 
  parse_number()

wiki.heritage

# [그림 5-25]
library(ggrepel)
map.data <- map_data(map="world")
windows(width=12.0, height=7.5)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="slategray1", color="dimgray", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(xlim=c(-180, 180), ylim=c(-55, 90), ratio=1.3) +
  geom_point(data=wiki.heritage, 
             aes(x=lon, y=lat, shape=criteria, color=criteria, fill=criteria), 
             alpha=0.8, size=3.5, stroke=1.0) +
  geom_text_repel(data=slice_max(wiki.heritage, order_by=year_end, 
                                 n=10, with_ties=FALSE), 
                  aes(x=lon, y=lat, label=str_wrap(name, 25)), 
                  color="brown", fontface="bold",
                  arrow=arrow(length=unit(0.2, "cm")),
                  box.padding=2.0, max.overlaps=Inf) +
  scale_shape_manual(labels=c("Cultural", "Natural"), values=c(21, 22)) +
  scale_color_manual(labels=c("Cultural", "Natural"), 
                     values=c("dimgray", "dimgray")) +
  scale_fill_manual(labels=c("Cultural", "Natural"),
                    values=c("darkorange", "forestgreen")) +
  labs(title="World Heritage in Danger",
       subtitle="Cultural and natural sites (with names of recently listed 10 sites)",
       caption="Source: Wikipedia (UNESCO)") +
  theme_void() +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_blank(), 
        legend.text=element_text(size=10))

table(wiki.heritage$year_end - wiki.heritage$year_ins)

years.to.endanger <- as.data.frame(table(wiki.heritage$year_end - wiki.heritage$year_ins))
str(years.to.endanger)

# [그림 5-26]
windows(width=7.0, height=5.5)
ggplot(years.to.endanger, aes(x=Var1, y=Freq)) +
  geom_segment(aes(x=Var1, xend=Var1, y=0, yend=Freq), 
               color=ifelse(years.to.endanger$Var1 %in% c("0"), "orange", "gray"), 
               size=ifelse(years.to.endanger$Var1 %in% c("0"), 2.5, 1.5)) +
  geom_point(color=ifelse(years.to.endanger$Var1 %in% c("0"), "darkorange", "orange"), 
             size=ifelse(years.to.endanger$Var1 %in% c("0"), 6, 3)) +
  labs(x="Years", y="Frequency", 
       title="World Heritage in Danger",
       subtitle="Frequency of years taking to become an endangered site",
       caption="Source: Wikipedia (UNESCO)") +
  scale_y_continuous(breaks=seq(2, max(years.to.endanger$Freq), by=2)) +
  theme_light() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank())

t(table(wiki.heritage$year_end - wiki.heritage$year_ins, wiki.heritage$criteria))

# [그림 5-27]
library(ggridges)
windows(width=7.0, height=5.0)
ggplot(wiki.heritage, aes(x=year_end-year_ins, y=criteria, fill=criteria)) +
  geom_density_ridges2(stat="binline") +
  scale_y_discrete(labels=c("Cultural", "Natural")) +
  scale_fill_manual(values=c("darkorange", "forestgreen")) +
  labs(x="Years", y=NULL, 
       title="World Heritage in Danger",
       subtitle="Distribution of years taking to become an endangered site",
       caption="Source: Wikipedia (UNESCO)") +
  theme_ridges() +
  theme(legend.position="none")

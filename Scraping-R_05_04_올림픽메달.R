
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

## 올림픽 메달 @위키피디아

library(tidyverse)
library(rvest)
url <- "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"
read_html(url) %>%
  html_elements("table")

read_html(url) %>%
  html_table() %>% 
  .[[3]] 

wiki.olympic <- read_html(url) %>%
  html_table() %>% 
  .[[3]] %>% 
  slice(-n()) %>% 
  mutate(Rank=as.numeric(Rank))
wiki.olympic

save(wiki.olympic, file="wiki-olympic.rda")
load("wiki-olympic.rda")

lapply(wiki.olympic[c("Gold", "Silver", "Bronze")], summary)

wiki.olympic.long <- wiki.olympic %>% 
  pivot_longer(cols=Gold:Bronze, names_to="Medal", values_to="Count") %>% 
  mutate(Medal=factor(Medal, levels=c("Gold", "Silver", "Bronze")))
wiki.olympic.long

# [그림 5-20] 
windows(width=7.0, height=5.5)
ggplot(filter(wiki.olympic.long, Rank <= 10), 
       aes(x=reorder(NOC, -Rank), y=Count, fill=Medal)) +
  geom_bar(position=position_dodge(width=-0.9), color="gray50", stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=-0.9), size=3, 
            fontface="bold", color="dimgray", hjust=-0.25) +
  coord_flip() +
  scale_fill_manual(values=c("gold", "gray", "goldenrod4")) +
  labs(x=NULL, y="Number of Medals", 
       title="2016 Summer Olympics",
       subtitle="Top 10 countries",
       caption="Source: Wikipedia") +
  theme_light() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank())

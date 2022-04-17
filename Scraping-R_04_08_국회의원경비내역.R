
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

## 국회의원 경비 내역 @BBC

library(xml2)
library(XML)
url <- "http://news.bbc.co.uk/2/hi/uk_politics/8044207.stm"
html <- read_html(url)
html.parsed <- htmlParse(html)
html.tab <- readHTMLTable(html.parsed) 
str(html.tab)
length(html.tab)

head(html.tab[[1]])
tail(html.tab[[1]])

head(html.tab[[5]], 3)[,c(1:2, 14)]
tail(html.tab[[5]], 3)[,c(1:2, 14)]

first <- "Abbott, Ms Diane"
last <- "157,841"
for (i in 1:length(html.tab)) {
  lastrow <- nrow(html.tab[[i]])
  lastcol <- ncol(html.tab[[i]])
  if (!is.null(lastrow)) {
    if (as.character(html.tab[[i]][1,1])==first & 
        as.character(html.tab[[i]][lastrow,lastcol])==last) 
    {tabi <- i}
  }
}
tabi

head(html.tab[[tabi]], 3)[,c(1:2, 14)]
tail(html.tab[[tabi]], 3)[,c(1:2, 14)]
bbc.expense <- html.tab[[tabi]]
head(bbc.expense, 3)[,c(1:2, 14)]

save(bbc.expense, file="bbc-expense.rda")
load("bbc-expense.rda")

library(tidyverse)
str(bbc.expense)
bbc.expense <- bbc.expense %>% 
  mutate(across(.cols=4:ncol(.), .fns=parse_number)) %>% 
  mutate(Party=as.factor(Party)) 
str(bbc.expense)

summary(bbc.expense$Total)

# [그림 4-54]
windows(width=7.0, height=5.5)
ggplot(filter(bbc.expense, Party %in% c("CON", "LAB", "LD")), 
       aes(x=Party, y=Total)) +
  geom_boxplot(fill="coral3", color="black", notch=TRUE) +
  geom_point(position="jitter", color="blue", alpha=0.5) +
  geom_rug(sides="l", color="black") +
  scale_y_continuous(breaks=c(0, 60000, 90000, 120000, 150000, 180000),
                     labels=c("0", "60,000", "90,000", 
                              "120,000", "150,000", "180,000")) +
  labs(x="Party", y="Total Expenses (pounds)",
       title="MPs' Expenses in UK",
       subtitle="Distribution of total expenses of top 3 parties",
       caption="Source: BBC") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"))

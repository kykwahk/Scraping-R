
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

##########################
## 제2장 파일 다운로드  ##
##########################

##############
## 2.2 사례 ##
##############

## 원자력 발전소 @NRC  

library(httr)
url <- "https://www.nrc.gov/reading-rm/doc-collections/event-status/reactor-status/PowerReactorStatusForLast365Days.txt"
get <- GET(url)
txt <- content(get, as="text")
txt <- as(get, Class="character")
cat(txt)

save(txt, file="nrc-reactor.rda")
load("nrc-reactor.rda")

library(readr)
npower <- read_delim(txt, delim="|")
head(npower, 3); tail(npower, 3)
npower$ReportDt <- as.Date(npower$ReportDt, format="%m/%d/%Y")
head(npower, 3); tail(npower, 3)
dim(npower)

?strptime

# [그림 2-2]
some <- npower[npower$Unit %in% unique(npower$Unit)[1:12],]
some
library(ggplot2)
Sys.setlocale("LC_TIME", "English")
windows(width=7.0, height=7.0)
ggplot(some, aes(x=ReportDt, y=Power)) +
  geom_line(col="red") +
  facet_wrap(~ Unit, scale="free_y", nrow=4) +
  scale_x_date(date_labels="%y-%b", date_breaks="3 months") +
  labs(x="", y="Power",
       title="Nuclear Power Reactor Status over Time",
       subtitle="12 samples of power reactor in US",
       caption="Source: Nuclear Regulatory Commission") +
  theme_bw() +
  theme(plot.title=element_text(face="bold"),
        strip.background=element_rect(fill="wheat"),
        axis.text=element_text(size=8, face="bold"),
        legend.title=element_blank()) 
Sys.setlocale()

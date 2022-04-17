
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

## 경제통계 @미국 노동통계국

library(tidyverse)
library(httr)
library(XML)
url <- "https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0"
html <- GET(url)
html.parsed <- htmlParse(html)
forms <- xpathSApply(html.parsed, "//form", xmlAttrs) %>% 
  map_dfr(bind_rows)
forms

option <- xpathSApply(html.parsed, "//select", xmlAttrs)
as.data.frame(t(option))

result <- POST(url, body=list(startYear="2020", endYear="2021"), encode="form")

result.parsed <- htmlParse(result)
CPI <- xpathSApply(result.parsed, "//table[@id='seriesDataTable1']//td", 
                   xmlValue, trim=TRUE, recursive=FALSE)
CPI

library(lubridate)
bls.CPI <- tibble(year=as.numeric(CPI[seq(1, length(CPI), by=4)]), 
                  period=CPI[seq(2, length(CPI), by=4)],
                  label=ym(CPI[seq(3, length(CPI), by=4)]),
                  value=as.numeric(CPI[seq(4, length(CPI), by=4)]))
bls.CPI
str(bls.CPI)

save(bls.CPI, file="bls-CPI.rda")
load("bls-CPI.rda")

getEconStats <- function(baseurl=NULL, startYear=NULL, endYear=NULL) {
  library(httr)
  library(XML)
  library(tibble)
  library(lubridate)
  if (is.null(baseurl)) {
    url <- "https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0"
  }
  if (is.null(startYear) | is.null(endYear)) {
    startYear <- as.character(year(today()) - 2)
    endYear <- as.character(year(today()))
  }
  result <- POST(url, body=list(startYear=startYear, endYear=endYear),
                 encode="form")
  result.parsed <- htmlParse(result)
  stat <- xpathSApply(result.parsed, "//table[@id='seriesDataTable1']//td", 
                      xmlValue, trim=TRUE, recursive=FALSE)
  stat <- tibble(year=as.numeric(stat[seq(1, length(stat), by=4)]), 
                 period=stat[seq(2, length(stat), by=4)],
                 label=ym(stat[seq(3, length(stat), by=4)]),
                 value=as.numeric(stat[seq(4, length(stat), by=4)]))
  return(stat)
}
bls <- getEconStats()
bls

url <- "https://beta.bls.gov/dataViewer/view/timeseries/CUUR0000SA0"
bls.CPI <- getEconStats(baseurl=url, startYear="2020", endYear="2021")
bls.CPI

url <- "https://beta.bls.gov/dataViewer/view/timeseries/LNS14000000"
bls.unemployment <- getEconStats(baseurl=url, startYear="2020", endYear="2021")
bls.unemployment

save(bls.unemployment, file="bls-unemployment.rda")
load("bls-unemployment.rda")

# [그림 4-60]
windows(width=10.5, height=6.0)
Sys.setlocale("LC_ALL", "English")
ggplot(bls.CPI, aes(x=label, y=value)) + 
  geom_line(color="gray", lwd=1) +
  geom_point(shape=21, color="gray", fill="blue", size=5, stroke=4, alpha=0.7) +
  geom_text(label=sprintf("%.1f", bls.CPI$value), 
            size=3, vjust=-1.1, color="royalblue") +
  scale_x_date(date_labels="%b\n%Y", date_breaks="2 months") +
  labs(x="", y=NULL,
       title="CPI(Consumer Price Index)",
       subtitle="U.S. CPI over time: base year 1982-84=100",
       caption="Source: U.S. Bureau of Labor Statistics") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(size=8.5, face="bold"),
        panel.grid.minor=element_blank())
Sys.setlocale()

# [그림 4-61]
library(scales)
windows(width=10.5, height=6.0)
Sys.setlocale("LC_ALL", "English")
ggplot(bls.unemployment, aes(x=label, y=value/100)) + 
  geom_line(color="gray", lwd=1) +
  geom_point(shape=21, color="gray", fill="red", size=5, stroke=4, alpha=0.7) +
  geom_text(label=sprintf("%.1f%%", bls.unemployment$value), 
            size=3, vjust=-1.1, color="firebrick") +
  scale_x_date(date_labels="%b\n%Y", date_breaks="2 months") +
  scale_y_continuous(labels=percent) +
  labs(x="", y=NULL,
       title="Unemployment Rate",
       subtitle="US unemployment rate over time",
       caption="Source: U.S. Bureau of Labor Statistics") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(size=8.5, face="bold"),
        panel.grid.minor=element_blank())
Sys.setlocale()

cor(bls.CPI$value, bls.unemployment$value)

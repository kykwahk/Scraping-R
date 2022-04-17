
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

## 특일 정보 @공공데이터포털/천문연구원

# 공휴일
url <- "http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?solYear=%s&solMonth=%s&ServiceKey=%s"
year <- "2023"
month <- "05"
key <- "abcxyz123456789"
sprintf(url, year, month, key)

library(XML)
library(xml2)
xml <- read_xml(sprintf(url, year, month, key))
xml.parsed <- xmlParse(xml)
holiday <- xpathSApply(xml.parsed, "//dateName", xmlValue)
date <- xpathSApply(xml.parsed, "//locdate", xmlValue)
holiday; date

findHolidays <- function(year, url, key) {
  library(XML)
  library(xml2)
  library(tidyverse)
  library(lubridate)
  holidays <- tibble()
  for(m in 1:12) {
    holiday <- c()
    date <- c()
    month <- str_pad(m, 2, pad=0)
    xml <- read_xml(sprintf(url, year, month, key))
    xml.parsed <- xmlParse(xml)
    holiday <- xpathSApply(xml.parsed, "//dateName", xmlValue)
    date <- xpathSApply(xml.parsed, "//locdate", xmlValue)
    if (length(holiday) == 0) next
    holidays.month <- tibble(holiday=holiday, date=date)
    holidays <- bind_rows(holidays, holidays.month)
  }
  holidays <- mutate(holidays, date=ymd(date))
  return(holidays)
}

holidays <- findHolidays(year=2023, url=url, key=key)
holidays

save(holidays, file="dataportal-holidays.rda")
load("dataportal-holidays.rda")

url <- "http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?solYear=%s&solMonth=%s&ServiceKey=%s"
year <- "2023"
month <- "05"
key <- "abcxyz123456789"
xml <- read_xml(sprintf(url, year, month, key))
data <- as_list(xml)
str(data)
data$response$body$items[[1]]

displayHolidays <- function(year, url, key) {
  library(xml2)
  library(stringr)
  library(lubridate)
  for(m in 1:12){
    month <- str_pad(m, 2, pad=0)
    xml <- read_xml(sprintf(url, year, month, key))
    data <- as_list(xml)
    items <- data$response$body$items
    for(item in items) {
      print(str_c(item$dateName, ymd(item$locdate), sep=": "))
    }
  }
}
displayHolidays(year=2023, url=url, key=key)

library(calendR)
library(lubridate)
myfills <- rep(NA, 365)
myfills[yday(holidays$date)] <- "Holidays"

# [그림 8-8]
Sys.setlocale("LC_ALL", "English")
windows(width=7.0, height=10.0)
calendR(year=2023, lty=0, mbg.col="orange", months.col="white", 
        orientation="portrait", special.days=myfills, special.col="salmon",
        legend.pos="bottom")
Sys.setlocale()

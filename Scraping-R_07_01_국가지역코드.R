
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

## 국가/지역 코드 @UN

library(jsonlite)
url <- "http://comtrade.un.org/data/cache/partnerAreas.json"
page <- readLines(url)
areacodes <- fromJSON(page)

areacodes <- fromJSON(url)
str(areacodes)
head(areacodes$results)

download.file(url, "un-partnerAreas.json")
areacodes <- fromJSON("un-partnerAreas.json")
head(areacodes$results)

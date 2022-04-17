
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

###########################
## 제3장 텍스트 패턴매칭 ##
###########################

##############
## 3.4 사례 ##
##############

## 국가코드 @UN

library(RCurl)
url <- "https://unstats.un.org/wiki/display/comtrade/Country+Code"
html <- getURL(url)
html
class(html)

save(html, file="un-countrycode.rda")
load("un-countrycode.rda")

html <- paste(readLines(url), collapse=" ")
class(html)

library(httr)
html.obj <- GET(url)
class(html.obj)
html <- content(html.obj, type="text")
html <- as(html.obj, "character")
class(html)

library(xml2)
html.obj <- read_html(url)
class(html.obj)
html <- as(html.obj, "character")
class(html)

library(curl)
html.obj <- curl(url)
class(html.obj)
html <- readLines(html.obj)
class(html)

pattern <- "[A-Z]{3}\\s([[:alpha:],.'()-]+\\s?){1,}"
library(stringr)
country.code <- unlist(str_extract_all(html, pattern))
head(country.code, 8)
country.code <- country.code[-c(1:6)]
head(country.code)

code <- str_sub(country.code, start=1, end=3)
country <- str_sub(country.code, start=5)
iso3code <- data.frame(code=code, country=country)
dim(iso3code)
head(iso3code, 3); tail(iso3code, 3)

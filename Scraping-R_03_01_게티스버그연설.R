
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

## 게티스버그 연설 @에이브러햄 링컨 온라인

library(RCurl)
url <- "http://www.abrahamlincolnonline.org/lincoln/speeches/gettysburg.htm"
html <- getURL(url)
html
class(html)

save(html, file="lincoln-gettysburg.rda")
load("lincoln-gettysburg.rda")

pattern <- "<.*?>"
text <- gsub(pattern, "", html)
text

#/
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
text <- gsub(pattern, "", html)
text
#/

text <- gsub("\\s{1,}", " ", text)
text <- trimws(text)
text

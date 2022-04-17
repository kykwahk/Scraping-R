
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

## 가독성 테스트 @웹FX

library(tidyverse)
library(httr)
library(XML)
url <- "https://www.webfx.com/tools/read-able/"
html <- GET(url)
html.parsed <- htmlParse(html)

xpathSApply(html.parsed, "//div[@id='enter-text-tab']/form", 
            xmlAttrs, simplify=FALSE)
forms <- xpathSApply(html.parsed, "//div[@id='enter-text-tab']/form", 
                     xmlAttrs, simplify=FALSE) %>% 
  bind_rows()
forms

xpathSApply(html.parsed, 
            "//div[@id='enter-text-tab']/form//textarea", xmlAttrs, simplify=FALSE)
form.textarea <- xpathSApply(html.parsed, 
                             "//div[@id='enter-text-tab']/form//textarea", xmlAttrs) %>% 
  bind_rows()
form.textarea

text <- "The rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well."

result <- POST(url=paste0(url, "check.php"), body=list(directInput=text), 
               encode="form")

result.parsed <- htmlParse(result)
indicator <- xpathSApply(result.parsed, 
                         "//div[@class='generator-result-card']//h5[@class='card-title']", 
                         xmlValue, recursive=FALSE, trim=TRUE)
score <- xpathSApply(result.parsed, "//p[@class='card-percent']", xmlValue) %>% 
  as.numeric()
readability <- tibble(indicator, score)
readability

getReadability <- function (text=NULL) {
  if (is.null(text)) {
    return("text 인수에 텍스트를 지정하세요.")
    break
  }
  library(httr)
  library(XML)
  url <- "https://www.webfx.com/tools/read-able/"
  result <- POST(url=paste0(url, "check.php"), body=list(directInput=text), 
                 encode="form")
  result.parsed <- htmlParse(result)
  indicator <- xpathSApply(result.parsed, 
                           "//div[@class='generator-result-card']//h5[@class='card-title']", 
                           xmlValue, recursive=FALSE, trim=TRUE)
  score <- xpathSApply(result.parsed, "//p[@class='card-percent']", xmlValue) %>% 
    as.numeric()
  readability <- tibble(indicator, score)
  return(readability)
}
getReadability()
text <- "You come at four in the afternoon, then at three I shall begin to be happy."
getReadability(text=text)


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

## 동의어 사전 @워드넷

library(httr)
library(XML)
url <- "http://wordnetweb.princeton.edu/perl/webwn"
html <- GET(url)
html.parsed <- htmlParse(html)
forms <- xpathSApply(html.parsed, "//form", xmlAttrs)
as.data.frame(t(forms))

form.input <- xpathSApply(html.parsed, "//form[1]/input", xmlAttrs)
as.data.frame(t(form.input))

result <- GET(url, query=list(s="data"))

result.parsed <- htmlParse(result)
xpathSApply(result.parsed, "//li", xmlValue)

url <- "http://wordnetweb.princeton.edu/perl/webwn?s=data"
html <- GET(url)
html.parsed <- htmlParse(html)
xpathSApply(html.parsed, "//li", xmlValue)

getWordNet <- function(word=NULL) {
  if (is.null(word)) {
    return("word 인수에 검색할 단어를 지정하세요.")
    break
  }
  library(httr)
  library(XML)
  url <- "http://wordnetweb.princeton.edu/perl/webwn"
  result <- GET(url, query=list(s=word))
  result.parsed <- htmlParse(result)
  out <- xpathSApply(result.parsed, "//li", xmlValue)
  return(cat(paste0(out, collapse="\n")))
}
getWordNet()
getWordNet(word="science")

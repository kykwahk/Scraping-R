
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

## 노벨상 소개 @노벨재단

library(httr)
url <- "https://www.nobelprize.org/prizes"
html <- GET(url)

library(xml2)
html <- read_html(url)

library(XML)
html.parsed <- htmlParse(html)

# 제목
xpathSApply(doc=html.parsed, path="//article[@class='content']//h4", 
            fun=xmlValue)
header <- xpathSApply(doc=html.parsed, path="//article[@class='content']//h4", 
                      fun=xmlValue, trim=TRUE)
header

# 내용
content <- xpathSApply(doc=html.parsed, path="//article[@class='content']//p", 
                       fun=xmlValue)
content
content <- paste(content[-length(content)], collapse=" ")
content

library(stringr)
content <- xpathSApply(doc=html.parsed, 
                       path="//article[@class='content']//p[position()!=last()]", 
                       fun=xmlValue) %>% 
  str_c(collapse=" ")
content

# 주요 통계
facts <- xpathSApply(doc=html.parsed, path="//ul[@class='factlist wp-block-list']/li", 
                     fun=xmlValue)
facts

library(tidyr)
separate_wider_delim(tibble(facts=facts), cols=facts, delim=": ", 
                     names=c("category", "statistics"))

facts <- xpathSApply(doc=html.parsed, path="//ul[@class='factlist wp-block-list']/li", 
                     fun=xmlValue) %>% 
  tibble(facts=.) %>% 
  separate_wider_delim(cols=facts, delim=": ", 
                       names=c("category", "statistics"))
facts

# 메달 이미지
medal <- xpathSApply(html.parsed, 
                     path="//div[@class='nobel__blocks--content']/section[1]/div[1]", 
                     fun=xmlGetAttr, "data-bgset")
medal

medal <- unlist(strsplit(x=medal, split=" \\| "))[3]
medal

medal <- gsub(pattern="(.*) \\[.*\\]", replacement="\\1", x=medal)
medal

library(stringr)
medal <- xpathSApply(html.parsed, 
                     path="//div[@class='nobel__blocks--content']/section[1]/div[1]", 
                     fun=xmlGetAttr, "data-bgset") %>% 
  str_split(pattern=" \\| ") %>% 
  unlist() %>% 
  .[3] %>% 
  str_replace_all(pattern="(.*) \\[.*\\]", replacement="\\1")
medal

download.file(medal, "medal.jpg", mode="wb")
file.exists("medal.jpg")

library(httr)
GET(medal, write_disk("medal.jpg", overwrite=TRUE))
file.exists("medal.jpg")

library(pander)
openFileInOS("medal.jpg")

library(imager)
windows(width=7.0, height=3.0)
par(mar=c(1,1,1,1))
plot(load.image("medal.jpg"), axes=FALSE)


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

## 이미지 포맷 변환 @픽스픽처

library(tidyverse)
library(httr)
library(XML)
url <- "http://content.time.com/time/covers/0,16641,19830103,00.html"
html <- GET(url)
html.parsed <- htmlParse(html)
timecover <-xpathSApply(html.parsed, "//img[@title='Original Cover is a gatefold']",
                        xmlGetAttr, "src")
download.file(timecover, "timecover.jpg", mode="wb")

# [그림 4-58]
library(imager)
windows(width=5.0, height=6.0)
par(mar=c(1,1,1,1))
plot(load.image("timecover.jpg"), axes=FALSE)

url <- "https://www.fixpicture.org/"
html <- GET(url)
html.parsed <- htmlParse(html)

forms <- xpathSApply(html.parsed, "//form", xmlAttrs)
as.data.frame(t(forms))

inputs <- xpathSApply(html.parsed, "//input", xmlAttrs)
inputs

inputs <- map_dfr(inputs, bind_rows)
str(inputs)
inputs[, c("type", "name", "class", "value")]

xpathSApply(html.parsed, "//select/option", xmlValue)

option <- xpathSApply(html.parsed, "//select", xmlAttrs)
as.data.frame(t(option))

install.packages("curl") # POST() 함수로부터 연결 에러 발생 시 새로 설치

result <- POST(url=paste0(url, "resize.php?LANG={LANG}"), 
               body=list(image=upload_file("timecover.jpg"), format="png"),
               encode="multipart")

result.parsed <- htmlParse(result)
xpathSApply(result.parsed, "//a/@href", as.character)
result.link <- paste0(url, xpathSApply(result.parsed, "//a/@href", as.character))
result.link
download.file(result.link, "timecover.png", mode="wb")
list.files(pattern="timecover")

library(pander)
openFileInOS("timecover.png")

getImage <- function(img=NULL, fmt="png") {
  if (is.null(img)) {
    img <- file.choose()
  }
  library(httr)
  library(XML)
  library(tools)
  url <- "https://www.fixpicture.org/"
  html <- GET(url)
  html.parsed <- htmlParse(html)
  result <- POST(url=paste0(url, "resize.php?LANG={LANG}"), 
                 body=list(image=upload_file(img), format=fmt),
                 encode="multipart")
  result.parsed <- htmlParse(result)
  result.link <- paste0(url, xpathSApply(result.parsed, "//a/@href", as.character))
  download.file(result.link, paste0(file_path_sans_ext(img), ".", fmt), mode="wb")
}
getImage(img="timecover.png", fmt="gif")
getImage(fmt="gif")
list.files(pattern="timecover")

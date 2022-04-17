
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

####################
## 제6장 셀레니움 ##
####################

#######################
## 6.1 셀레니움 개요 ##
#######################

library(tidyverse)
library(rvest)
url <- "https://www.ssa.gov/oact/babynames/decades/names2010s.html"
babyname <- read_html(url) %>%
  html_table() %>% 
  .[[1]] %>% 
  slice(-c(1:2, n())) %>% 
  rename(rank=X1, male.name=X2, Male.number=X3, female.name=X4, female.number=X5)
babyname

url.base <- "https://www.ssa.gov/oact/babynames/decades/"
decades <- str_c(seq(2010, 1880, -10), "s")
babynames <- vector("list", length(decades))
for (i in seq_along(babynames)) {
  url <- str_c(url.base, "names", decades[i], ".html")
  babynames[[i]] <- read_html(url) %>%
    html_table() %>% 
    .[[1]] %>% 
    slice(-c(1:2, n())) %>% 
    rename(rank=X1, male.name=X2, male.number=X3, 
           female.name=X4, female.number=X5) %>% 
    mutate(decade=decades[i]) %>% 
    mutate(across(.cols=ends_with("number"), .fns=parse_number)) %>% 
    select(decade, everything())
  Sys.sleep(sample(10, 1)*0.1)
}
babynames
babynames <- reduce(babynames, bind_rows)
babynames

############################
## 6.2 셀레니움 환경 구축 ##
############################

## 자바 바이너리

install.packages("RSelenium")
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.naver.com")
remDr$getTitle()
remDr$getCurrentUrl()

## rsDriver()

library(RSelenium)
rsDriver()
binman::list_versions("chromedriver")

rD <- rsDriver()
rD <- rsDriver(chromever="98.0.4758.80")
remDr <- rD$client
remDr$navigate("https://www.naver.com")
remDr$getTitle()
remDr$getCurrentUrl()
remDr$close()
rD$server$stop()

# Java instance kill: 모든 포트 free화
pingr::ping_port("localhost", 4567)
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
pingr::ping_port("localhost", 4567)

## 도커

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome", version="98.0.4758.80")
remDr$open()
remDr$navigate("https://www.naver.com")
remDr$getTitle()
remDr$getCurrentUrl()
remDr$getTitle()
remDr$screenshot(display=TRUE)

##########################
## 6.3 RSelenium 패키지 ##
##########################

install.packages("RSelenium")
install.packages("devtools")
devtools::install_github("ropensci/RSelenium")

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()

remDr$navigate("https://www.naver.com/")
search <- remDr$findElement(using="xpath", value="//input[@id='query']") 
search$sendKeysToElement(list("인공지능")) 
button <- remDr$findElement(using="xpath", value="//button[@id='search_btn']")
button$sendKeysToElement(list(key="enter"))

## 시작

library(RSelenium)
remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr <- remoteDriver(port=4445, browserName="chrome")
class(remDr)

?remoteDriver

remDr$open()

remDr$getStatus()

## 이동

remDr$navigate("https://www.google.com/")

remDr$navigate("https://www.naver.com/")
remDr$getCurrentUrl()

remDr$goBack()
remDr$getCurrentUrl()
remDr$goForward()
remDr$getCurrentUrl()

remDr$refresh()

## 요소 식별

remDr$navigate("https://www.google.com/")
elem <- remDr$findElement(using="name", value="q")
class(elem)

?webElement

elem$highlightElement()
elem$getElementAttribute("name")

elem <- remDr$findElement(using="class name", value="gLFyf")
elem$getElementAttribute("type")

elem <- remDr$findElement(using="class name", value="gLFyf gsfi")

elem <- remDr$findElement(using="css selector", value="input[name='q']")
elem <- remDr$findElement(using="css selector", value="[name='q']")

elem <- remDr$findElement(using="css selector", value="[class='gLFyf gsfi']")

elem <- remDr$findElement(using="css selector", value="input.gLFyf.gsfi")
elem <- remDr$findElement(using="css selector", value=".gLFyf.gsfi")

elem <- remDr$findElement(using="xpath", value="//input[@name='q']")
elem <- remDr$findElement(using="xpath", value="//input[@class='gLFyf gsfi']")

## 요소 구동

remDr$navigate("https://www.google.com/")
elem <- remDr$findElement(using="xpath", value="//input[@name='q']")
elem$sendKeysToElement(list("R CRAN"))

elem$sendKeysToElement(list(key="enter"))

remDr$findElement(using="xpath", value="//input[@name='q']")$clearElement()
elem <- remDr$findElement(using="xpath", value="//input[@name='q']")
elem$sendKeysToElement(list("R CRAN", key="enter"))

elem <- remDr$findElement(using="xpath", value="//body")
elem$sendKeysToElement(list(key="page_down"))
elem$sendKeysToElement(list(key="page_up"))

elem$sendKeysToElement(list(key="end"))
elem$sendKeysToElement(list(key="home"))

elem$sendKeysToElement(list(key="control", "A"))
elem$clickElement()

names(selKeys)

remDr$navigate("https://www.google.com/")
elem <- remDr$findElement(using="xpath", value="//input[@name='q']")
elem$sendKeysToElement(list("R CRAN", key="enter"))

elems <- remDr$findElements(using="xpath", value="//h3[@class='LC20lb MBeuO DKV0Md']")
unlist(sapply(elems, function(x) x$getElementText()[[1]]))

elems[[1]]$getElementText()[[1]]

elems[[2]]$getElementText()[[1]]
remDr$executeScript("return arguments[0].innerHTML;", args=list(elems[[2]]))[[1]]

elems[[1]]$clickElement()
remDr$getCurrentUrl()
remDr$getTitle()

remDr$goBack()
elems <- remDr$findElements(using="xpath", value="//h3[@class='LC20lb MBeuO DKV0Md']")
remDr$mouseMoveToLocation(webElement=elems[[1]])
remDr$click()

## 자바스크립트 실행

remDr$navigate("https://www.google.com/")
elem <- remDr$findElement(using="css selector", value="img.lnXdpd")
elem$highlightElement()

script <- "return document.getElementsByClassName('lnXdpd')[0].hidden;"
remDr$executeScript(script, args=list(""))

script <- "document.getElementsByClassName('lnXdpd')[0].hidden=true; 
           return document.getElementsByClassName('lnXdpd')[0].hidden;"
remDr$executeScript(script, args=list(""))

script <- "document.getElementsByClassName('lnXdpd')[0].hidden=false; 
           return document.getElementsByClassName('lnXdpd')[0].hidden;"
remDr$executeScript(script, args=list(""))

elem <- remDr$findElement(using="css selector", value="img.lnXdpd")
script <- "arguments[0].hidden=true; return arguments[0].hidden;"
remDr$executeScript(script, args=list(elem))

script <- "arguments[0].hidden=false; return arguments[0].hidden;"
remDr$executeScript(script, args=list(elem))

## 프레임

remDr$navigate("https://CRAN.r-project.org")
library(XML)
htmlParse(remDr$getPageSource()[[1]])

elems <- remDr$findElements(using="tag name", value="frame")
length(elems)
for (i in seq_along(elems)) {
  elems[[i]]$highlightElement()
  Sys.sleep(1)
}

sapply(elems, function(x) x$getElementAttribute("src"))

remDr$switchToFrame(elems[[2]])
htmlParse(remDr$getPageSource()[[1]])

elems <- remDr$findElements(using="css selector", value="a[href]")
unlist(sapply(elems, function(x) x$getElementText()[[1]]))
unlist(sapply(elems, function(x) x$getElementAttribute("href")))

remDr$navigate("https://CRAN.r-project.org")
elems <- remDr$findElements(using="tag name", value="frame")

remDr$switchToFrame(elems[[3]])
htmlParse(remDr$getPageSource()[[1]])

elems <- remDr$findElements(using="partial link text", value="Download R")
sapply(elems, function(x) x$getElementText())

remDr$mouseMoveToLocation(webElement=elems[[3]])
remDr$click()
htmlParse(remDr$getPageSource()[[1]])

elem <- remDr$findElement(using="xpath", value="//a[@href='base/']")
elem$highlightElement()
elem$getElementAttribute("href")
elem$clickElement()

elem <- remDr$findElement(using="partial link text", value="Download R")
elem$highlightElement()
elem$getElementAttribute("href")
elem$clickElement()

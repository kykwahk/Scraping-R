
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

######################
## 제5장 CSS 선택자 ##
######################

##############
## 5.3 사례 ##
##############

## 동의어 사전 @워드넷

library(rvest)
url <- "http://wordnetweb.princeton.edu/perl/webwn"
page <- session(url)
page

html_elements(page, "form") %>%
  html_form()
form.unfilled <- html_elements(page, "form") %>%
  html_form() %>%
  .[[1]]
form.unfilled

word <- "data"
form.filled <- html_form_set(form.unfilled, s=word)
form.filled

session <- session_submit(page, form.filled)
session$url
session$response

result <- session %>%
  html_elements("li") %>%
  html_text()
result

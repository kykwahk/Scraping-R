
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

## 가독성 테스트 @웹FX

library(rvest)
url <- "https://www.webfx.com/tools/read-able/"
page <- session(url)
page

html_elements(page, "form") %>%
  html_form()
form.unfilled <- html_elements(page, "form") %>%
  html_form() %>%
  .[[4]]
form.unfilled

text <- "The rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well."
form.filled <- html_form_set(form.unfilled, directInput=text)
form.filled

session <- session_submit(page, form.filled)
str(session)
session$response

session %>%
  html_elements(".generator-result-card .card-title") %>%
  html_text()

library(tidyverse)
indicator <- session %>%
  html_elements(".generator-result-card .card-title") %>%
  html_text() %>% 
  str_extract(pattern="^[^\r]*") %>% 
  str_trim()
indicator

score <- session %>%
  html_elements(".card-percent") %>%
  html_text() %>% 
  as.numeric()
score

reliability <- tibble(indicator, score)
reliability

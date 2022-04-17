
##########################################
## RÀ» ÀÌ¿ëÇÑ À¥½ºÅ©·¹ÀÌÇÎ°ú µ¥ÀÌÅÍºÐ¼® ##
## (°û±â¿µ, µµ¼­ÃâÆÇ Ã»¶÷)              ## 
##########################################

#################
## Á¦4Àå XPath ##
#################

##############
## 4.5 »ç·Ê ##
##############

## ¿µÈ­ ¸®ºä @³×ÀÌ¹ö

library(tidyverse)
library(httr)
library(XML)
url <- "https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=143435&target=&page=1"
html <- GET(url)
html.parsed <- htmlParse(html)

# ÀÛ¼ºÀÚ
author <- xpathSApply(html.parsed, "//a[@class='author']", xmlValue)
author

# ÀÛ¼º ÀÏÀÚ
xpathSApply(html.parsed, "//td[@class='num']/text()", xmlValue)

xpathSApply(html.parsed, "//td[@class='num']/node()")
xpathSApply(html.parsed, "//td[@class='num']/node()[self::*]")
xpathSApply(html.parsed, "//td[@class='num']/node()[not(self::*)]")

xpathSApply(html.parsed, "//td[@class='num']/node()[not(self::*)]", xmlValue)

library(lubridate)
date <- xpathSApply(html.parsed, "//td[@class='num']/text()", xmlValue) %>% 
  ymd()
date

# ¿µÈ­ Á¦¸ñ
title <- xpathSApply(html.parsed, "//a[@class='movie color_b']", xmlValue)
title

# °¨»óÆò
xpathSApply(html.parsed, "//td[@class='title']", xmlValue)

review <- xpathSApply(html.parsed, "//td[@class='title']", 
                      xmlValue, recursive=FALSE) %>% 
  str_trim()
review

# ÆòÁ¡
rating <- xpathSApply(html.parsed, "//div[@class='list_netizen_score']/em", 
                      xmlValue) %>% 
  as.numeric()
rating

# ÃÑÆäÀÌÁö °³¼ö
total.pages <- xpathSApply(html.parsed, "//strong[@class='c_88 fs_11']", xmlValue)
total.pages <- ceiling(as.numeric(total.pages)/10)
total.pages

# ³×ÀÌ¹ö ¿µÈ­ ¸®ºä µ¥ÀÌÅÍ ÃßÃâ ÇÔ¼ö
naverMovieReview <- function (baseurl, n=NULL) {
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(XML)
  if (is.null(n)) {
    url <- str_c(baseurl, "1")
    html <- GET(url)
    html.parsed <- htmlParse(html)
    total.pages <- xpathSApply(html.parsed, 
                               "//strong[@class='c_88 fs_11']", xmlValue)
    total.pages <- ceiling(as.numeric(total.pages)/10)
    ifelse(total.pages > 1000, n <- 1000, n <- total.pages)
  }
  else 
    ifelse(n > 1000, n <- 1000, n <- n)
  navermov.review <- tibble()
  for (i in c(1:n)) {
    url <- str_c(baseurl, i)
    html <- GET(url)
    html.parsed <- htmlParse(html)
    author <- xpathSApply(html.parsed, "//a[@class='author']", xmlValue)
    date <- xpathSApply(html.parsed, "//td[@class='num']/text()", xmlValue) %>% 
      ymd()
    title <- xpathSApply(html.parsed, "//a[@class='movie color_b']", xmlValue) 
    review <- xpathSApply(html.parsed, "//td[@class='title']", 
                          xmlValue, recursive=FALSE) %>% 
      str_trim()
    rating <- xpathSApply(html.parsed, "//div[@class='list_netizen_score']/em", 
                          xmlValue) %>% 
      as.numeric()
    if (length(date) > 0 ) {
      navermov.r <- tibble(author=author, date=date, title=title, 
                           review=review, rating=rating)
      navermov.review <- bind_rows(navermov.review, navermov.r)
    } 
    else break
    Sys.sleep(sample(10, 1)*0.1)
  }
  navermov.review <- bind_cols(id=1:nrow(navermov.review), navermov.review)
  return(navermov.review)
}

baseurl <- "https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=143435&target=&page="
navermov.okja <- naverMovieReview(baseurl=baseurl, n=10)
navermov.okja

baseurl <- "https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=143435&target=&page="
navermov.okja <- naverMovieReview(baseurl=baseurl)
navermov.okja

save(navermov.okja, file="navermov-okja.rda")
load("navermov-okja.rda")

# ¼î»ýÅ© Å»Ãâ
baseurl <- "https://movie.naver.com/movie/point/af/list.naver?st=mcode&sword=17421&target=&page="
navermov.shawshank <- naverMovieReview(baseurl=baseurl)
navermov.shawshank

save(navermov.shawshank, file="navermov-shawshank.rda")
load("navermov-shawshank.rda")

# ½Ã°¢È­ ¹× ºÐ¼®
install.packages("KoNLP")
library(KoNLP)

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade="never", 
                        INSTALL_opts="--no-multiarch")
library(KoNLP)

navermov.review <- navermov.okja
okja.review <- navermov.review$review

okja.review5 <- okja.review[1:5]
okja.review5

library(tidyverse)
okja.review5 <- okja.review5 %>%
  str_replace_all("[¤¡-¤Ó]", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("[[:digit:]]", " ") %>%
  str_trim()
okja.review5

okja.review[1:5] %>%
  str_replace_all("[^°¡-ÆR ]", " ") %>%
  str_trim()

buildDictionary(ext_dic=c("sejong", "woorimalsam", "insighter"))

okja.words <- SimplePos09(okja.review5)
okja.words

okja.words <- str_match_all(okja.words, pattern="([°¡-ÆR]+)/[NPM]")
okja.words

okja.words <- lapply(okja.words, function(x) x[,2][str_length(x[,2]) >= 2])
okja.words

okja.words <- unlist(okja.words)
okja.words

sort(table(okja.words), decreasing=TRUE)

okja.words <- okja.review %>%
  str_replace_all("[¤¡-¤Ó]", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("[[:digit:]]", " ") %>%
  str_trim() %>%
  SimplePos09(autoSpacing=TRUE) %>%
  str_match_all(pattern="([°¡-ÆR]+)/[NPM]") %>%
  lapply(function(x) x[,2][str_length(x[,2]) >= 2]) %>%
  unlist()

sort(table(okja.words), decreasing=TRUE)[1:10]

# [±×¸² 4-41]
library(wordcloud2)
okja.wc <- 
  wordcloud2(data=table(okja.words[!okja.words %in% c("¿µÈ­", "¿ÁÀÚ", "ºÀÁØÈ£", "°¨µ¶")]), 
             minSize=1, color="random-light", backgroundColor="black")
okja.wc

library(webshot)
install_phantomjs()
library(htmlwidgets)
saveWidget(okja.wc, "okjawc.html", selfcontained=FALSE)
webshot(url="okjawc.html", file="okjawc.png", delay=10)

library(pander)
openFileInOS("okjawc.png")
openFileInOS("okjawc.html")

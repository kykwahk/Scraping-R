
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

######################
## 제5장 CSS 선택자 ##
######################

###################
## 5.1 노드 선택 ##
###################

html.string <- c(
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<title>Moive Quotes</title>',
  '</head>',
  '<body>',
  '<h1>Famous Quotes from Movies</h1>',
  '<div time="1h 39min" genre="drama" lang="english" date="December/16/1970">',
  '<h2>Ali MacGraw as Jennifer Cavilleri</h2>',
  '<p><i>Love means never having to say you\'re sorry.</i></p>',
  '<p><b>Movie: </b>Love Story</p>',
  '</div>',
  '<div time="2h 22min"  genre="comedy" date="June/23/1994">',
  '<h2>Tom Hanks as Forrest Gump</h2>',
  "<p><i>My mama always said, 'Life was like a box of chocolates; you never know what you\'re gonna get.'</i></p>",
  "<p><i>Mama says,'Stupid is as stupid does.'</i></p>",
  '<p><b>Movie: </b><a href="http://www.imdb.com/title/tt0109830/?ref_=nv_sr_1">Forrest Gump</a></p>',
  '</div>',
  '<p>',
  '<b>Sources:</b><br>',
  '<a href="http://www.afi.com/"><i>American Film Institute</i></a><br>',
  '<a href="http://www.hollywoodreporter.com/"><i>Hollywood Reporters</i></a>',
  '</p>',
  '</body>',
  '</html>'
)
writeLines(html.string, "moviequotes.html")
library(pander)
openFileInOS("moviequotes.html")

library(rvest)
html <- read_html(x="moviequotes.html") 
html
class(html)

node <- html_elements(x=html, css="div i")
node
class(node)

text <- html_text(x=node)
text
text[1]

text <- read_html("moviequotes.html") %>%
  html_elements("div i") %>%
  html_text() %>%
  .[1]
text

text <- read_html("moviequotes.html") %>%
  html_elements(xpath="//div//i") %>%
  html_text() %>%
  .[1]
text

library(XML)
html.parsed <- htmlParse("moviequotes.html")
text <- xpathSApply(html.parsed, "//div//i", xmlValue)
text[1]

########################
## 5.2 SelectorGadget ##
########################

library(rvest)
url <- "https://www.nobelprize.org/prizes/economic-sciences/"
quickfacts <- read_html(url) %>%
  html_elements(".factlist li") %>%
  html_text() 
quickfacts

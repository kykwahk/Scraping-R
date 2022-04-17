
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

#################
## 제4장 XPath ##
#################

###################
## 4.1 HTML 구조 ##
###################

html.string <- c(
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<title>Recommended Books</title>',
  '</head>',
  '<body>',
  '<h1>Books for Seaside Reading</h1>',
  '<p>Reading List</p>',
  '<table>',
  '<tr> <th>Title</th> <th>Author</th> <th>Publisher</th> </tr>',
  '<tr> <td>The Old Man and The Sea</td> <td>Ernest Hemingway</td> <td>Scribner</td> </tr>',
  '<tr> <td>Moby Dick</td> <td>Herman Melville</td> <td>Simon and Brown</td> </tr>',
  '</table>',
  '<br>',
  '<a href="http://www.amazon.com">Click Here to Buy</a>',
  '</body>',
  '</html>'
)
writeLines(html.string, "books.html")

library(pander)
openFileInOS("books.html")

##################
## 4.2 XML 구조 ##
##################

xml.string <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<books>',
  '<book lang="eng" format="paperback" pages="128">',
  '<title>The Old Man and The Sea</title>',
  '<author>',
  '<first_name>Ernest</first_name>',
  '<last_name>Hemingway</last_name>',
  '</author>',
  '<year>1995</year>',
  '<publisher>Scribner</publisher>',
  '</book>',
  '<book lang="eng" format="hardcover" pages="488">',
  '<title>Moby Dick</title>',
  '<author>',
  '<first_name>Herman</first_name>',
  '<last_name>Melville</last_name>',
  '</author>',
  '<year>2016</year>',
  '<publisher>Simon and Brown</publisher>',
  '</book>',
  '</books>'
)
writeLines(xml.string, "books.xml")

library(pander)
openFileInOS("books.xml")

xml.string2 <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<!DOCTYPE html>',
  '<root xmlns:t="http://www.html.com/myhtml" xmlns:m="http://www.xml.com/myxml">',
  '<t:head>',
  '<t:title>Look Inside!</t:title>',
  '</t:head>',
  '<m:book id="1">',
  '<m:title>Moby Dick</m:title>',
  '<m:author>Herman Melville</m:author>',
  '</m:book>',
  '</root>'
)
writeLines(xml.string2, "bookinside.xml")

library(pander)
openFileInOS("bookinside.xml")

#####################################
## 4.3 HTML/XML 파싱과 노드셋 추출 ##
#####################################

library(XML)
books <- xmlParse("books.xml")
books

class(books)

books <- xmlParse(xml.string, asText=TRUE)
class(books)

books2 <- htmlParse("books.html")
books2
class(books2)

root <- xmlRoot(books)
root
class(root)

xmlName(root)
xmlSize(root)
xmlValue(root)

root[[1]]

root[[1]][[1]]
root[[1]][[1]][[1]]

root[["book"]]
root["book"]

root["book"][[1]][[1]]
root["book"][[2]][[1]]

children <- xmlChildren(root)
children
class(children)

old.man <- children[[1]]
old.man

xmlName(old.man)
xmlSize(old.man)
xmlAttrs(old.man)
xmlGetAttr(old.man, name="format")
xmlValue(old.man)
getSibling(old.man)

xmlChildren(old.man)

author <- xmlChildren(old.man)[[2]]
author
xmlParent(author)
xmlChildren(author)
xmlChildren(author)[[2]][[1]]

xmlSApply(root[[1]], xmlValue)
xmlSApply(root, xmlAttrs)
xmlSApply(root, xmlGetAttr, name="format")

xmlToDataFrame(root)

xmlToDataFrame("books.xml")
xmlToDataFrame(books)

xmlToList(children[[1]])

####################################
## 4.4 XPath와 노드셋/데이터 추출 ##
####################################

html.string2 <- c(
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<title>Movie Quotes</title>',
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
writeLines(html.string2, "moviequotes.html")

library(pander)
openFileInOS("moviequotes.html")

library(XML)
quotes <- htmlParse("moviequotes.html")
quotes

## 노드셋 추출 - 절대경로와 상대경로

xpathSApply(doc=quotes, path="/html/body/div/p/i")

xpathSApply(quotes, "//body//p/i")

xpathSApply(quotes, "//p/i")

xpathSApply(quotes, "//div/*/i")
xpathSApply(quotes, "//div/node()/i")

xpathSApply(quotes, "//title/..")

xpathSApply(quotes, "//title | //h1")

queries <- c("//title", "//h1")
xpathSApply(quotes, queries)

## 노드셋 추출 - XPath 액시스

xpathSApply(quotes, "//a/ancestor::div")

xpathSApply(quotes, "//a/ancestor::div//i")

xpathSApply(quotes, "//b/preceding::i")

xpathSApply(quotes, "//p/preceding-sibling::h2")

xpathSApply(quotes, "//h1/following-sibling::p/a")

xpathSApply(quotes, "//i/ancestor::p/a")

xpathSApply(quotes, "//title/parent::*")
xpathSApply(quotes, "//title/parent::node()")

xpathSApply(quotes, "//title/..")

xpathSApply(quotes, "//title/parent::* | //i/ancestor::*/a")

## 노드셋 추출 - XPath 프레디킷

xpathSApply(quotes, "//p[./i]")

xpathSApply(quotes, "//div[@date='December/16/1970']//i")
xpathSApply(quotes, "//div[@date='December/16/1970' or @genre='comedy']//i")

xpathSApply(quotes, "//div/p[position()=1]")

xpathSApply(quotes, "//div/p[1]")

xpathSApply(quotes, "//div/p[position()=last()]")
xpathSApply(quotes, "//div/p[last()]")

xpathSApply(quotes, "//div/p[last()-1]")

xpathSApply(quotes, "//div[count(.//a)>0]")

xpathSApply(quotes, "//div[count(./@*)>=4]")

xpathSApply(quotes, "//*[string-length(text())>60]")

xpathSApply(quotes, "//*[not(string-length(text())<=60)]")

xpathSApply(quotes, "//div[not(count(./@*)<4)]")

xpathSApply(quotes, "//div[@lang]")
xpathSApply(quotes, "//div[not(@lang)]")

xpathSApply(quotes, "//div[.//a]")

xpathSApply(quotes, "//div[@date='December/16/1970']")

xpathSApply(quotes, "//*[contains(text(), 'Love')]")

xpathSApply(quotes, "//*[contains(text(), 'love')]")

xpathSApply(quotes, "//div[starts-with(./@time, '1h')]/p")

xpathSApply(quotes, "//div[substring-before(./@date, '/')='December']/p[2]")

xpathSApply(quotes, 
            "//div[substring-after(substring-after(./@date, '/'), '/')='1970']
            /p[2]")

## 데이터 추출

xpathSApply(doc=quotes, path="//p/i", fun=xmlValue)

xpathSApply(quotes, "//*[contains(text(), 'love')]")
xpathSApply(quotes, "//*[contains(text(), 'love')]", xmlValue)

xpathSApply(quotes, "//div", xmlAttrs)

xpathSApply(quotes, "//div", xmlGetAttr, "date")

xpathSApply(quotes, "//div", xmlGetAttr, "lang")

lowertextFun <- function(x) {
  x <- tolower(xmlValue(x))
  return(x)
}

xpathSApply(quotes, "//p/i", fun=lowertextFun)

yearFun <- function(x) {
  library(stringr)
  date <- xmlGetAttr(x, "date")
  year <- str_extract(date, "[0-9]{4}")
  return(year)
}

xpathSApply(quotes, "//div", yearFun)

langFun <- function(x) {
  lang <- xmlGetAttr(x, "lang")
  lang <- ifelse(is.null(lang), "Not Exist", lang)
  return(lang)
}

xpathSApply(quotes, "//div", langFun)

bookinside <- xmlParse("bookinside.xml")
bookinside

xpathSApply(bookinside, "//title", xmlValue)

xpathSApply(bookinside, "//*[local-name()='title']", xmlValue)

xpathSApply(bookinside, "//t:title", xmlValue)

xpathSApply(bookinside, "//x:title", 
            namespaces=c(x="http://www.html.com/myhtml"), xmlValue)

xpathSApply(bookinside, "//m:title", xmlValue)
xpathSApply(bookinside, "//x:title", 
            namespaces=c(x="http://www.xml.com/myxml"), xmlValue)

xmlNamespaceDefinitions(bookinside)

ns <- xmlNamespaceDefinitions(bookinside)[[2]]$uri
ns
xpathSApply(bookinside, "//x:title", namespaces=c(x=ns), xmlValue)

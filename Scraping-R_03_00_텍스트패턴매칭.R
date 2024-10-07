
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

###########################
## 제3장 텍스트 패턴매칭 ##
###########################

####################
## 3.1 정규표현식 ##
####################

?regex

## 문자열 매칭

string <- c("banana is yellow", 
            "panda is next to bamboo", 
            "X-men are dancing the samba")
string
grep(pattern="X-men", x=string)
grep(pattern="X-men", x=string, value=TRUE)

grep("x-men", string, value=TRUE)
grep("x-men", string, value=TRUE, ignore.case=TRUE)

grep("ana", string, value=TRUE)
grep("next to", string, value=TRUE)

grep("ba", string, value=TRUE)
grep("^ba", string, value=TRUE)
grep("ba$", string, value=TRUE)

grep("banana|bamboo", string, value=TRUE)

grep("p.", string, value=TRUE)

## 문자 클래스

string <- c("R is free software.",
            "The R Foundation for Statistical Computing",
            "Platform: x86_64-w64-mingw32/x64")
string
grep("[Pp]", string, value=TRUE)

grep("[0-9]", string, value=TRUE)
grep("[0123456789]", string, value=TRUE)
grep("[A-C]", string, value=TRUE)

grep("[z.]", string, value=TRUE)

grep("[^0-9a-zA-Z ]", string, value=TRUE)

string <- "Tweet (@tweet) Food for thought 2030-12-31"
string
gsub(pattern="[[:alpha:]]", replacement="", x=string)

gsub(pattern="[:alpha:]", replacement="", x=string)

gsub("[[:alnum:]]", "*", string)
gsub("[[:punct:]]", "*", string)
gsub("[[:digit:]]", "*", string)
gsub("[[:blank:]]", "*", string)

gsub("[^[:blank:]]", "_", string)

gsub("[[:punct:]0-9 ]", "", string)

gsub("t[[:alpha:]][[:alpha:]][[:alpha:]]t", "*", string)

## 수량자

string <- "Tweet (@tweet) Food for thought 2030-12-31"
gsub("t[[:alpha:]]{3}t", "*", string)
gsub("t[[:alpha:]]{5}t", "*", string)

string <- c("x", "xz", "xyz", "xyyz", "xyyyz", "xyxyxyz")
string
grep("xy?z", string, value=TRUE)
grep("xy*z", string, value=TRUE)
grep("xy+z", string, value=TRUE)
grep("xy{2}z", string, value=TRUE)
grep("xy{2,}z", string, value=TRUE)
grep("xy{2,3}z", string, value=TRUE)

grep("(xy){2,3}", string, value=TRUE)

grep("xy{2,3}", string, value=TRUE)

## 그리디 매칭 vs. 레이지 매칭

string <- "eeeAiiZoooAuuuZeee"
string
regmatches(x=string, m=gregexpr(pattern="A.*Z", text=string))

regmatches(string, gregexpr("A.*?Z", string))

html.doc <- "<TITLE>Happy Days</TITLE>"
gsub("<.*>", "", html.doc)

gsub("<.*?>", "", html.doc)

## 이스케이프 시퀀스

string <- 'Alice's Adventures'

string <- 'Alice\'s Adventures'
string

string <- "long\t\ttab lines can be\nbroken with newlines"
string
cat(string)
gsub("\t\t", " ", string)
cat(gsub("\t\t", " ", string))

string <- "I need 100$, but she gave me a ^. I\\don't\\know\\why."
cat(string)
gsub(".", "!", string)
gsub("\\.", "!", string)

gsub("\\$", " dollars", string)
gsub("\\^", "carrot", string)
gsub("\\\\", " ", string)

gsub("\\^\\.", "carrot!", string)
gsub("^.", "carrot!", string, fixed=TRUE)

library(stringr)
str_extract_all(string, fixed("^."))
str_replace_all(string=string, pattern=fixed("^."), replacement="carrot!")

## 문자 클래스 시퀀스

string <- "<123> Alice's Adventures in Wonder_land!-$10"
string

gsub("\\w", "*", string)

gsub("\\w+", "*", string)

gsub("\\W+", "+", string)

gsub("\\d+", "*", string)
gsub("\\D+", "+", string)

gsub("\\s+", "*", string)
gsub("\\S+", "+", string)

cat("Badly \t spaced text \n can be fixed.")
gsub("\\s+", " ", "Badly \t spaced text \n can be fixed.")
gsub("\\S+", " ", "Badly \t spaced text \n can be fixed.")

gsub("\\b", "_", string, perl=TRUE)

gsub("d", "ds", string)

gsub("d\\b", "ds", string, perl=TRUE)

gsub("\\B", "+", string, perl=TRUE)

txt <- c("Korea", "Korean", "Koreans")
txt

grep("Korea", txt, value=TRUE)

grep("\\bKorea\\b", txt, value=TRUE, perl=TRUE)

grep("Korea\\b", txt, value=TRUE, perl=TRUE)

txt <- "banana is next to bamboo and panda"
txt
gsub("\\<b", "*", txt)

gsub("^b", "*", txt)

gsub("a\\>", "*", txt)
gsub("a$", "*", txt)

## 백레퍼런스

txt <- "# a small thing makes a big difference #"
txt
gsub("([[:alpha:]]).+\\1", "*", txt)

gsub("([[:alpha:]]).+?\\1", "*", txt, perl=TRUE)

library(stringr)
str_replace_all(txt, "([[:alpha:]]).+\\1", "*")
str_replace_all(txt, "([[:alpha:]]).+?\\1", "*")
gsub("([[:alpha:]]).+\\1", "*", txt, perl=TRUE)
gsub("([[:alpha:]]).+?\\1", "*", txt, perl=TRUE)

html <- "<div class='power'>100%</div>"

gsub("(<.*?>)(.*)(<.*?>)", "\\2", html)

state.name
grep("^New", state.name, value=TRUE, ignore.case=TRUE)

state <- gsub("^New(.*[xy].*)", "NEW\\1", state.name, ignore.case=TRUE)

grep("^New", state, value=TRUE, ignore.case=TRUE)

## 예: 전화번호부

telephones <- "Barabasi, Albert-Laszlo917 1843James Bond(02)563-1987(1)John F. Kennedy051-776-5879(123)Dr. Who(062) 324-9576McCartney, J. Paul0648323912"
telephones

name <- unlist(regmatches(telephones, gregexpr("[[:alpha:]., -]{2,}", telephones)))
name

unlist(regmatches(telephones, gregexpr("[[:alpha:]., -]", telephones)))
unlist(regmatches(telephones, gregexpr("[[:alpha:]., -]{1,}", telephones)))

pattern <- "\\(?(\\d{2,3})?\\)?(-| )?\\d{3}(-| )?\\d{4}\\(?(\\d{1,3})?\\)?"
phone <- unlist(regmatches(telephones, gregexpr(pattern, telephones)))
phone

data.frame(Name=name, Phone=phone)

library(stringr)
name <- unlist(str_extract_all(telephones, "[[:alpha:]., -]{2,}"))
pattern <- "\\(?(\\d{2,3})?\\)?(-| )?\\d{3}(-| )?\\d{4}\\(?(\\d{1,3})?\\)?"
phone <- unlist(str_extract_all(telephones, pattern))
data.frame(Name=name, Phone=phone)

#####################
## 3.2 텍스트 함수 ##
#####################

## base 패키지

# 패턴검출
string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")
grep("data", string)

grep("data", string, value=TRUE)

string[grep("data", string)]

grep("useful|helpful", string, value=TRUE, invert=TRUE)

grepl("data", string)

head(state.name)
grepl("new", state.name, ignore.case=TRUE)
state.name[grepl("new", state.name, ignore.case=TRUE)]
sum(grepl("new", state.name, ignore.case=TRUE))

# 패턴위치
regexpr("data", string)

gregexpr("data", string)

# 패턴추출
regmatches(string, regexpr("data", string))
regmatches(string, gregexpr("data", string))

regmatches(string, gregexpr("data", string), invert=TRUE)

# 패턴치환
sub("data", "text", string)

gsub("data", "text", string)

# 패턴분할
strsplit(x=string, split=" ")

unlist(strsplit(string, " "))
unique(unlist(strsplit(string, " ")))

## stringr 패키지

# 패턴검출
library(stringr)
string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")
str_detect(string=string, pattern="data")

str_detect(string, "DATA")

str_detect(string, fixed("DATA", ignore_case=TRUE))

str_detect(c("abz", "ayz", "a.z"), "a.z")

str_detect(c("abz", "ayz", "a.z"), fixed("a.z"))

str_detect(c("abz", "ayz", "a.z"), "a\\.z")

# 패턴위치
str_locate(string, "data")
str_locate_all(string, "data")

# 패턴추출
str_extract(string, "data")
str_extract_all(string, "data")

str_extract_all(string, "data", simplify=TRUE)

unlist(str_extract_all(string, "data"))

sentences5 <- sentences[1:5]
sentences5

str_extract(sentences5, "(a|A|the|The) (\\w+)")

str_match(sentences5, "(a|A|the|The) (\\w+)")

str_extract_all(sentences5, "(a|A|the|The) (\\w+)")
str_match_all(sentences5, "(a|A|the|The) (\\w+)")

telephones <- "Barabasi, Albert-Laszlo917 1843James Bond(02)563-1987(1)John F. Kennedy051-776-5879(123)Dr. Who(062) 324-9576McCartney, J. Paul0648323912"
name.pattern <- "([[:alpha:]., -]{2,})"
phone.pattern <- "(\\(?(\\d{2,3})?\\)?(-| )?\\d{3}(-| )?\\d{4}\\(?(\\d{1,3})?\\)?)"
phonebook <- str_match_all(telephones, paste0(name.pattern, phone.pattern))
phonebook
phonebook <- phonebook[[1]]
phonebook

phonebook[,2]
phonebook[,3]
data.frame(Name=phonebook[,2], Phone=phonebook[,3])

# 패턴치환
str_replace(string=string, pattern="data", replacement="text")
str_replace_all(string, "data", "text")

# 패턴분할
str_split(string, " ")

unlist(str_split(string, " "))
unique(unlist(str_split(string, " ")))

str_split(string, " ", n=3)
str_split(string, " ", n=3, simplify=TRUE)

# 그 밖의 유용한 함수
str_length(string)

str_count(string, "data")

str_count(string, "\\w+")

str_pad(string=c("a", "abc", "abcde"), width=6, side="left", pad=" ")

mon <- c(1:12)
str_pad(mon, 2, side="left", pad="0")

str.pad <- str_pad(string, max(str_length(string)), side="both", pad=" ")
str.pad

str_trim(string=str.pad, side="both")

str_c("data", "mining")
str_c("data", "mining", sep=" ")

str.mining <- str_c(c("data mining", "text mining"), "is useful", sep=" ")
str.mining

str_c(str.mining, collapse="; ")

str_c(str.mining, collapse="\n")
cat(str_c(str.mining, collapse="\n"))

str_sub(string=str.mining, start=1, end=4)

str_sub(str.mining, 5, 5) <- "-"
str.mining

str_sub(string="abcdefg", start=-2)

str_sub(string="abcdefg", end=-3)

################
## 3.3 인코딩 ##
################

Sys.getlocale()

Sys.localeconv()

Sys.getlocale(category="LC_CTYPE")

sapply(c("LC_COLLATE", "LC_CTYPE"), Sys.getlocale)

localeToCharset()

l10n_info()

star <- "별 헤는 밤, Copyright 1941. 윤동주."
star

Encoding(star)

localeToCharset()

star2 <- iconv(x=star, from=localeToCharset(), to="CP949")
Encoding(star2)
star2

star2 <- iconv(x=star2, from="CP949", to=localeToCharset())
Encoding(star2)
star2

length(iconvlist())
sample(iconvlist(), 10)

library(readr)
guess_encoding(file="https://www.naver.com")

library(stringr)
html <- readLines(con="https://www.naver.com")
unlist(str_extract_all(html, "<meta.+?>"))[1:2]

library(readr)
library(httr)
guess_encoding(file="https://www.mk.co.kr")
get <- GET(url="https://www.mk.co.kr")
html <- content(get, as="text", encoding="EUC-KR")
html

html <- content(get, as="text", encoding="UTF-8")
html.doc <- unlist(str_extract_all(html, "<p>.*?</p>"))
html.doc
gsub("<.*?>", "", html.doc[c(5:8)])

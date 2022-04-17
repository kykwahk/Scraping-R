
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

##########################
## 제2장 파일 다운로드  ##
##########################

###################
## 2.1 파일 읽기 ##
###################

## CSV

read.csv(file="product.csv")

read.csv("product-with-no-header.csv", header=FALSE)

## 테이블

read.table(file="product.txt")

read.table("product.txt", header=TRUE)

read.table("product-colon.txt", sep=":", header=TRUE)

read.table("product-missing.txt", header=TRUE)
read.table("product-missing.txt", header=TRUE, na.strings=".")

library(readr)
read_delim("product.txt", delim=" ", col_names=TRUE)
read_delim("product-with-no-header.csv", delim=",", col_names=c("ID", "NAME", "PRICE"))

read.fwf(file="product-fwf.txt", widths=c(4,-1,10,8))

read.fwf("product-fwf.txt", widths=c(4,-1,10,8), col.names=c("id", "name", "price"))

## 비정형 텍스트

readLines(con="won-dollar.txt")

paste(readLines(con="won-dollar.txt"), collapse=" ")

readLines("won-dollar.txt", n=2)

scan(file="won-dollar.txt", what=character())

scan("won-dollar.txt", what=list(character(), numeric(), numeric()))

scan("won-dollar.txt", 
     what=list(date=character(), buy=numeric(), sell=numeric()))

scan("won-dollar.txt", 
     what=list(date=character(), buy=numeric(), sell=numeric()), nlines=2)
scan("won-dollar.txt",
     what=list(date=character(), buy=numeric(), sell=numeric()), skip=3)

## 스프레드시트

library(readxl)
read_excel(path="product.xlsx", sheet=1)

library(openxlsx)
read.xlsx(xlsxFile="product.xlsx", sheet=1)

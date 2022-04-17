
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

##############
## 2.2 사례 ##
##############

## 이상한 나라의 앨리스 @프로젝트 구텐베르크

url <- "https://www.gutenberg.org/files/11/11-0.txt"
alice <- readLines(url, encoding="UTF-8")
alice[1:10]
paste(alice, collapse=" ")

alice.chap1 <- character(10)
skip <- 54
for (i in 1:10) {
  one.line <- scan(url, what="", skip=skip, nlines=1, encoding="UTF-8")
  alice.chap1[i] <- paste(one.line, collapse=" ")
  skip <- skip + 1
}
alice.chap1

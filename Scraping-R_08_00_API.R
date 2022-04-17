
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제8장. API ##
################

#############################
## 8.3 API 래퍼 소프트웨어 ##
#############################

install.packages("comtradr")
library(comtradr)
ct_search(reporters="USA", partners=c("China", "Japan", "Rep. of Korea"), 
          trade_direction="imports")
ct_country_lookup("Korea", "partner")

install.packages("exoplanets")
library(exoplanets)
exoplanets(table="ps", columns=c("pl_name", "pl_orbper", "pl_massj", "discoverymethod"))

install.packages("gutenbergr")
library(gutenbergr)
View(gutenberg_metadata)
gutenberg_works(title %in% "Moby Dick")
gutenberg_works(title=="Moby Dick")
gutenberg_download(gutenberg_works(title=="Moby Dick"))

install.packages("Lahman")
library(Lahman)
battingStats()

install.packages("Quandl")
library(Quandl)
Quandl(code="OPEC/ORB")
Quandl(code="FRED/GDP", collapse="annual")

install.packages("quantmod")
library(quantmod)
getSymbols(Symbols="GOOG", auto.assign=FALSE)
barChart(GOOG)

install.packages("rfishbase")
library(rfishbase)
species("Salmo trutta", fields=c("Species", "SpecCode", "PriceCateg", "Vulnerability"))
species(common_to_sci("trout")$Species, fields=c("Species", "SpecCode", "PriceCateg", "Vulnerability"))

install.packages("rgbif")
library(rgbif)
occ_search(scientificName="Ursus americanus", limit=50)
name_lookup(query="mammalia", limit=10)
name_suggest(q="tiger")

install.packages("rnoaa")
library(rnoaa)
storms2020 <- se_data(year=2020, type="details")
View(storms2020)

install.packages("wbstats")
library(wbstats)
wb_search("gdp.*capita.*current")
wb_search("Unemployment, Total")
wb_data(country="KR", indicator="NY.GDP.PCAP.CD", start_date=2011, end_date=2020)

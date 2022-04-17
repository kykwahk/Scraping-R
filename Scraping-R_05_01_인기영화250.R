
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

## 인기 영화 250 @IMDb

library(tidyverse)
library(rvest)
url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- read_html(url) 
html

# 제목
title <- html_elements(html, ".titleColumn a") %>%
  html_text()
head(title)
length(title)

# 순위
html_elements(html, ".titleColumn") %>%  
  html_text()
html_elements(html, ".titleColumn") %>%  
  html_text2()

rank <- html_elements(html, ".titleColumn") %>%  
  html_text2() %>% 
  parse_number()
head(rank)
length(rank)

# 개봉연도
html_elements(html, ".secondaryInfo") %>%
  html_text()

year <- html_elements(html, ".secondaryInfo") %>%
  html_text() %>% 
  parse_number()
head(year)
length(year)

# 평점
rating <- html_elements(html, ".ratingColumn.imdbRating > strong") %>%
  html_text() %>% 
  as.numeric()
head(rating)
length(rating)

title.url <- "https://www.imdb.com/title/tt0111161/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=e31d89dd-322d-4646-8962-327b42fe94b1&pf_rd_r=16WKASGWQF1F0MMHJ27W&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_tt_1"
html <- read_html(title.url) 

# 감독
html_elements(html, "div[data-testid=title-pc-wide-screen] 
                     li[data-testid=title-pc-principal-credit]:nth-child(1) 
                     li.ipc-inline-list__item") %>% 
  html_text() 

title.url2 <- "https://www.imdb.com/title/tt0133093/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=e31d89dd-322d-4646-8962-327b42fe94b1&pf_rd_r=3R9PBC18V6W5WCEQZXA6&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_tt_16"
html2 <- read_html(title.url2) 
html_elements(html2, "div[data-testid=title-pc-wide-screen] 
                      li[data-testid=title-pc-principal-credit]:nth-child(1) 
                      li.ipc-inline-list__item") %>% 
  html_text() 
html_elements(html2, "div[data-testid=title-pc-wide-screen] 
                      li[data-testid=title-pc-principal-credit]:nth-child(1) 
                      li.ipc-inline-list__item:nth-child(1)") %>% 
  html_text()

director <- html_elements(html, "div[data-testid=title-pc-wide-screen] 
                                 li[data-testid=title-pc-principal-credit]:nth-child(1) 
                                 li.ipc-inline-list__item:nth-child(1)") %>% 
  html_text() 
director

# 상영 시간
runtime <- html_elements(html, "ul[data-testid^=hero-title-block] 
                                li[class=ipc-inline-list__item]:last-child") %>% 
  html_text() 
runtime

hour <- str_extract(runtime, "\\d+h") %>% 
  parse_number() %>% 
  replace_na(replace=0)
min <- str_extract(runtime, "\\d+m") %>% 
  parse_number() %>% 
  replace_na(replace=0)
runtime <- hour*60 + min
runtime

# 장르
html_elements(html, "div[data-testid=genres] a") %>% 
  html_text()

title.url3 <- "https://www.imdb.com/title/tt0133093/?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=e31d89dd-322d-4646-8962-327b42fe94b1&pf_rd_r=3R9PBC18V6W5WCEQZXA6&pf_rd_s=center-1&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_tt_16"
html3 <- read_html(title.url3) 
html_elements(html3, "div[data-testid=genres] a") %>% 
  html_text()
html_elements(html3, "div[data-testid=genres] a:first-child") %>% 
  html_text()

genre <- html_elements(html, "div[data-testid=genres] a:first-child") %>% 
  html_text() 
genre

# 모든 영화로 확장
url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- read_html(url) 
title.url <- html_elements(html, ".titleColumn a") %>% 
  html_attr(name="href")
head(title.url, 3)

title.url <- str_c("https://www.imdb.com", title.url)
head(title.url, 3)
length(title.url)

extra.info <- data.frame(matrix(nrow=length(title.url), ncol=3))
colnames(extra.info) <- c("director", "runtime", "genre")
head(extra.info)

for (i in 1:nrow(extra.info)) {
  html <- read_html(title.url[i]) 
  director <- html_elements(html, "div[data-testid=title-pc-wide-screen] 
                                   li[data-testid=title-pc-principal-credit]:nth-child(1) 
                                   li.ipc-inline-list__item:nth-child(1)") %>% 
    html_text()
  runtime <- html_elements(html, "ul[data-testid^=hero-title-block] 
                                  li[class=ipc-inline-list__item]:last-child") %>% 
    html_text() 
  hour <- str_extract(runtime, "\\d+h") %>% 
    parse_number() %>% 
    replace_na(replace=0)
  min <- str_extract(runtime, "\\d+m") %>% 
    parse_number() %>% 
    replace_na(replace=0)
  runtime <- hour*60 + min
  genre <- html_elements(html, "div[data-testid=genres] a:first-child") %>% 
    html_text() 
  extra.info[i,] <- c(director, runtime, genre)
  Sys.sleep(sample(10,1)*0.5)
}

extra.info <- extra.info %>% 
  as_tibble() %>% 
  mutate(runtime=as.numeric(runtime), genre=as.factor(genre))
extra.info

imdb.movie250 <- tibble(rank=rank, title=title, year=year, rating=rating) %>% 
  bind_cols(extra.info) 
imdb.movie250

save(imdb.movie250, file="imdb-movie250.rda")
load("imdb-movie250.rda")

library(kableExtra)
imdb.movie250 %>% 
  kbl(caption="IMDb Top 250 Movies") %>% 
  kable_styling(html_font="Cambria",
                bootstrap_options=c("striped", "hover", "condensed"))
imdb.movie250 %>% 
  kbl(caption="IMDb Top 250 Movies") %>% 
  kable_styling(html_font="Cambria",
                bootstrap_options=c("striped", "hover", "condensed")) %>% 
  save_kable("imdb-movie250.html")
library(pander)
openFileInOS("imdb-movie250.html")

table(imdb.movie250$genre)

genre.df <- data.frame(table(imdb.movie250$genre))
str(genre.df)
genre.df <- genre.df %>% 
  mutate(label=str_c(Var1, Freq, sep="\n"))
genre.df

# [그림 5-12]
library(treemap)
windows(width=7.0, height=5.5)
treemap(dtf=genre.df, index="label", vSize="Freq", type="value", vColor="Freq", 
        palette="PuOr", border.col="snow", border.lwds=3, force.print.labels=FALSE, 
        title="Distribution of IMDb Top 250 Movies by Genre", title.legend="Counts")

?RColorBrewer
?treemap

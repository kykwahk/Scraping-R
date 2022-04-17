
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

####################
## 제6장 셀레니움 ##
####################

##############
## 6.4 사례 ##
##############

## 동영상 정보 @유튜브

library(tidyverse)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.youtube.com")

search <- remDr$findElement(using="xpath", value="//input[@id='search']")
search$highlightElement()
search$sendKeysToElement(list("여행"))

btn.search <- remDr$findElement(using="xpath", value="//button[@id='search-icon-legacy']")
btn.search$highlightElement()
btn.search$clickElement()

search$clearElement()
search$sendKeysToElement(list("여행", key="enter"))

body <- remDr$findElement(using="xpath", value="//body")
body$sendKeysToElement(list(key="page_down"))

# 스크롤1
body <- remDr$findElement(using="xpath", value="//body")
for (i in 1:10) {
  body$sendKeysToElement(list(key="page_down"))
  Sys.sleep(1)
}

# 스크롤2
library(R.utils)
body <- remDr$findElement(using="xpath", value="//body")
withTimeout({
  repeat {
    body$sendKeysToElement(list(key="page_down"))
    Sys.sleep(1)
  }
}, timeout=10, onTimeout="silent")

tryCatch(
  withTimeout({
    repeat {
      body$sendKeysToElement(list(key="page_down"))
      Sys.sleep(1)
    }
  }, timeout=10, onTimeout="error"), 
  error=function(e) {
    e
    message("Timeout. Stopped.")
    }
)

# 스크롤3
remDr$executeScript("window.scrollBy(0,1000);")

withTimeout({
  repeat {
    remDr$executeScript("window.scrollBy(0,1000);")
    Sys.sleep(1)
  }
}, timeout=10, onTimeout="silent")

# 스크롤4
remDr$navigate("https://www.youtube.com")
search <- remDr$findElement(using="xpath", value="//input[@id='search']")
search$sendKeysToElement(list("여행"))
search$sendKeysToElement(list(key="enter"))

body <- remDr$findElement(using="xpath", value="//body")
flag <- TRUE
i <- 0
while (flag) {
  i <- i + 1
  body$sendKeysToElement(list(key="page_down"))
  Sys.sleep(1)
  if (exists("pagesource")) {
    if (pagesource == remDr$getPageSource()[[1]]) {
      flag <- FALSE
      cat(str_c("Scrolled down ", i, " times.\n"))
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  } else {
    pagesource <- remDr$getPageSource()[[1]]
  }
}
body$sendKeysToElement(list(key="home"))

# 스크롤5
remDr$navigate("https://www.youtube.com")
search <- remDr$findElement(using="xpath", value="//input[@id='search']")
search$sendKeysToElement(list("여행"))
search$sendKeysToElement(list(key="enter"))

page <- remDr$findElement(using="xpath", value="//ytd-page-manager[@id='page-manager']")
page$highlightElement()

remDr$executeScript("return document.getElementById('page-manager').scrollHeight;")

remDr$executeScript("window.scrollTo(0, document.getElementById('page-manager').scrollHeight);")

remDr$executeScript("return document.getElementById('page-manager').scrollHeight;")

remDr$executeScript("window.scrollTo(0, document.getElementById('page-manager').scrollHeight);")
remDr$executeScript("return document.getElementById('page-manager').scrollHeight;")

height.last <- 0
i <- 0
repeat {   
  i <- i + 1
  remDr$executeScript("window.scrollTo(0, document.getElementById('page-manager').scrollHeight);")
  Sys.sleep(3) 
  height.new <- remDr$executeScript("return document.getElementById('page-manager').scrollHeight;")
  if(unlist(height.last) == unlist(height.new)) {
    cat(str_c("Scrolled down ", i, " times.\n"))
    break
  } else {
    height.last <-  height.new
  }
}

html <- remDr$getPageSource()[[1]]

title <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']/yt-formatted-string") %>% 
  html_text() 
length(title)
head(title, 3)

# 크리에이터가 "YouTube 영화"인 광고도 추출
creator <- read_html(html) %>% 
  html_elements(xpath="//div[@id='channel-info']//yt-formatted-string[@id='text']/a") %>% 
  html_text()
length(creator)
head(creator, 3)
which(str_detect(creator, "YouTube 영화"))

creator <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']/yt-formatted-string
                /ancestor::div[@id='meta']/following-sibling::div[@id='channel-info']
                //yt-formatted-string[@id='text']/a") %>% 
  html_text() 
length(creator)
head(creator, 3)

view <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']/yt-formatted-string") %>% 
  html_attr("aria-label") %>% 
  str_extract("조회수\\s[0-9,]+회") %>% 
  parse_number()
length(view)
head(view, 3)
summary(view)

description <- read_html(html) %>% 
  html_elements(xpath="//div[@class='text-wrapper style-scope ytd-video-renderer']") %>%
  html_element(xpath="div/yt-formatted-string[@class='metadata-snippet-text style-scope ytd-video-renderer']") %>%
  html_text()
length(description)
head(description, 3)

url <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']/yt-formatted-string/parent::a") %>% 
  html_attr("href") %>% 
  str_c("https://www.youtube.com", .)
length(url)
head(url, 3)

youtube.travel <- tibble(title=title, creator=creator, view=view, 
                         description=description, url=url)
youtube.travel

save(youtube.travel, file="youtube-travel.rda")
load("youtube-travel.rda")

library(tidyverse)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.youtube.com/곽기영")

btn.tab <- remDr$findElements(using="xpath", 
                              value="//div[@id='tabsContent']/tp-yt-paper-tab/div")
length(btn.tab)
sapply(btn.tab, function(x) x$getElementText())

btn.tab[[3]]$clickElement()

btn.viewmore <- remDr$findElements(using="xpath", 
                                   value="//yt-formatted-string[@id='view-more']/a")
length(btn.viewmore)
sapply(btn.viewmore, function(x) x$getElementText())

btn.viewmore[[1]]$clickElement()

body <- remDr$findElement(using="xpath", value="//body")
body$sendKeysToElement(list(key="page_down"))

html <- remDr$getPageSource()[[1]]

playlist <- read_html(html) %>% 
  html_elements("h1#title") %>% 
  html_text()
playlist

video <- read_html(html) %>% 
  html_elements("#meta #video-title") %>% 
  html_text() %>% 
  str_trim()
head(video, 3)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.youtube.com/곽기영")
btn.tab <- remDr$findElements(using="xpath", 
                              value="//div[@id='tabsContent']/tp-yt-paper-tab/div")
btn.tab[[3]]$clickElement()
btn.viewmore <- remDr$findElements(using="xpath", 
                                   value="//yt-formatted-string[@id='view-more']/a")
playlist.data <- vector("list", length(btn.viewmore))
for (i in seq_along(btn.viewmore)) {
  btn.viewmore[[i]]$clickElement()
  body <- remDr$findElement(using="xpath", value="//body")
  flag <- TRUE
  j <- 0
  while (flag) {
    j <- j + 1
    body$sendKeysToElement(list(key="page_down"))
    Sys.sleep(1)
    if (exists("pagesource")) {
      if (pagesource == remDr$getPageSource()[[1]]) {
        flag <- FALSE
      } else {
        pagesource <- remDr$getPageSource()[[1]]
      }
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  }
  html <- remDr$getPageSource()[[1]]
  playlist <- read_html(html) %>% 
    html_elements("h1#title") %>% 
    html_text()
  video <- read_html(html) %>% 
    html_elements("#meta #video-title") %>% 
    html_text() %>% 
    str_trim()
  playlist.data[[i]] <- tibble(playlist=playlist, video=video)
  remDr$goBack()
  Sys.sleep(3)
  btn.viewmore <- remDr$findElements(using="xpath", 
                                     value="//yt-formatted-string[@id='view-more']/a")
}

playlist.data

playlist.video <- reduce(playlist.data, bind_rows)
playlist.video

remDr$navigate("https://www.youtube.com/곽기영")
btn.tab <- remDr$findElements(using="xpath", 
                              value="//div[@id='tabsContent']/tp-yt-paper-tab/div")
btn.tab[[2]]$clickElement()

body <- remDr$findElement(using="xpath", value="//body")
flag <- TRUE
i <- 0
while (flag) {
  i <- i + 1
  body$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  if (exists("pagesource")) {
    if (pagesource == remDr$getPageSource()[[1]]) {
      flag <- FALSE
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  } else {
    pagesource <- remDr$getPageSource()[[1]]
  }
}

html <- remDr$getPageSource()[[1]]

video <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("title") %>% 
  str_trim()
length(video)
head(video, 3)

view <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("aria-label") %>% 
  str_extract("조회수\\s[0-9,]+회") %>% 
  parse_number()
length(view)
head(view, 3)

url <- read_html(html) %>% 
  html_elements(xpath="//a[@id='video-title']") %>% 
  html_attr("href") %>% 
  str_c("https://www.youtube.com", .)
length(url)
head(url, 3)

video.all <- tibble(video=video, view=view, url=url)
video.all

youtube.playlist <- full_join(playlist.video, video.all, by="video")
youtube.playlist

save(youtube.playlist, file="youtube-playlist.rda")
load("youtube-playlist.rda")

youtube.playlist$title <- str_split(youtube.playlist$video, "🔑") %>% 
  map_chr(~.x[1]) %>% 
  str_split("-", n=2) %>% 
  map_chr(~.x[2]) %>% 
  str_trim()
youtube.playlist$keyword <- str_split(youtube.playlist$video, "🔑") %>% 
  map_chr(~.x[2]) %>% 
  str_trim()  
youtube.playlist

youtube.playlist %>% 
  drop_na(playlist) %>% 
  count(playlist)

youtube.playlist %>% 
  drop_na(playlist) %>% 
  group_by(playlist) %>% 
  summarise(views=sum(view))

edges <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  distinct(video, .keep_all=TRUE) %>%
  select(from=playlist, to=title)
edges

vertices <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  distinct(video, .keep_all=TRUE) %>% 
  select(name=title, keyword, view)
vertices

view.playlist <- youtube.playlist %>% 
  drop_na(playlist) %>% 
  group_by(playlist) %>% 
  summarise(view=sum(view)) %>% 
  rename(name=playlist)
view.playlist

vertices <- vertices %>% 
  bind_rows(view.playlist) %>% 
  replace_na(list(keyword="playlist"))
vertices
tail(vertices)

library(igraph)
mygraph <- graph_from_data_frame(d=edges, vertices=vertices)
mygraph

# [그림 6-26]
library(ggraph)
windows(width=7.0, height=7.0)
set.seed(1234)
ggraph(mygraph, layout="circlepack", weight=view) + 
  geom_node_circle(aes(fill=depth)) +
  theme_void() +
  geom_node_label(aes(label=str_c(name, ":", format(view, big.mark=",")), 
                      filter=(keyword=="playlist")), color="darkred", repel=TRUE,
                  position="jitter", fontface="bold") +
  geom_node_text(aes(label=str_c(name, ":", format(view, big.mark=",")),  
                     filter=(view > quantile(view[keyword!="Playlist"], 0.9)) 
                              & (keyword!="playlist")), color="orangered", 
                 position="jitter", fontface="bold") +
  theme(legend.position="FALSE") +
  scale_fill_distiller(palette="GnBu") +
  labs(title="곽기영 교수 유튜브 동영상 강의",
       subtitle="조회수 상위 동영상",
       caption="출처: YouTube") +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=16),
        plot.caption=element_text(size=13))

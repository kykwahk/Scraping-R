
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

## 글로벌 대학 순위 @US뉴스

library(tidyverse)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.usnews.com/education/best-global-universities/rankings")

remDr$deleteAllCookies()

body <- remDr$findElement(using="css selector", value="body")
body$sendKeysToElement(list(key="page_down"))

body$sendKeysToElement(list(key="home"))
flag.scroll <- TRUE
i <- 0
while (flag.scroll) {
  i <- i + 1
  body$sendKeysToElement(list(key="page_down"))
  Sys.sleep(sample(c(1, 2), 1))
  if (exists("pagesource")) {
    if (pagesource == remDr$getPageSource()[[1]]) {
      flag.scroll <- FALSE
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  } else {
    pagesource <- remDr$getPageSource()[[1]]
  }
}

btn.loadmore <- remDr$findElements(using="css selector", value="#rankings > div > button")
class(btn.loadmore)
length(btn.loadmore)
btn.loadmore[[1]]$highlightElement()
btn.loadmore[[1]]$isElementDisplayed()

btn.loadmore[[1]]$clickElement()

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.usnews.com/education/best-global-universities/rankings")
remDr$deleteAllCookies()
flag.btn <- TRUE
j <- 0
while (flag.btn) {
  body <- remDr$findElement(using="css selector", value="body")
  flag.scroll <- TRUE
  i <- 0
  while (flag.scroll) {
    i <- i + 1
    body$sendKeysToElement(list(key="page_down"))
    Sys.sleep(sample(c(1, 2), 1))
    if (exists("pagesource")) {
      if (pagesource == remDr$getPageSource()[[1]]) {
        flag.scroll <- FALSE
        cat(str_c("Scrolled down ", i, " times.\n"))
      } else {
        pagesource <- remDr$getPageSource()[[1]]
      }
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  }
  btn.loadmore <- remDr$findElements(using="css selector", value="#rankings > div > button")
  if (length(btn.loadmore) == 0) {
    message("Load more ended")
    flag.btn <- FALSE
    break
  }
  btn.loadmore[[1]]$clickElement()
  Sys.sleep(3)
  j <- j + 1
  cat(str_c("Loaded more ", j, " times.\n"))
}

html <- remDr$getPageSource()[[1]]

rank <- read_html(html) %>% 
  html_elements(".RankList__Rank-sc-2xewen-2 > strong") %>%
  html_text() %>% 
  na_if("Unranked") %>% 
  parse_number()
length(rank)
head(rank)

university <- read_html(html) %>% 
  html_elements(".mb2.md-mb3.lg-mb4 a") %>%
  html_text()
length(university)
head(university)

location <- read_html(html) %>% 
  html_elements(".mb2.md-mb3.lg-mb4 p") %>%
  html_text() 
length(location)
head(location)

score <- read_html(html) %>% 
  html_elements(".DetailCardGlobalUniversities__CardStats-sc-1v60hm5-7 > 
                div:nth-child(1) .QuickStatHug__Description-hb1bl8-1") %>%
  html_text() %>% 
  na_if("N/A") %>% 
  parse_number()
length(score)
head(score)

enrollment <- read_html(html) %>% 
  html_elements(".DetailCardGlobalUniversities__CardStats-sc-1v60hm5-7 > 
                div:nth-child(2) .QuickStatHug__Description-hb1bl8-1") %>%
  html_text() %>% 
  na_if("N/A") %>% 
  parse_number()
length(enrollment)
head(enrollment)

usnews.univ <- tibble(rank=rank, university=university, location=location, 
                      score=score, enrollment=enrollment) %>% 
  separate(col=location, into=c("country", "city"), sep="\\|")
usnews.univ

save(usnews.univ, file="usnews-univ.rda")
load("usnews-univ.rda")

usnews.univ %>% 
  drop_na(rank) %>% 
  mutate(country=fct_lump_n(country, n=10)) %>% 
  count(country, sort=TRUE)

univ.rank.country <- usnews.univ %>% 
  drop_na(rank) %>% 
  group_by(country) %>% 
  summarise(count=n(), 
            score.mean=mean(score, na.rm=TRUE),
            enroll.mean=mean(enrollment, na.rm=TRUE)) %>% 
  slice_max(count, n=10)
univ.rank.country

# [그림 6-28]
windows(width=7.0, height=7.0)
ggplot(univ.rank.country) +
  geom_hline(aes(yintercept=y), data.frame(y=seq(0, 300, 100)), 
             color="lightgray") +
  geom_col(aes(x=fct_reorder(str_wrap(country, 5), count), 
               y=count, fill=score.mean), color="dimgray", 
           position="dodge2", alpha=0.9, show.legend=TRUE) +
  geom_segment(aes(x=fct_reorder(str_wrap(country, 5), count), 
                   y=0,
                   xend=fct_reorder(str_wrap(country, 5), count),
                   yend=300), linetype="dashed", color="gray20") +
  scale_y_continuous(limits=c(-100, 315), expand=c(0, 0),
                     breaks=c(0, 100, 200, 300)) + 
  annotate(x=10, y=220, label="Number of\nUniversities", geom="text", 
           color="cyan", angle=23, size=3.5) +
  annotate(x=10.8, y=115, label="100", geom="text", 
           color="gray20") +
  annotate(x=10.8, y=215, label="200", geom="text", 
           color="gray20") +
  annotate(x=10.8, y=315, label="300", geom="text", 
           color="gray20") +
  scale_fill_distiller(palette="YlOrRd", direction=1) +
  guides(fill=guide_colorbar(barwidth=15, barheight=0.5, 
                             title="Average Score",
                             title.position="top", 
                             title.hjust=0.5)) +
  labs(title="Best Global Universities",
       subtitle="Number of universities and average scores by top 10 country",
       caption="Source: US News") +
  theme_void() +  
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(color="gray10", size=11),
        legend.position="bottom") +
  coord_polar()
 
install.packages("googleway")
library(googleway)
gapi.key <- "YourGoogleAPIKey"
loc <- google_geocode(address="Harvard University", key=gapi.key)
str(loc, max.level=1)
names(loc$results)
loc$results$formatted_address
loc$results$geometry$location

geocode_coordinates(loc)

getGeocode <- function(address, key) {
  geo.data <- vector("list", length(address))
  for (i in seq_along(address)) {
    geo.loc <- google_geocode(address[i], key=key)
    geo.loc <- geo.loc$results$geometry$location
    if (is.null(geo.loc))
      next
    else {
      geo.data[[i]] <- geo.loc %>% 
        slice_head(n=1) %>% 
        mutate(address=address[i])
    }
    Sys.sleep(sample(10, 1)*0.1)
  }
  geo.result <- reduce(geo.data, bind_rows)
  return(geo.result)
}

geo.result <- getGeocode(address=paste(usnews.univ$university[1:50],
                                       usnews.univ$city[1:50], sep="|"),
                         key=gapi.key)
geo.result <- geo.result %>% 
  separate(col=address, into=c("university", "city"), sep="\\|")
head(geo.result, 3); tail(geo.result, 3)

univ.top <- left_join(usnews.univ[1:50,], geo.result, by=c("university", "city"))
univ.top

# [그림 6-29]
map.data <- map_data(map="world")
windows(width=8.0, height=5.5)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="olivedrab3", color="gray40", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(xlim=c(-180, 180), ylim=c(-55, 90), ratio=1.3) +
  geom_point(data=univ.top, aes(x=lng, y=lat), shape=21, 
             color="red", fill="orange", alpha=0.6, size=2.0, stroke=1.5) +
  labs(title="Best Global Universities",
       subtitle="Top 50 universities",
       caption="Source: US News") +
  theme_void() +
  theme(plot.title=element_text(face="bold"))

univ.top.us20 <- univ.top %>% 
  filter(country == "United States") %>% 
  slice_head(n=20)
univ.top.us20

# [그림 6-30]
map.data <- map_data(map="state")
windows(width=8.0, height=5.5)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="honeydew", color="royalblue", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(ratio=1.3) +
  geom_point(data=univ.top.us20, aes(x=lng, y=lat), shape=21, 
             color="gray", fill="blue", alpha=0.6, size=3.0, stroke=2.0) +
  geom_text(data=univ.top.us20[1:3,], 
            aes(x=lng, y=lat, label=paste(rank, "-", university)), 
            position=position_jitter(width=0, height=1),
            size=2.5, color="red", fontface="bold") + 
  labs(title="Best Global Universities",
       subtitle="Top 20 universities in US",
       caption="Source: US News") +
  theme_void() +
  theme(plot.title=element_text(face="bold"))

# [그림 6-31] 줌인
windows(width=5.0, height=7.5)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="honeydew", color="royalblue", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(xlim=c(-80, -70),  ylim=c(35, 45), ratio=1.3) +
  geom_point(data=univ.top.us20, aes(x=lng, y=lat), shape=21, 
             color="gray", fill="blue", alpha=0.6, size=3.0, stroke=2.0) +
  geom_text(data=univ.top.us20, 
            aes(x=lng, y=lat, label=paste(rank, "-", university)), 
            position=position_jitter(width=0, height=0.5),
            size=2.5, color="red", fontface="bold") + 
  labs(title="Best Global Universities",
       subtitle="Top universities in US eastern area",
       caption="Source: US News") +
  theme_void() +
  theme(plot.title=element_text(face="bold")) 

library(ggmap)
register_google(key=gapi.key)
map <- get_googlemap(center=c(-75, 40), zoom=6, maptype="terrain")

# [그림 6-32]
windows(width=7.0, height=7.0)
mybreaks <- seq(0, 40000, 10000)
ggmap(map) + 
  geom_point(data=arrange(univ.top.us20, enrollment), shape=20,
             aes(x=lng, y=lat, size=enrollment, 
                 color=enrollment, alpha=enrollment)) +
  geom_text(data=univ.top.us20, 
            aes(x=lng, y=lat, label=paste(rank, "-", university)), 
            position=position_jitter(width=0, height=0.5),
            size=2.5, color="red", fontface="bold") +
  scale_size_continuous(name="Enrollment", range=c(1, 15), breaks=mybreaks) +
  scale_alpha_continuous(name="Enrollment", range=c(0.1, 0.5), breaks=mybreaks) +
  scale_color_viridis_c(name="Enrollment", option="plasma", 
                        direction=-1, breaks=mybreaks) +
  labs(title="Best Global Universities",
       subtitle="Top universities in US eastern area",
       caption="Source: US News") +
  theme_void() + 
  guides(size=guide_legend(), color=guide_legend(),
         alpha=guide_legend()) +
  theme(plot.title=element_text(face="bold", color="orange"), 
        panel.border=element_rect(color="gray", fill=NA, size=1),
        legend.position="bottom")

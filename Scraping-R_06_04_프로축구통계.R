
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

## 프로축구 통계 @프리미어리그

library(tidyverse)
library(rvest)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr="localhost", port=4445, browserName="chrome")
remDr$open()
remDr$navigate("https://www.premierleague.com/stats/top/players/goals")

acceptCookies <- remDr$findElement(using="css selector", 
                                   value="button.js-accept-all-close")
acceptCookies$clickElement()

advertClose <- remDr$findElement(using="css selector", value="#advertClose")
advertClose$clickElement()

html <- remDr$getPageSource()[[1]]

stats <- read_html(html) %>% 
  html_elements(".topStatsLink") %>% 
  html_text() %>% 
  str_trim() %>% 
  str_to_title()
stats

seasons <- read_html(html) %>% 
  html_elements("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li") %>% 
  html_attr("data-option-name") %>% 
  .[-1]
seasons

positions <- read_html(html) %>% 
  html_elements("ul[data-dropdown-list=Position] > li") %>% 
  html_attr("data-option-name") %>% 
  .[-1]
positions

stats.target <- stats[c(1,2,5,8,10,15)]
stats.target

seasons.target <- seasons[str_detect(seasons, "2020|2019")]
seasons.target

stats.dropdown <- remDr$findElement(using="css selector",
                                    value=".dropDown.noLabel.topStatsFilterDropdown")
stats.dropdown$clickElement()

stats.elem <- remDr$findElement(using="link text", value=stats.target[[3]])
stats.elem$clickElement()

seasons.dropdown <- remDr$findElement(using="css selector",
                                      value=".current[data-dropdown-current=FOOTBALL_COMPSEASON]")
seasons.dropdown$clickElement()

seasons.elem <- remDr$findElement(using="css selector", 
                                  value=str_c("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li[data-option-name='", 
                                              seasons.target[[1]], "']"))
seasons.elem$clickElement()

positions.dropdown <- remDr$findElement(using="css selector",
                                        value=".current[data-dropdown-current=Position]")
positions.dropdown$clickElement()

positions.elem <- remDr$findElement(using="css selector", 
                                    value=str_c("ul[data-dropdown-list=Position] > li[data-option-name='", 
                                                positions[[4]], "']"))
positions.elem$clickElement()

html <- remDr$getPageSource()[[1]]

positions.data <- read_html(html) %>%
  html_table() %>% 
  .[[1]] %>% 
  select(-last_col()) %>% 
  mutate(Stat=as.character(Stat) %>% parse_number())
positions.data

check.stats <- read_html(html) %>% 
  html_element(".statsTableContainer") %>% 
  html_text()
check.stats

btn.next.exists <- read_html(html) %>% 
  html_element(".paginationNextContainer.inactive") %>% 
  html_text() %>% 
  is.na()
btn.next.exists

while (btn.next.exists) {
  btn.next <- remDr$findElement(using="css selector",
                                value=".paginationNextContainer")
  btn.next$clickElement()
  Sys.sleep(2)
  html <- remDr$getPageSource()[[1]]
  table.next <- read_html(html) %>% 
    html_table() %>% 
    .[[1]] %>% 
    select(-last_col()) %>% 
    mutate(Stat=as.character(Stat) %>% parse_number())
  positions.data <- bind_rows(positions.data, table.next)
  btn.next.exists <- read_html(html) %>% 
    html_element(".paginationNextContainer.inactive") %>% 
    html_text() %>% 
    is.na()
}
positions.data

btn.next.exists <- read_html(html) %>% 
  html_element(".paginationNextContainer.inactive") %>% 
  html_text() %>% 
  is.na()
btn.next.exists

positions.data <- positions.data %>% 
  rename(!!stats.target[[3]] := Stat) %>% 
  mutate(Season=seasons.target[[1]], Position=positions[[4]])
positions.data

stats.data <- vector("list", length(stats.target))
for (i in seq_along(stats.target)) {
  stats.dropdown <- remDr$findElement(using="css selector",
                                      value=".dropDown.noLabel.topStatsFilterDropdown")
  stats.dropdown$clickElement()
  Sys.sleep(2)
  stats.elem <- remDr$findElement(using="link text", value=stats.target[[i]])
  stats.elem$clickElement()
  Sys.sleep(2)
  seasons.data <- vector("list", length(seasons.target))
  for (j in seq_along(seasons.target)) {
    seasons.dropdown <- remDr$findElement(using="css selector",
                                          value=".current[data-dropdown-current=FOOTBALL_COMPSEASON]")
    seasons.dropdown$clickElement()
    Sys.sleep(2)
    seasons.elem <- remDr$findElement(using="css selector", 
                                      value=str_c("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li[data-option-name='", seasons.target[[j]], "']"))
    seasons.elem$clickElement()
    Sys.sleep(2)
    positions.data <- vector("list", length(positions))
    for (k in seq_along(positions)) {
      positions.dropdown <- remDr$findElement(using="css selector",
                                              value=".current[data-dropdown-current=Position]")
      positions.dropdown$clickElement()
      Sys.sleep(2)
      positions.elem <- remDr$findElement(using="css selector", 
                                          value=str_c("ul[data-dropdown-list=Position] > li[data-option-name='", positions[[k]], "']"))
      positions.elem$clickElement()
      Sys.sleep(2)
      html <- remDr$getPageSource()[[1]]
      check.stats <- read_html(html) %>% 
        html_element(".statsTableContainer") %>% 
        html_text()
      if (check.stats == "No stats are available for your search") next
      positions.data[[k]] <- read_html(html) %>% 
        html_table() %>% 
        .[[1]] %>% 
        select(-last_col()) %>% 
        mutate(Stat=as.character(Stat) %>% parse_number())
      btn.next.exists <- read_html(html) %>% 
        html_element(".paginationNextContainer.inactive") %>% 
        html_text() %>% 
        is.na()
      while (btn.next.exists) {
        btn.next <- remDr$findElement(using="css selector",
                                      value=".paginationNextContainer")
        btn.next$clickElement()
        Sys.sleep(2)
        html <- remDr$getPageSource()[[1]]
        table.next <- read_html(html) %>% 
          html_table() %>% 
          .[[1]] %>% 
          select(-last_col()) %>% 
          mutate(Stat=as.character(Stat) %>% parse_number())
        positions.data[[k]] <- bind_rows(positions.data[[k]], table.next)
        btn.next.exists <- read_html(html) %>% 
          html_element(".paginationNextContainer.inactive") %>% 
          html_text() %>% 
          is.na()
      }
      positions.data[[k]] <- positions.data[[k]] %>% 
        rename(!!stats.target[[i]] := Stat) %>% 
        mutate(Position=positions[[k]])
      go.top <- remDr$findElement(using="css selector", value="body")
      go.top$sendKeysToElement(list(key="home"))
      Sys.sleep(2)
    }
    positions.df <- reduce(positions.data, bind_rows)
    seasons.data[[j]] <- positions.df %>% 
      mutate(Season=seasons.target[[j]])
  }
  seasons.df <- reduce(seasons.data, bind_rows)
  stats.data[[i]] <- seasons.df
}

stats.data

save(stats.data, file="premier-stats.rda")
load("premier-stats.rda")

premier <- stats.data %>% 
  map(function(x) select(x, -Rank)) %>% 
  map(function(x)
    select(x, Season, Player, Nationality, Position, Club, everything())) %>% 
  reduce(full_join,
         by=c("Season", "Player", "Nationality", "Position", "Club")) %>% 
  mutate(across(.cols=where(is.numeric), .fns=replace_na, replace=0))

premier

premier.data <- premier %>% 
  filter(Season=="2020/21" & Position != "Goalkeeper" & Goals > 0)

# [그림 6-34]
library(hrbrthemes)
import_roboto_condensed()
library(scales)
windows(width=7.0, height=5.5)
ggplot(arrange(premier.data, desc(Passes)), 
       aes(x=`Minutes Played`, y=Goals+Assists, size=Passes, fill=Position)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_text(data=slice_max(premier.data, order_by=Goals+Assists, n=5), 
            aes(x=`Minutes Played`, y=Goals+Assists, 
                label=str_c(Player)), 
            position=position_jitter(width=1, height=2),
            size=2.5, color="orangered", fontface="bold") +
  scale_x_continuous(labels=comma) +
  scale_size(breaks=seq(500, 2500, 1000), range=c(0.1, 9), name="Passes") +
  scale_fill_viridis_d(option="D") +
  theme_ipsum() +
  labs(title="Total Goal Contribution vs. Minutes Played",
       subtitle="By position with the number of passes",
       caption="Source: Premier League") +
  xlab("Minutes Played") +
  ylab("Total Goal Contribution (Goals + Assists)") +
  guides(fill=guide_legend(order=1), size=guide_legend(order=2))

# [그림 6-35]
library(plotly)
premier.data2 <- premier.data %>% 
  mutate(text=str_c("Player: ", Player, "\nGoals: ", Goals, "\nAssists: ", Assists,
                    "\nMinutes Played: ", `Minutes Played`, "\nPasses: ", Passes))
p <- ggplot(arrange(premier.data2, desc(Passes)),
            aes(x=`Minutes Played`, y=Goals+Assists, 
                size=Passes, fill=Position, text=text)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_x_continuous(labels=comma) +
  scale_size(breaks=seq(500, 2500, 1000), range=c(0.1, 9), name="Passes") +
  scale_fill_viridis_d(option="D") +
  theme_ipsum() +
  labs(title="Total Goal Contribution vs. Minutes Played",
       subtitle="By position with the number of passes",
       caption="Source: Premier League") +
  xlab("Minutes Played") +
  ylab("Total Goal Contribution (Goals + Assists)") +
  theme(legend.position="none")
ggplotly(p, tooltip="text")

library(htmlwidgets)
pp <- ggplotly(p, tooltip="text")
saveWidget(pp, file="premier-stats.html")

library(pander)
openFileInOS("premier-stats.html")

library(webshot)
webshot(url="premier-stats.html", file="premier-stats.png")
openFileInOS("premier-stats.png")

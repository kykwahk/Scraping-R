
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

## 세계인구 @위키피디아

library(tidyverse)
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
read_html(url) %>%
  html_elements("table")

Sys.setlocale("LC_ALL", "English")
read_html(url) %>%
  html_table() %>% 
  .[[1]]
Sys.setlocale()

library(lubridate)
Sys.setlocale("LC_ALL", "English")
wiki.pop <- read_html(url) %>%
  html_table() %>% 
  .[[1]] %>% 
  rename_with(.fn=function(x) c("rank", "country", "region", "population", 
                                "percent", "date", "source", "notes")) %>% 
  mutate(rank=parse_number(rank), 
         country=str_replace_all(country, "\\[.*\\]", ""),
         region=str_replace_all(region, "\\[.*\\]", ""),
         population=parse_number(population), 
         percent=parse_number(percent), 
         date=dmy(date))
Sys.setlocale()

wiki.pop

save(wiki.pop, file="wiki-pop.rda")
load("wiki-pop.rda")

wiki.pop <- slice(wiki.pop, -1)
options("scipen")
options(scipen=999)
summary(wiki.pop$population)
options(scipen=0)

# [그림 5-16] 
p1 <- ggplot(wiki.pop, aes(x="", y=population)) +
  geom_violin(fill="darkslategray3", alpha=0.7, color="darkslategray3", width=0.8) + 
  geom_jitter(color="turquoise4", alpha=0.7, height=0, width=0.1, size=1) +
  geom_boxplot(width=0.1, color="black", fill="darkslategray1", alpha=0.5) +
  geom_rug(sides="l", color="dimgray") +
  scale_y_log10(breaks=c(10, 100, 1000, 10000, 100000, 1000000, 
                         10000000, 100000000, 1000000000), 
                labels=c("10", "100", "1,000", "10,000", "100,000", "1,000,000", 
                         "10,000,000", "100,000,000", "1,000,000,000")) +
  labs(x=NULL, y="Population (persons)", 
       title="Distribution") +
  theme_gray() +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=10),
        panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
wiki.pop20 <- slice_max(wiki.pop, order_by=population, n=20)
p2 <- ggplot(wiki.pop20, aes(x=reorder(country, population), y=population)) +
  geom_segment(aes(x=reorder(country, population),  
                   xend=reorder(country, population), 
                   y=0, yend=population), 
               color=ifelse(wiki.pop20$population > 100000000, "orange", "skyblue"), 
               size=1.2) +
  geom_point(color=ifelse(wiki.pop20$population > 100000000, "darkorange", 
                          "cornflowerblue"), size=4) +
  geom_text(aes(label=str_c(format(round(population/1000000), big.mark=","), "M")), 
            size=3, fontface="bold", color="dimgray", hjust=-0.2) +
  scale_y_sqrt(breaks=c(100000000, 500000000, 1000000000, 1500000000),
               labels=c("100M", "500 Million", "1 Billion", "1.5 Billion"),
               limits=c(0, 1600000000)) +
  coord_flip() +
  labs(x=NULL, y="", 
       title="Top 20 countries") +
  theme_light() +
  theme(plot.title=element_text(size=14),
        axis.text=element_text(size=10),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.ticks.y=element_blank())

windows(width=9.0, height=5.5)
library(patchwork)
p1 + p2 + 
  plot_annotation(title="World Population",
                  caption="Source: Wikipedia",
                  theme=theme(plot.title=element_text(face="bold", size=18),
                              plot.caption=element_text(size=12))) +
  plot_layout(widths=c(1, 3))

install.packages("rnaturalearth")
library(rnaturalearth)
map.data <- ne_countries(scale="medium", returnclass="sf")
class(map.data)
names(map.data)
head(map.data[c("name", "geometry")])
nrow(map.data)

sort(setdiff(wiki.pop$country, map.data$name))
sort(setdiff(map.data$name, wiki.pop$country))

library(countrycode)
countrycode("Korea", origin="country.name", destination="iso3c")
countrycode("KOR", origin="iso3c", destination="country.name")

tail(wiki.pop$country, 3)
countrycode(tail(wiki.pop$country, 3), origin="country.name", destination="iso3c")

wiki.pop$country2 <- wiki.pop$country %>%
  str_replace_all("\\(.*?\\)", "") %>%
  str_replace_all("\\s{2,}", " ") %>%
  str_trim
tail(wiki.pop$country2, 3)
countrycode(tail(wiki.pop$country2, 3), origin="country.name", destination="iso3c")

filter(wiki.pop, 
       is.na(countrycode(wiki.pop$country2, origin="country.name", destination="iso3c")))

wiki.pop$iso3c <- countrycode(wiki.pop$country2, 
                              origin="country.name", destination="iso3c",
                              custom_match=c("Micronesia"="FSM", "Saint Martin"="MAF"))
wiki.pop

countrycode(setdiff(wiki.pop$iso3c, map.data$iso_a3),
            origin="iso3c", destination="country.name")
countrycode(setdiff(map.data$iso_a3, wiki.pop$iso3c),
            origin="iso3c", destination="country.name")

map.data <- left_join(map.data, wiki.pop, by=c("iso_a3"="iso3c"), 
                      keep=TRUE, na_matches="never")
head(map.data[c("name", "geometry", "population")])

# [그림 5-17] 
windows(width=7.0, height=4.0)
mybreaks <- c(50000000, 100000000, 500000000, 1000000000)
mylabels <- c("50M", "100M", "500 Million", "1 Billion")
ggplot(map.data) +
  geom_sf(aes(fill=population), color="dimgray") +
  coord_sf(xlim=c(-180, 180), ylim=c(-60, 90), expand=FALSE) +
  scale_fill_viridis_c(option="viridis", trans="sqrt",
                       breaks=mybreaks, labels=mylabels) +
  labs(title="World Population",
       subtitle="Worldwide",
       caption="Source: Wikipedia") +
  theme_void() +
  guides(color=guide_legend(),
         fill=guide_colorbar(barwidth=20, barheight=0.7)) +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_blank(), 
        legend.text=element_text(size=8))

library(sf)
country.coord <- st_coordinates(st_centroid(st_make_valid(map.data$geometry)))
head(country.coord)

map.data <- cbind(map.data, country.coord)
head(map.data[c("name", "geometry", "population", "X", "Y")])

# [그림 5-18] 
windows(width=7.0, height=6.0)
mybreaks <- c(50000000, 100000000, 500000000, 1000000000)
mylabels <- c("50M", "100M", "500 Million", "1 Billion")
library(ggspatial)
ggplot(map.data) +
  geom_sf(aes(fill=population), color="dimgray") +
  annotation_scale(location="tr", width_hint=0.5) +
  annotation_north_arrow(location="tr", pad_x=unit(2.5, "cm"), pad_y=unit(1.0, "cm"),
                         style=north_arrow_nautical) +
  coord_sf(xlim=c(89, 153), ylim=c(-14, 30), expand=FALSE) +
  geom_text(aes(x=X, y=Y, label=name), color="darkorange", 
            size=3, fontface="bold") +
  scale_fill_viridis_c(option="viridis", trans="sqrt",
                       breaks=mybreaks, labels=mylabels) +
  labs(title="World Population",
       subtitle="Southeast Asia",
       caption="Source: Wikipedia",
       x="Longitude", y="Latitude") +
  theme_bw() +
  guides(color=guide_legend(),
         fill=guide_colorbar(barwidth=20, barheight=0.7)) +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_blank(), 
        legend.text=element_text(size=8),
        panel.grid.major=element_line(color="gray70", linetype="dashed", size=0.5), 
        panel.background=element_rect(fill="aliceblue"))

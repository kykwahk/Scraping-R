
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

#################
## 제4장 XPath ##
#################

##############
## 4.5 사례 ##
##############

## 국가별 가처분 소득 @NationMaster

library(httr)
library(XML)
url <- "https://www.nationmaster.com/country-info/stats/Cost-of-living/Average-monthly-disposable-salary/After-tax"
html <- GET(url)
html.parsed <- htmlParse(html)
html.tab <- readHTMLTable(doc=html.parsed)

str(html.tab)
nation.salary <- html.tab[[1]]
head(nation.salary)

nation.salary <- readHTMLTable(html.parsed, which=1)
str(nation.salary)

library(tidyverse)
nation.salary <- nation.salary %>% 
  rename(rank="#") %>% 
  rename_with(tolower) %>% 
  mutate(rank=parse_number(rank), amount=parse_number(amount)) %>% 
  select(-5) %>% 
  as_tibble()
nation.salary

summary(nation.salary$amount)

save(nation.salary, file="nation-salary.rda")
load("nation-salary.rda")

# [그림 4-50]
p1 <- ggplot(nation.salary, aes(x="", y=amount)) +
  geom_violin(fill="paleturquoise", alpha=0.7, color="deepskyblue", width=0.8) + 
  geom_boxplot(width=0.1, color="dodgerblue4", fill="aliceblue", alpha=0.5) +
  geom_jitter(color="mediumblue", alpha=0.5, height=0, width=0.2) +
  geom_rug(sides="l", color="black") +
  scale_y_continuous(breaks=c(1000, 2000, 3000, 4000, 5000, 6000),
                     labels=c("$1,000", "$2,000", "$3,000", 
                              "$4,000", "$5,000", "$6,000")) +
  labs(x="", y=NULL, 
       title="Average Monthly Salary",
       subtitle="Distribution of worldwide salary",
       caption="") +
  theme_gray() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
nation.salary20 <- bind_rows(slice_max(nation.salary, order_by=amount, n=10), 
                             slice_min(nation.salary, order_by=amount, n=10))
p2 <- ggplot(nation.salary20, aes(x=reorder(country, amount), y=amount)) +
  geom_segment(aes(x=reorder(country, amount),  xend=country, 
                   y=median(nation.salary$amount), yend=amount), 
               color="skyblue", size=1) +
  geom_point(color="royalblue", size=3, fill=alpha("cornflowerblue", 0.3), 
             alpha=0.7, shape=21, stroke=2) +
  geom_hline(yintercept=median(nation.salary$amount), lty="dotted", color="blue") +
  geom_text(aes(x=0, y=median(nation.salary$amount), vjust=-0.5, 
                label=paste0("Median $", round(median(nation.salary$amount)))), 
            size=3, color="royalblue") +
  scale_y_continuous(breaks=c(0, 500, 1000, 2000, 4000, 6000),
                     labels=c("0", "$500", "$1,000", "$2,000", "$4,000", "$6,000")) +
  coord_flip() +
  labs(x=NULL, y="", 
       title="",
       subtitle="Top 10 and bottom 10 countries",
       caption="Source: NationMaster") +
  theme_light() +
  theme(axis.text=element_text(face="bold"),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank(),
        axis.ticks.y=element_blank()) 
windows(width=9.0, height=5.5)
library(gridExtra)
grid.arrange(p1, p2, ncol=2, widths=c(0.3, 0.7))

library(tidyverse)
library(maps)
map.data <- map_data(map="world")
head(map.data)

setdiff(nation.salary$country, map.data$region)

from.country <- c("United States", "British Virgin Islands", "The Bahamas", "United Kingdom", 
                  "Trinidad and Tobago", "Republic of Macedonia", "Burma")
to.country <- c("USA", "Virgin Islands", "Bahamas", "UK", "Trinidad", "Macedonia", "Myanmar")
nation.salary$country <- replace(x=nation.salary$country, 
                                 list=(nation.salary$country %in% from.country),
                                 values=to.country)

map.data <- left_join(map.data, nation.salary, by=c("region"="country"))
head(map.data)

# [그림 4-51]
windows(width=8.0, height=5.5)
salary.plot <- ggplot(data=map.data, mapping=aes(x=long, y=lat)) + 
  geom_polygon(aes(fill=amount, group=group), color="white") +
  coord_fixed(xlim=c(-180, 180), ylim=c(-55, 90), ratio=1.3) +
  scale_fill_viridis_c(name=NULL, option="plasma",
                       breaks=c(1000, 2000, 3000, 4000, 5000, 6000), 
                       labels=c("$1,000", "$2,000", "$3,000", 
                                "$4,000", "$5,000", "$6,000")) +
  labs(title="Average Monthly Salary",
       subtitle="Worldwide",
       caption="Source: NationMaster") +
  theme_void() +
  guides(color=guide_legend(), 
         fill=guide_colorbar(barwidth=15, barheight=0.7)) +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.text=element_text(size=8))
salary.plot

cnames <- aggregate(cbind(long, lat) ~ region, data=map.data, FUN=mean)
head(cnames)

# [그림 4-52]
windows(width=6.0, height=7.0)
salary.plot +
  coord_fixed(xlim=c(-10, 40),  ylim=c(35, 70), ratio=1.3) +
  geom_text(data=cnames, aes(x=long, y=lat, label=region), color="darkorange", 
            size=2.5, fontface="bold", check_overlap=TRUE) +
  labs(subtitle="Europe")

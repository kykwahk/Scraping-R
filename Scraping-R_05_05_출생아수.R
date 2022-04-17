
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

## 출생아수 @미국사회보장국

library(tidyverse)
library(rvest)
url <- "https://www.ssa.gov/oact/babynames/numberUSbirths.html"
read_html(url) %>%
  html_elements("table")
read_html(url) %>%
  html_table() %>% 
  .[[1]] 

ssa.birth <- read_html(url) %>%
  html_table() %>% 
  .[[1]] %>% 
  rename_with(.fn=function(x) c("year", "male", "female", "total")) %>% 
  mutate(across(where(is.character), parse_number))
ssa.birth

save(ssa.birth, file="ssa-birth.rda")
load("ssa-birth.rda")

ssa.birth.long <- ssa.birth %>% 
  pivot_longer(cols=male:total, names_to="gender", values_to="number") 
ssa.birth.long

# [그림 5-22] 
windows(width=8.0, height=5.0)
ssa.birth.plot <- ggplot(ssa.birth.long, aes(x=year, y=number, color=gender)) +
  geom_line(size=1.5) +
  scale_x_continuous(breaks=seq(min(ssa.birth.long$year), max(ssa.birth.long$year), 20)) +
  scale_y_continuous(breaks=c(0, 1000000, 2000000, 3000000, 4000000),
                     labels=c("0", "1M", "2M", "3M", "4M")) +
  scale_color_discrete(labels=c("Female", "Male", "Total")) +
  labs(x="", y="Number of Babies", 
       title="Number of Births in US",
       subtitle="Number of social security card holders born in US",
       caption="Source: Social Security Administration of US") +
  theme_light() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom")
ssa.birth.plot

search()
detach(package:tm)
detach(package:NLP)
search()

# [그림 5-23] 
ssa.birth.plot + 
  annotate("rect", xmin=1946, xmax=1965, ymin=0, ymax=Inf, fill="orange", alpha=0.3) +
  annotate("segment", x=1946, xend=1965, y=0, yend=0,
           arrow=arrow(ends="both", angle=45, length=unit(0.2,"cm"), type="open"),
           color="dimgray", size=1) +
  geom_text(aes(x=1955.5, y=500000, label="Baby Boom\n1946-1965"),
            color="dimgray", fontface="bold")

ssa.birth.plot + 
  ggplot2::annotate("rect", xmin=1946, xmax=1965, ymin=0, ymax=Inf, fill="orange", alpha=0.3) +
  ggplot2::annotate("segment", x=1946, xend=1965, y=0, yend=0,
                    arrow=arrow(ends="both", angle=45, length=unit(0.2,"cm"), type="open"),
                    color="dimgray", size=1) +
  geom_text(aes(x=1955.5, y=500000, label="Baby Boom\n1946-1965"),
            color="dimgray", fontface="bold")

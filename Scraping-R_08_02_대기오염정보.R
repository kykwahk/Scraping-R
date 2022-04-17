
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제8장. API ##
################

##############
## 8.4 사례 ##
##############

## 대기오염 정보 @공공데이터포털/한국환경공단

library(glue)
url <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?numOfRows={row}&pageNo={page}&itemCode={item}&dataGubun={time}&searchCondition={period}&ServiceKey={key}"
row <- "100"
page <- "1"
item <- "PM25"
time <- "DAILY"
period <- "MONTH"
key <- "abcxyz123456789"
glue(url)

library(XML)
library(xml2)
xml <- read_xml(glue(url))
data <- as_list(xml)
str(data, max.level=5)

library(tidyverse)
air <- data$response$body$items %>% 
  map(unlist) %>% 
  map(enframe) %>% 
  map(~pivot_wider(data=., names_from=name, values_from=value)) %>% 
  reduce(bind_rows)

air

save(air, file="dataportal-air.rda")
load("dataportal-air.rda")

library(lubridate)
air <- air %>% 
  mutate(dataTime=ymd(dataTime)) %>% 
  mutate(across(where(is.character) & !itemCode, as.numeric)) %>% 
  select(itemCode, dataTime, dataGubun, everything())
air

air.long <- pivot_longer(data=air, cols=4:20, names_to="city", values_to="value")
air.long

# [그림 8-9]
windows(width=7.0, height=5.5)
ggplot(filter(air.long, city=="seoul"), aes(x=dataTime, y=value)) +
  geom_ribbon(aes(ymin=value-10, ymax=value+10), fill="gray90") +
  geom_line(color="coral", size=1.5) +
  geom_point(shape=21, fill="gray50", size=6) +
  geom_text(aes(label=value), color="white", size=2, fontface="bold") +
  scale_x_date(date_labels="%m-%d", date_breaks="3 days") +
  labs(x="", y="PM2.5", 
       title="대기오염 정보",
       subtitle="서울시 미세먼지 농도",
       caption="출처: 한국환경공단(공공데이터포털)") +
  theme_light() +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.border=element_blank())

air.long.city <- filter(air.long, city %in% c("seoul", "busan", "daegu", "gwangju")) %>% 
  mutate(city=factor(.$city, 
                     levels=c("seoul", "busan", "daegu", "gwangju"),
                     labels=c("서울", "부산", "대구", "광주")))
air.long.city

# [그림 8-10]
library(ggalt)
library(hrbrthemes)
windows(width=7.0, height=5.5)
ggplot(air.long.city, aes(x=dataTime, y=value, color=city)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_encircle(data=filter(air.long.city, value >= 36), 
                aes(x=dataTime, y=value), inherit.aes=FALSE,
                s_shape=0.5, expand=0.04, size=1,  
                color="darkorange", fill="orange", alpha=0.2) +
  geom_hline(yintercept=36, lty="dashed", color="royalblue", size=0.5) +
  annotate("text", x=min(air.long.city$dataTime)-2, y=36, 
           label="36", color="royalblue", size=3) +
  coord_cartesian(xlim=c(min(air.long.city$dataTime), max(air.long.city$dataTime)), 
                  clip="off") +
  scale_x_date(date_labels="%m-%d", date_breaks="3 days") +
  labs(x="", y="PM2.5", 
       title="대기오염 정보",
       subtitle="서울/부산/대구/광주 미세먼지 농도",
       caption="출처: 한국환경공단(공공데이터포털)") +
  theme_ipsum(plot_title_size=18,
              plot_title_face="bold",
              subtitle_size=15,
              caption_size=12,
              caption_face="plain",
              axis_title_size=12,
              axis_text_size=10,
              plot_margin=margin(10, 10, 10, 10)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        legend.text=element_text(size=12)) 


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

## 아파트 매매 실거래 정보 @공공데이터포털/국토교통부

library(glue)
url <- "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD={loc}&DEAL_YMD={date}&serviceKey={key}"
loc <- "11110"
date <- "202109"
key <- "abcxyz123456789"
glue(url)

library(XML)
library(xml2)
xml <- read_xml(glue(url))
xml.parsed <- xmlParse(xml)

items <- xpathSApply(xml.parsed, "//item")
length(items)
items[[1]]

data <- as_list(xml)
str(data, max.level=5)
length(data$response$body$items)

library(tidyverse)
apt <- data$response$body$items %>% 
  map(unlist) %>% 
  map(enframe) %>% 
  map(~pivot_wider(data=., names_from=name, values_from=value)) %>% 
  reduce(bind_rows)

apt

url <- "https://www.mois.go.kr/cmm/fms/FileDown.do?atchFileId=FILE_00106215lHoK9xZ&fileSn=0"
local.copy <- "loccode.zip"
download.file(url, local.copy, mode="wb")
library(readxl)
area.code <- read_excel(unzip(zipfile=local.copy, "KIKcd_B.20220103.xlsx"), sheet=1)
area.code

save(area.code, file="dataportal-area-code.rda")
load("dataportal-area-code.rda")

seoul.code <- area.code %>% 
  mutate(법정동코드s=str_sub(법정동코드, 1, 5)) %>% 
  distinct(법정동코드s, .keep_all=TRUE) %>% 
  select(법정동코드s, 시도명, 시군구명) %>% 
  drop_na() %>% 
  filter(시도명=="서울특별시")

seoul.code
seoul.code$시군구명

makeYearMonth <- function(from, to) {
  from <- as.Date(str_c(from, "01"), "%Y%m%d")
  to <- as.Date(str_c(to, "01"), "%Y%m%d")
  yearmonth <- format(seq(from=from, to=to, by="month"), "%Y%m%d") %>% 
    as.character() %>% 
    str_sub(start=1, end=6)
}
yearmonth <- makeYearMonth(from="201811", to="202201")
yearmonth

library(glue)
url <- "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD={loc}&DEAL_YMD={date}&serviceKey={key}"
key <- "abcxyz123456789"
urls <- c()
for (loc in seoul.code$법정동코드s) {
  for (date in yearmonth) {
    url.each <- glue(url)
    urls <- c(urls, url.each)
  }
}
length(urls)
urls

library(tidyverse)
library(XML)
library(xml2)
apt.trans <- tibble()
for (url in urls) {
  xml <- read_xml(url)
  data <- as_list(xml)
  apt <- data$response$body$items %>% 
    map(unlist) %>% 
    map(enframe) %>% 
    map(~pivot_wider(data=., names_from=name, values_from=value)) %>% 
    reduce(bind_rows)
  apt.trans <- bind_rows(apt.trans, apt)
  Sys.sleep(sample(10, 1)*0.1)
}
apt.trans

save(apt.trans, file="dataportal-apt-trans.rda")
load("dataportal-apt-trans.rda")

library(lubridate)
apt.trans <- apt.trans %>% 
  mutate(거래금액=parse_number(거래금액)*10, 
         건축년도=as.numeric(건축년도),
         계약연월일=ymd(str_c(년, str_pad(월, 2, pad=0), str_pad(일, 2, pad=0))),
         전용면적=as.numeric(전용면적),
         층=as.numeric(층)) %>% 
  left_join(seoul.code, by=c("지역코드"="법정동코드s"))

apt.trans
View(apt.trans[apt.trans$시군구명=="성북구",])

apt.agg <- apt.trans %>%
  mutate(계약연월=floor_date(계약연월일, unit="month")) %>%
  group_by(지역코드, 시군구명, 계약연월) %>%
  summarise(평균거래금액=mean(거래금액, na.rm=TRUE), 거래건수=n()) %>%
  filter(계약연월 == max(계약연월)) %>% 
  ungroup()
apt.agg

# 지오서비스 지도 데이터 http://www.gisdeveloper.co.kr/?p=2332
library(raster)
map.data <- shapefile("SHP-Geoservice/TL_SCCO_SIG.shp")
map.data
class(map.data)

# [그림 8-11]
windows(width=5.5, height=7.0)
ggplot(data=map.data, mapping=aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), fill="dimgray", color="white") + 
  theme_void()

map.data <- spTransform(map.data, 
                        CRSobj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
map.data
head(map.data, 3)

library(broom)
map.data <- tidy(map.data, region="SIG_CD")
map.data

map.data.seoul <- left_join(apt.agg, map.data, by=c("지역코드"="id")) 
map.data.seoul

area.center <- aggregate(cbind(long, lat, 평균거래금액, 거래건수) ~ 시군구명, 
                         data=map.data.seoul, FUN=mean)
head(area.center, 3)

# [그림 8-12]
library(scales)
windows(width=7.0, height=5.5)
ggplot(data=map.data.seoul, mapping=aes(x=long, y=lat)) +
  geom_polygon(aes(fill=거래건수, group=group), color="white") +
  geom_text(data=area.center, 
            aes(x=long, y=lat, 
                label=str_c(시군구명, "\n", format(거래건수, big.mark=","))), 
            color="darkgoldenrod4", size=3, fontface="bold") +
  scale_fill_distiller(name=NULL, palette="Blues", direction=1, label=comma) +
  labs(title="아파트 매매 실거래",
       subtitle="서울시 아파트 거래량",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_void() +
  guides(color=guide_legend(), 
         fill=guide_colorbar(barwidth=15, barheight=0.7)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        legend.position="bottom",
        legend.text=element_text(size=8))

# 국가공간정보포털 지도 데이터 http://data.nsdi.go.kr/dataset/15144
map.data.seoul <- shapefile("SHP-NSDI/LARD_ADM_SECT_SGG_11.shp")
map.data.seoul
head(map.data.seoul, 3)

library(broom)
map.data.seoul <- tidy(map.data.seoul, region="ADM_SECT_C")
map.data.seoul

map.data.seoul <- left_join(apt.agg, map.data.seoul, by=c("지역코드"="id")) 
map.data.seoul
area.center <- aggregate(cbind(long, lat, 평균거래금액, 거래건수) ~ 시군구명, 
                         data=map.data.seoul, FUN=mean)
head(area.center, 3)

# [그림 8-13]
library(scales)
windows(width=7.0, height=5.5)
ggplot(data=map.data.seoul, mapping=aes(x=long, y=lat)) +
  geom_polygon(aes(fill=평균거래금액/1000, group=group), color="white") +
  geom_text(data=area.center, 
            aes(x=long, y=lat, 
                label=str_c(시군구명, "\n", format(round(평균거래금액/1000), big.mark=","))), 
            color="darkcyan", size=3, fontface="bold") +
  scale_fill_distiller(name=NULL, palette="Reds", direction=1, label=comma) +
  labs(title="아파트 매매 실거래",
       subtitle="서울시 아파트 매매가(백만원)",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_void() +
  guides(color=guide_legend(), 
         fill=guide_colorbar(barwidth=15, barheight=0.7)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        legend.position="bottom",
        legend.text=element_text(size=8))

map.data.seoul <- shapefile("SHP-NSDI/LARD_ADM_SECT_SGG_11.shp")
map.data.seoul <- st_as_sf(map.data.seoul)
map.data.seoul

library(sf)
map.data.seoul <- st_read("SHP-NSDI/LARD_ADM_SECT_SGG_11.shp")
class(map.data.seoul)
map.data.seoul

map.data.seoul <- left_join(map.data.seoul, apt.agg, by=c("ADM_SECT_C"="지역코드")) 
map.data.seoul

area.center <- st_coordinates(st_centroid(st_make_valid(map.data.seoul$geometry)))
head(area.center, 3)
map.data.seoul <- cbind(map.data.seoul, area.center)
names(map.data.seoul)

# [그림 8-14]
library(ggspatial)
library(scales)
windows(width=7.0, height=7.0)
ggplot(map.data.seoul) +
  geom_sf(aes(fill=평균거래금액/1000), color="dimgray") +
  annotation_scale(location="tl", width_hint=0.3) +
  annotation_north_arrow(location="tl", pad_x=unit(1.0, "cm"), pad_y=unit(1.0, "cm"),
                         style=north_arrow_fancy_orienteering) +
  geom_text(aes(x=X, y=Y, 
                label=str_c(시군구명, "\n", format(round(평균거래금액/1000), big.mark=","))),
            color="cadetblue", size=3, fontface="bold") +
  scale_fill_viridis_c(option="plasma", label=comma) +
  labs(title="아파트 매매 실거래",
       subtitle="서울시 아파트 매매가(백만원)",
       caption="출처: 국토교통부(공공데이터포털)",
       x="경도(longitude)", y="위도(latitude)") +
  theme_bw() +
  guides(color=guide_legend(),
         fill=guide_colorbar(barwidth=15, barheight=0.7)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        legend.position="bottom",
        legend.title=element_blank(), 
        legend.text=element_text(size=8),
        panel.grid.major=element_line(color="gray70", linetype="dashed", size=0.5), 
        panel.background=element_rect(fill="honeydew"))

library(lubridate)
apt.agg <- apt.trans %>%
  mutate(계약연월=floor_date(계약연월일, unit="month")) %>%
  group_by(계약연월) %>%
  summarise(평균거래금액=mean(거래금액, na.rm=TRUE), 거래건수=n()) %>%
  ungroup()

apt.agg

# [그림 8-15]
library(scales)
library(hrbrthemes)
windows(width=9.0, height=6.0)
ggplot(apt.agg, aes(x=계약연월, y=평균거래금액/1000)) +
  geom_point(color="darkorange", size=2) +
  geom_smooth(method="loess", color="firebrick1", size=1.5) +
  scale_x_date(date_labels="%Y-%m", date_breaks="3 months", expand=c(0, 0)) +
  scale_y_continuous(labels=comma) +
  labs(x="", y="매매가(백만원)",
       title="아파트 매매 실거래",
       subtitle="서울시 아파트 매매가 추이",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_ipsum(axis_title_size=12,
              plot_margin=margin(10, 10, 10, 10)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.text.x=element_text(size=8))

search()
detach(package:raster)
search()

library(xts)
apt.agg.xts <- apt.agg %>% 
  select(계약연월, 거래건수) %>% 
  xts(x=.$거래건수, order.by=.$계약연월)
apt.agg.xts <- apt.agg %>% 
  dplyr::select(계약연월, 거래건수) %>% 
  xts(x=.$거래건수, order.by=.$계약연월)
head(apt.agg.xts, 3); tail(apt.agg.xts, 3)

# [그림 8-16]
library(dygraphs)
p <- dygraph(apt.agg.xts, main="서울시 아파트 거래량 추이", ylab="거래량(건)") %>% 
  dyAxis("x", drawGrid=FALSE) %>% 
  dyOptions(fillGraph=TRUE, gridLineColor="lightblue", colors="royalblue") 
p

library(htmlwidgets)
library(webshot)
saveWidget(p, file="dataportal-apt-trans.html")
webshot(url="dataportal-apt-trans.html", file="dataportal-apt-trans.png",
        vwidth=810, vheight=540)

library(pander)
openFileInOS("dataportal-apt-trans.html")
openFileInOS("dataportal-apt-trans.png")

library(lubridate)
apt.agg <- apt.trans %>%
  mutate(계약연월=floor_date(계약연월일, unit="month")) %>%
  group_by(시군구명, 계약연월) %>%
  summarise(평균거래금액=mean(거래금액, na.rm=TRUE), 거래건수=n()) %>%
  ungroup()
apt.agg

# [그림 8-17]
library(scales)
windows(width=9.0, height=9.0)
ggplot(apt.agg, aes(x=계약연월, y=평균거래금액/1000)) +
  geom_point(color="darkturquoise") +
  geom_smooth(method="loess", color="tomato") +
  scale_x_date(date_labels="%y-%m", date_breaks="years") +
  scale_y_continuous(labels=comma) +
  facet_wrap(~ 시군구명, scale="free_y", ncol=5) +
  labs(x="", y="매매가(백만원)",
       title="아파트 매매 실거래",
       subtitle="서울시 아파트 매매가 추이",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_bw() +
  theme(strip.background=element_blank(), 
        strip.text=element_text(color="dimgray", face="bold"),
        plot.title=element_text(face="bold", size=20),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.text.x=element_text(size=7))

apt.ts <- ts(data=filter(apt.agg, 시군구명=="강남구")$평균거래금액/1000, 
             start=c(2018, 11), frequency=12)
class(apt.ts)

apt.ts

library(forecast)
apt.ets <- ets(apt.ts)
apt.ets

apt.ets.pred <- forecast(apt.ets, h=12)

apt.ets.pred

# [그림 8-18]
library(ggfortify)
library(scales)
library(hrbrthemes)
windows(width=9.0, height=6.0)
autoplot(apt.ets.pred, ts.colour="darkorange", ts.size=1.5, 
         predict.colour="red", predict.linetype="dashed",
         predict.size=1.2, conf.int.fill="tomato") +
  scale_y_continuous(labels=comma) +
  labs(x="", y="매매가(백만원)",
       title="아파트 매매 실거래",
       subtitle="서울시 강남구 아파트 매매가 추이와 예측",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_ipsum(axis_title_size=12,
              plot_margin=margin(10, 10, 10, 10)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.text.x=element_text(size=8))

# [그림 8-19]
data <- data.frame(price=as.numeric(apt.ts), time=as.numeric(time(apt.ts)))
library(scales)
library(hrbrthemes)
windows(width=9.0, height=6.0)
ggplot(data=data, aes(x=time, y=price)) +
  geom_forecast(h=12, lty="dashed", color="mistyrose") +
  geom_point(color="salmon", size=2) +
  geom_smooth(method="loess", color="firebrick1", size=1.2) +
  scale_y_continuous(labels=comma) +
  labs(x="", y="매매가(백만원)",
       title="아파트 매매 실거래",
       subtitle="서울시 강남구 아파트 매매가 추이와 예측",
       caption="출처: 국토교통부(공공데이터포털)") +
  theme_ipsum(axis_title_size=12,
              plot_margin=margin(10, 10, 10, 10)) +
  theme(plot.title=element_text(face="bold", size=18),
        plot.subtitle=element_text(size=15),
        plot.caption=element_text(size=12),
        axis.text.x=element_text(size=8))

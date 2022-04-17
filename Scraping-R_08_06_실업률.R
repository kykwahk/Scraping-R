
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

## 실업률 @세계은행

library(wbstats)
str(wb_cachelist, max.level=1)

names(wb_cachelist$countries)
names(wb_cachelist$indicators)

wb_search(pattern="unemployment, total")

wb.data <- wb_data(country=c("KR", "JP", "EU", "US"), indicator="SL.UEM.TOTL.ZS", 
                   start_date=2011, end_date=2020)
wb.data

save(wb.data, file="worldbank-unemployment.rda")
load("worldbank-unemployment.rda")

# [그림 8-35]
library(ggplot2)
library(ggrepel)
library(scales)
windows(width=7.0, height=5.5)
ggplot(wb.data, aes(x=date, y=SL.UEM.TOTL.ZS/100, color=country, fill=country)) + 
  geom_line(size=1) +
  geom_point(shape=21, color="slategray", size=3, stroke=2) +
  geom_text_repel(aes(label=paste0(sprintf("%.1f", round(SL.UEM.TOTL.ZS, 1)), "%")), 
                  color="dimgray", fontface="bold", size=3,
                  arrow=arrow(length=unit(0.1, "cm")),
                  box.padding=0.5) + 
  scale_x_continuous(breaks=seq(min(wb.data$date), max(wb.data$date), 1)) +
  scale_y_continuous(limit=c(0, max(wb.data$SL.UEM.TOTL.ZS)/100), 
                     breaks=seq(0, max(wb.data$SL.UEM.TOTL.ZS)/100, 0.02),
                     labels=percent) +
  labs(x="", y=NULL,
       title="Unemployment Rates",
       subtitle="Four regions' unemployment rates over time",
       caption="Source: World Bank") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line=element_line(color="gray", size=1),
        panel.grid.minor=element_blank(),
        legend.position="bottom", 
        legend.title=element_blank())

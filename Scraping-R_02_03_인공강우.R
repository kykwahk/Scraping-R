
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

##########################
## 제2장 파일 다운로드  ##
##########################

##############
## 2.2 사례 ##
##############

## 인공강우 @StatLib

cloud.seeding <- scan("http://lib.stat.cmu.edu/datasets/cloud",
                      skip=15,
                      nlines=108,
                      what=list(period=character(),
                                seeded=character(),
                                season=character(),
                                te=numeric(),
                                tw=numeric(),
                                nc=numeric(),
                                sc=numeric(),
                                nwc=numeric()))

save(cloud.seeding, file="statlib-cloud.rda")
load("statlib-cloud.rda")

str(cloud.seeding)
cloud.seeding
cloud.seeding$season

cloud.seeding <- as.data.frame(cloud.seeding)
cloud.seeding

summary(cloud.seeding[4:8])
colMeans(subset(cloud.seeding, select=c(te, tw), 
                subset=(seeded=="S" & season=="AUTUMN")))
colMeans(subset(cloud.seeding, select=c(te, tw), 
                subset=(seeded=="U" & season=="AUTUMN")))

# [그림 2-3]
library(ggplot2)
windows(width=7.0, height=5.5)
ggplot(cloud.seeding, aes(x=factor(season,
                                   levels=c("SPRING",
                                            "SUMMER",
                                            "AUTUMN",
                                            "WINTER")),
                          y=te, fill=seeded)) +
  geom_boxplot() +
  labs(x="Season", y="Rainfall of East Target Area",
       title="Cloud Seeding Experiment",
       subtitle="Comparison by season",
       caption="Source: StatLib",
       fill="Seeded?")

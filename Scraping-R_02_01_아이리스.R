
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

## 아이리스 @UCI

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.csv(url, header=FALSE)
head(iris)

save(iris, file="uci-iris.rda")
load("uci-iris.rda")

colnames(iris) <- c("sepal.length", "sepal.width", 
                    "petal.length", "petal.width", "species")

summary(iris)
aggregate(iris[1:4], list(species=iris$species), mean)

# [그림 2-1]
library(ggplot2)
windows(width=7.0, height=5.5)
ggplot(iris, aes(x=petal.length, y=petal.width, color=species, shape=species)) +
  geom_point(position="jitter") +
  scale_color_discrete(labels=c("Setosa", "Versicolor", "Virginica")) +
  scale_shape_discrete(labels=c("Setosa", "Versicolor", "Virginica")) +
  labs(x="Petal Length (cm)", y="Petal Width (cm)",
       title="Iris",
       subtitle="Distribution of petal length and width by iris species",
       caption="Source: UCI Machine Learning Repository") +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_blank())

download.file(url, destfile="iris.csv")
iris <- read.csv("iris.csv", header=FALSE)
head(iris)

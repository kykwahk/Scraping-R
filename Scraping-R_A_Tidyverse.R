
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

#####################
## 부록. Tidyverse ##
#####################

#############################
## A.1 팩터와 데이터프레임 ##
#############################

## forcats 패키지

food <- factor(c("Vegetables", "Fruits", "Vegetables", "Grains", "Fruits", 
                 "Vegetables", "Dairy", "Fruits", "Proteins", "Fruits"))
food

library(forcats)
fct_relevel(food, "Fruits", "Vegetables", "Grains", "Proteins", "Dairy")

fct_relevel(food, "Proteins")

fct_relevel(food, "Proteins", after=2)

food
value <- c(1000, 1500, 1200, 700, 2000, 
           2000, 1350, 2500, 15000, 3000)
fct_reorder(food, .x=value)

fct_reorder(food, .x=value, .desc=TRUE)

## tibble 패키지

library(tibble)
v1 <- c("A001", "A002", "A003")
v2 <- c("Mouse", "Keyboard", "USB")
v3 <- c(30000, 90000, 50000)
product <- tibble(id=v1, name=v2, price=v3)
product

tbl <- tibble(id=c(1, 2, 3),
              data=list(tibble(x=1, y=2),
                        tibble(x=4:5, y=6:7),
                        tibble(x=10)))
tbl
tbl$data
tbl$data[[2]]

str(iris)
as_tibble(iris)

library(Lahman)
str(Batting)
head(Batting, 3)

Batting.tbl <- as_tibble(Batting)
Batting.tbl
class(Batting.tbl)

Batting.df <- as.data.frame(Batting.tbl)
class(Batting.df)

#######################
## A.2 텍스트와 날짜 ##
#######################

## stringr 패키지

library(stringr)
string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")
str_detect(string=string, pattern="data")

str_detect(string, "DATA")
str_detect(string, fixed("DATA", ignore_case=TRUE))

str_locate(string, "data")
str_locate_all(string, "data")

str_extract(string, "data")
str_extract_all(string, "data")

str_replace(string=string, pattern="data", replacement="text")
str_replace_all(string, "data", "text")

str_split(string, " ")

unlist(str_split(string, " "))

## lubridate 패키지

library(lubridate)
ymd("2030-03-15")
mdy("03/15/2030")
dmy("15032030")

ymd("300315")
ymd("2030년 3월 15일")
mdy("3월 15일, 2030")
dmy("15-March-2030")

ymd(20300315)

ymd_hms("2030-03-15 23:11:59")
mdy_hm("03/15/2030 23:12")
dmy_h("15032030 23")

datetime <- ymd_hms("1969/07/20, 20:17:39")
datetime
year(datetime)
month(datetime)
mday(datetime)
hour(datetime)
minute(datetime)
second(datetime)
yday(datetime)
wday(datetime)

datetime
round_date(datetime, unit="year")
round_date(datetime, unit="month")
round_date(datetime, unit="day")
round_date(datetime, unit="week")
floor_date(datetime, unit="month")
ceiling_date(datetime, unit="month")

years(1)
months(3)
days(7)
weeks(2)

10 * (months(6) + days(2))
days(60) + hours(12) + minutes(15)

moon <- ymd("1969/07/20")
moon
moon + days(10000)
moon + months(1000)
moon - years(100)

mars <- ymd("2021/02/18")
mars
mars - moon

interval(moon, mars)
class(interval(moon, mars))

moon %--% mars

as.period(moon %--% mars)

####################################
## A.3 입력 및 출력: readr 패키지 ##
####################################

library(readr)
read_csv(file="product.csv")
read_csv(file="product.csv", show_col_types=FALSE)

read_csv("product-with-no-header.csv", col_names=FALSE, show_col_types=FALSE)
read_csv("product-with-no-header.csv", col_names=c("id", "name", "price"), 
         show_col_types=FALSE)

read_delim(file="product.txt", delim=" ", show_col_types=FALSE)
read_delim("product-with-no-header.csv", delim=",", 
           col_names=c("id", "name", "price"), show_col_types=FALSE)

read_table(file="product.txt")

read_lines(file="won-dollar.txt")

read_lines("won-dollar.txt", skip=1, n_max=3)

parse_number("$100")
parse_number("30%")
parse_number("61.3kg")
parse_number("Salary per year: $250,000")

Orange
write_csv(x=Orange, file="orange.csv")
read_csv("orange.csv", show_col_types=FALSE)

write_delim(x=Orange, file="orange.txt", delim=";")
read_delim("orange.txt", delim=";", show_col_types=FALSE)

#####################
## A.4 데이터 변환 ##
#####################

## dplyr 패키지

head(airquality)

library(dplyr)
filter(airquality, Month == 6)

filter(airquality, Month == 6, Temp > 90)
filter(airquality, Month == 6 & Temp > 90)
filter(airquality, Ozone > 80 | Temp > 90)

slice(airquality, 6:10)

slice_head(airquality, n=3)
slice_tail(airquality, n=3)

slice_max(airquality, order_by=Ozone, n=3)
slice_min(airquality, order_by=Temp, n=3)

select(airquality, Month, Day, Temp)
select(airquality, Temp:Day)
select(airquality, -(Ozone:Wind))

select(airquality, Month, Day, everything())

arrange(airquality, Temp, Month, Day)
arrange(airquality, desc(Temp), Month, Day)

mutate(airquality, Temp.C=(Temp-32)/1.8, Diff=Temp.C - mean(Temp.C))

summarise(airquality, mean(Temp))
summarise(airquality, 
          Min=min(Temp, na.rm=TRUE),
          Median=median(Temp, na.rm=TRUE),
          Mean=mean(Temp, na.rm=TRUE),
          SD=sd(Temp, na.rm=TRUE),
          Max=max(Temp, na.rm=TRUE),
          N=n(),
          Distinct.Month=n_distinct(Month),
          First.Month=first(Month),
          Last.Month=last(Month))	

air.group <- group_by(airquality, Month)
class(air.group)
air.group

summarise(air.group,
          Number.of.Days=n(),
          Mean.Temp=mean(Temp, na.rm=TRUE),
          SD.Temp=sd(Temp, na.rm=TRUE))

air.ungroup <- ungroup(air.group)
class(air.ungroup)
summarise(air.ungroup, Mean.Temp=mean(Temp, na.rm=TRUE))

count(airquality, Month, sort=TRUE)

df1 <- data.frame(x=1:6, y=month.name[1:6])
df2 <- data.frame(x=7:12, y=month.name[7:12])
df3 <- bind_rows(df1, df2)
df3

df4 <- data.frame(z=month.abb)
bind_cols(df3, df4)

band_members
band_instruments

inner_join(x=band_members, y=band_instruments, by="name")

left_join(band_members, band_instruments, by="name")
right_join(band_members, band_instruments, by="name")
full_join(band_members, band_instruments, by="name")

band_instruments2
full_join(band_members, band_instruments2, by=c("name"="artist"))

semi_join(band_members, band_instruments, by="name")

anti_join(band_members, band_instruments, by="name")

iris %>% head
1:10 %>% mean

a1 <- select(airquality, Ozone, Temp, Month)
a2 <- group_by(a1, Month)
a3 <- summarise(a2, 
                Mean.Ozone=mean(Ozone, na.rm=TRUE), 
                Mean.Temp=mean(Temp, na.rm=TRUE))
a4 <- filter(a3, Mean.Ozone > 30 | Mean.Temp > 70)
a5 <- arrange(a4, desc(Mean.Temp))
a6 <- left_join(a5, tibble(Month=1:12, Month.Name=month.name), by="Month")
a6

air <- airquality %>% 
  select(Ozone, Temp, Month) %>% 
  group_by(Month) %>% 
  summarise(Mean.Ozone=mean(Ozone, na.rm=TRUE), 
            Mean.Temp=mean(Temp, na.rm=TRUE)) %>% 
  filter(Mean.Ozone > 30 | Mean.Temp > 70) %>% 
  arrange(desc(Mean.Temp)) %>% 
  left_join(tibble(Month=1:12, Month.Name=month.name), by="Month")
air

## purrr 패키지

exams <- list(s1=c(78, 89, 91, 85, 95, 98),
              s2=c(85, 86, 97, 99, 90),
              s3=c(98, 96, 89, 90, 93, 85, 92),
              s4=c(98, 96, 91, 88, 93, 99))
exams
library(purrr)
map(.x=exams, .f=mean)

map_dbl(exams, mean)

map_dbl(exams, mean, trim=0.3)

exams %>% 
  map_dbl(mean, trim=0.3)

exams %>% 
  map(function(x) x*1.1) 

exams %>% 
  map(~.x*1.1)

exams %>% 
  map(~.*1.1)

fruits <- c("Apple", "Banana", "Strawberry")
fruits %>% 
  map_chr(paste, "Juice", sep="-")
fruits %>% 
  map_chr(~paste(.x, "Juice", sep="-"))

lst <- list(list(num=1:3, letters[1:3]),
            list(num=101:103, chr=letters[4:6]),
            list(),
            list(num=c(9, 99), chr=letters[7:9]))
lst

lst %>% 
  map("num")

lst %>% 
  map(2, .default=NA)

USArrests %>% 
  map_dfr(range)

reduce(.x=c(1, 3, 5, 7), .f=`*`)

dfs <- list(data.frame(name="Superman", age=30),
            data.frame(name=c("Spiderman", "Wonderwoman"), sex=c("M", "F")),
            data.frame(name="Batman", grade="A"))
dfs

library(dplyr)
dfs %>% 
  reduce(bind_rows)

## tidyr 패키지

head(airquality, 3)
library(tidyr)
aq.long <- pivot_longer(data=airquality, cols=Ozone:Temp,
                        names_to="Factor", values_to="Measurement")
head(aq.long, 3)
tail(aq.long, 3)

aq.wide <- pivot_wider(data=aq.long, names_from=Factor, values_from=Measurement)
head(aq.wide, 3)
tail(aq.wide, 3)

df <- tibble(x=c(1, 1, 1, 2, 2, 3), y=1:6, z=6:1)
df
df.nested <- nest(.data=df, ndata=c(y, z))
df.nested
df.nested$ndata
df.nested$ndata[[2]]

unnest(df.nested, cols=ndata)

head(mtcars, 3)
library(dplyr)
mtcars.n <- mtcars %>%
  group_by(cyl) %>%
  nest() 
mtcars.n

head(iris, 3)
iris.long <- pivot_longer(data=iris, cols=-Species, 
                          names_to="Element", values_to="Measurement")
head(iris.long, 3)

iris.sep <- separate(data=iris.long, col=Element, into=c("Part", "Measures"))
head(iris.sep, 3)

iris.unite <- unite(data=iris.sep, col="Factor", Part, Measures, sep="_")
head(iris.unite, 3)

################################
## A.5 그래픽: ggplot2 패키지 ##
################################

## 그래프 생성

install.packages("ggplot2")
library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-1]
str(mtcars)
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + 
  geom_point() +
  labs(title="Fuel Consumption vs. Weight", 
       x="Weight (1,000 lbs)", y="Fuel Consumption (miles per gallon)") 

data(mtcars)
mtcars$cyl <- factor(mtcars$cyl, levels=c(4, 6, 8),
                     labels=c("4 cylinders", "6 cylinders", "8 cylinders"))
ggplot(data=mtcars, mapping=aes(x=mpg)) + geom_histogram() + facet_grid(cyl ~ .) +
  labs(title="geom_histogram()", x="Miles per Gallon")
ggplot(data=mtcars, mapping=aes(x=cyl, y=mpg)) + geom_boxplot() +
  labs(title="geom_boxplot()", x="Number of Cylinders", y="Miles per Gallon")
ggplot(data=mtcars, mapping=aes(x=mpg, fill=cyl)) + geom_density(outline.type="full") +
  labs(title="geom_density()", x="Miles per Gallon")
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg, col=cyl)) + geom_point() +
  labs(title="geom_point()", x="Weight (1,000 lbs)", y="Miles per Gallon")
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + geom_smooth() +
  labs(title="geom_smooth()", x="Weight (1,000 lbs)", y="Miles per Gallon")
ggplot(data=economics, mapping=aes(x=date, y=unemploy)) + geom_line() +
  labs(title="geom_line()", x="Year", y="Number of Unemployed (thousands)")

# [그림 A-2]
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl, levels=c(4, 6, 8),
                     labels=c("4 cylinders", "6 cylinders", "8 cylinders"))
g1 <- ggplot(data=mtcars, mapping=aes(x=mpg)) + geom_histogram() + facet_grid(cyl ~ .) +
  labs(title="geom_histogram()", x="Miles per Gallon")
g2 <- ggplot(data=mtcars, mapping=aes(x=cyl, y=mpg)) + geom_boxplot() +
  labs(title="geom_boxplot()", x="Number of Cylinders", y="Miles per Gallon")
g3 <- ggplot(data=mtcars, mapping=aes(x=mpg, fill=cyl)) + geom_density(outline.type="full") +
  labs(title="geom_density()", x="Miles per Gallon")
g4 <- ggplot(data=mtcars, mapping=aes(x=wt, y=mpg, col=cyl)) + geom_point() +
  labs(title="geom_point()", x="Weight (1,000 lbs)", y="Miles per Gallon")
g5 <- ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + geom_smooth() +
  labs(title="geom_smooth()", x="Weight (1,000 lbs)", y="Miles per Gallon")
g6 <- ggplot(data=economics, mapping=aes(x=date, y=unemploy)) + geom_line() +
  labs(title="geom_line()", x="Year", y="Number of Unemployed (thousands)")

library(gridExtra)
windows(width=10.0, height=15)
grid.arrange(g1, g2, g3, g4, g5, g6, ncol=2)

# [그림 A-3]
windows(width=7.0, height=5.5)
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + 
  geom_point() + 
  geom_smooth() +
  labs(title="Fuel Consumption vs. Weight", 
       x="Weight (1,000 lbs)", y="Fuel Consumption (miles per gallon)")

## geom 객체 옵션

library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-4]
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + 
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red",linetype=2, size=1) +
  labs(title="Fuel Consumption vs. Weight", 
       x="Weight (1,000 lbs)", y="Fuel Consumption (miles per gallon)")

# [그림 A-5]
ggplot(data=mtcars, mapping=aes(x=wt, y=mpg)) + 
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red",linetype=2, size=1) +
  geom_text(label=rownames(mtcars), hjust=0, vjust=1, nudge_y=0.7, size=2) +
  labs(title="Fuel Consumption vs. Weight", 
       x="Weight (1,000 lbs)", y="Fuel Consumption (miles per gallon)")

# [그림 A-6]
library(car)
str(Salaries)
ggplot(Salaries, aes(x=rank, y=salary)) +
  geom_boxplot(fill="coral3", color="black", notch=TRUE) +
  geom_point(position="jitter", color="blue", alpha=0.5) +
  geom_rug(sides="l", color="black")

## 집단별 그래프

library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-7]
library(car)
ggplot(Salaries, aes(x=salary, fill=rank)) + 
  geom_density(alpha=0.5, outline.type="full")

# [그림 A-8]
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=sex)) +
  geom_point()

library(car)
ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") + 
  labs(title='position="dodge"')
ggplot(Salaries, aes(x=rank, fill=sex)) + 
  geom_bar(position="stack") + 
  labs(title='position="stack"')
ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="fill") + 
  labs(title='position="fill"', y="proportion")

# [그림 A-9]
library(car)
p1 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") + 
  labs(title='position="dodge"')
p2 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="stack") +  
  labs(title='position="stack"')
p3 <- ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="fill") + 
  labs(title='position="fill"', y="proportion")
library(gridExtra)
windows(width=6.0, height=8.0)
grid.arrange(p1, p2, p3, nrow=3)

# [그림 A-10]
library(lattice)
windows(width=7.0, height=5.5)
ggplot(data=singer, aes(x=height)) +
  geom_histogram() +  
  facet_wrap(~ voice.part, nrow=4)

# [그림 A-11]
windows(width=6.0, height=6.0)
ggplot(data=singer, aes(x=height, fill=voice.part)) +
  geom_density(outline.type="full") +  
  facet_grid(voice.part ~ .)

# [그림 A-12]
windows(width=7.0, height=5.5)
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) + 
  geom_point() + 
  facet_grid(sex ~ rank)

# [그림 A-13]
windows(width=7.0, height=5.5)
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=rank)) +
  geom_point() + 
  facet_grid(. ~ sex)

## 그래프 옵션

library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-14]
library(car)
ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor", 
                            "Associate\nProfessor", "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Sex", x="", y="")

# [그림 A-15]
library(car)
ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor", 
                            "Associate\nProfessor", "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000), 
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Gender", x="", y="", fill="Gender") +
  theme(legend.position=c(0.1, 0.85))

# [그림 A-15] reproduction
library(car)
ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor", 
                            "Associate\nProfessor", "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000), 
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  scale_fill_discrete(name="Gender") +
  labs(title="Faculty Salary by Rank and Gender", x="", y="") +
  theme(legend.position=c(0.1, 0.85))

# [그림 A-16]
library(car)
ggplot(Salaries, aes(x=rank, fill=sex)) +  
  geom_bar() + 
  scale_fill_manual(values=c("tomato", "cornflowerblue")) 

# [그림 A-17]
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  geom_point(size=2) +
  scale_color_manual(values=c("orange", "violetred", "steelblue"))

# [그림 A-18]
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  geom_point(size=2) +
  scale_color_brewer(palette="Accent")

library(RColorBrewer)
?RColorBrewer
display.brewer.all()
display.brewer.pal(3, "Accent")

# [그림 A-19]
ggplot(data=mtcars, aes(x=wt, y=mpg, size=disp)) + 
  geom_point(shape=21, color="black", fill="wheat") +
  labs(size="Engine\nDisplacement\n(cubic inch)")

# [그림 A-20]
ggplot(data=mtcars, aes(x=wt, y=mpg, size=disp)) + 
  geom_point(shape=21, color="black", fill="wheat") +
  labs(size="Engine\nDisplacement\n(cubic inch)") +
  scale_size_continuous(range=c(1, 12))

# [그림 A-21]
ggplot(data=mtcars, aes(x=wt, y=mpg, color=disp)) + 
  geom_point(size=5) +
  labs(color="Engine\nDisplacement\n(cubic inch)") +
  scale_color_gradient(low="orange", high="maroon")

# [그림 A-22]
ggplot(data=mtcars, aes(x=wt, y=mpg, fill=disp)) + 
  geom_point(shape=21, size=5, color="black") +
  labs(fill="Engine\nDisplacement\n(cubic inch)") +
  scale_fill_distiller(palette="YlOrRd", direction=1)

# [그림 A-23]
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=rank)) +
  scale_shape_manual(values=c(15, 17, 19)) +
  geom_point(size=2)

## 테마

library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-24]
library(car)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=rank)) +
  geom_point() + 
  facet_grid(. ~ sex) + 
  theme_dark()

?theme

# [그림 A-25]
library(car)
mytheme <- theme(plot.title=element_text(face="bold.italic", 
                                         size="14", color="brown"),
                 axis.title=element_text(face="bold.italic",
                                         size="10", color="tomato"),
                 axis.text=element_text(face="bold",
                                        size="9", color="royalblue"),
                 panel.background=element_rect(fill="snow", color="darkblue"),
                 panel.grid.major.y=element_line(color="gray", linetype="solid"),
                 panel.grid.minor.y=element_line(color="gray", linetype="dashed"),
                 legend.position="top")
ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  labs(title="Salary by Rank and Sex", x="Rank", y="Salary") +
  mytheme

## 그래프 배치 및 저장

library(ggplot2)
windows(width=7.0, height=5.5)

# [그림 A-26]
library(car)
p1 <- ggplot(Salaries, aes(x=rank)) + geom_bar()
p2 <- ggplot(Salaries, aes(x=salary)) + geom_histogram()
p3 <- ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) + geom_point()
p4 <- ggplot(Salaries, aes(x=rank, y=salary)) + geom_boxplot() 
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

# [그림 A-27]
grid.arrange(arrangeGrob(p3, p4, nrow=2, heights=c(0.5, 0.5)), p1,
             ncol=2, widths=c(0.6, 0.4))

myggplot <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
ggsave(file="myplot.png", plot=myggplot, width=7.0, height=5.5)

library(pander)
openFileInOS("myplot.png")

ggplot(Salaries, aes(x=rank, y=salary)) + geom_boxplot()
ggsave(file="myplot.png")

openFileInOS("myplot.png")

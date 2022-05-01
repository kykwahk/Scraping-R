
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

###########################
## 제3장 텍스트 패턴매칭 ##
###########################

##############
## 3.4 사례 ##
##############

## 직업별 연봉 @CNBC

library(httr)
url <- "https://www.cnbc.com/2020/01/06/the-best-paying-jobs-of-2020-from-us-news-and-world-report.html"
html <- content(GET(url), type="text")

save(html, file="cnbc-salary.rda")
load("cnbc-salary.rda")

library(stringr)
job.pattern <- "(\\d{1,2}\\.)\\s([A-Za-z\\s])+"
job <- unlist(str_extract_all(html, job.pattern))[1:25]
job

library(readr)
salary.pattern <- "(Mean salary:\\s<.+?>)\\$[\\d+,]+"
salary <- unlist(str_extract_all(html, salary.pattern)) %>% 
  parse_number()
salary

library(tibble)
library(tidyr)
library(dplyr)
jobsalary <- tibble(job=job, salary=salary) %>% 
  separate(col=job, into=c("rank", "job"), sep="\\. ", convert=TRUE) %>% 
  arrange(rank)
jobsalary

# [그림 3-11]
library(ggplot2)
library(scales)
windows(width=7.0, height=5.5)
ggplot(jobsalary, aes(x=reorder(job, salary), y=salary)) +
  geom_col(color="gray50", fill="orange") +
  geom_text(aes(label=paste0("$", format(salary, big.mark=","))), size=3, 
            fontface="bold", color="dimgray", hjust=-0.1) +
  scale_y_continuous(limit=c(0, 300000), labels=comma) +
  coord_flip() +
  labs(x=NULL, y="Salary Per Year (dollars)", 
       title="Best-Paying Jobs Rankings",
       subtitle="25 best-paying jobs",
       caption="Source: CNBC") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        axis.line.x=element_line(color="gray"),
        panel.grid.minor=element_blank())

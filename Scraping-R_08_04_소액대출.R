
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

## 소액대출 @Kiva

library(tidyverse)
library(ghql)
library(jsonlite)

url <- "https://api.kivaws.org/graphql"
con <- GraphqlClient$new(url=url)
con
class(con)

query.borrower <- "query {
  lend {
    loans(filters: {status: fundraising}, sortBy: newest, limit: 20) {
      values {
        name
        loanAmount
        activity {
          name
        }
        geocode {
          country {
            name
          }
        }
      }
    }
  }
}"

qry <- Query$new()
class(qry)
qry$query("borrower", query.borrower)
qry
qry$queries$borrower

con$exec(qry$queries$borrower) %>% 
  prettify(indent=2)

result <- con$exec(qry$queries$borrower) %>% 
  fromJSON(flatten=TRUE)
str(result)
result <- result$data$lend$loans$values %>% 
  as_tibble() %>% 
  mutate(loanAmount=as.numeric(loanAmount))
result

summary(result$loanAmount)

con <- GraphqlClient$new(url=url)

query.lender <- "query {
  lend {
    lendingActions(limit: 100) {
      values {
        lender {
          id
          name
          loanCount
        }
        loan {
          name
          activity {
            name
          }
          loanAmount
        }
        shareAmount
      }
    }
  }
}"

qry <- Query$new()
qry$query("lender", query.lender)

result <- con$exec(qry$queries$lender) %>% 
  fromJSON(flatten=TRUE)
str(result)
result <- result$data$lend$lendingActions$values %>% 
  mutate(loan.loanAmount=as.numeric(loan.loanAmount),
         shareAmount=as.numeric(shareAmount)) %>% 
  as_tibble()
result

distinct(result, lender.id, .keep_all=TRUE) %>% 
  summarize(number_of_lender=n(),
            median_loanCount=median(lender.loanCount),
            mean_loanCount=mean(lender.loanCount),
            max_loanCount=max(lender.loanCount),
            min_loanCount=min(lender.loanCount))

query.borrower.page <- "query($offset: Int!) {
  lend {
    loans(filters: {status: fundraising}, sortBy: newest, limit: 100, offset: $offset) {
      values {
        name
        loanAmount
        activity {
          name
        }
        geocode {
          city
          state
          country {
            name
            isoCode
            region
          }
          latitude
          longitude
        }
      }
    }
  }
}"

con <- GraphqlClient$new(url=url)
qry <- Query$new()
qry$query("borrower.page", query.borrower.page)

kiva.loan <- tibble()
for (i in seq(0, 900, by=100)) {
  page <- con$exec(qry$queries$borrower.page, variables=list(offset=i)) %>% 
    fromJSON(flatten=TRUE)
  result <- page$data$lend$loans$values %>%
    mutate(loanAmount=as.numeric(loanAmount)) %>% 
    as_tibble()
  kiva.loan <- bind_rows(kiva.loan, result)
}

kiva.loan

save(kiva.loan, file="kiva-loan.rda")
load("kiva-loan.rda")

summary(kiva.loan$loanAmount)

kiva.loan %>% 
  group_by(activity.name) %>% 
  count(sort=TRUE) %>% 
  ungroup() %>% 
  slice_head(n=10)

kiva.loan.activity <- kiva.loan %>% 
  group_by(activity.name) %>% 
  count(sort=TRUE) %>% 
  ungroup() %>% 
  slice_head(n=10) %>% 
  mutate(label=str_c(activity.name, n, sep="\n"))
kiva.loan.activity

# [그림 8-22]
library(treemap)
windows(width=7.0, height=5.5)
treemap(dtf=kiva.loan.activity, index="label", vSize="n", type="value", vColor="n", 
        palette="RdYlGn", border.col="black", border.lwds=3, force.print.labels=TRUE, 
        title="Distribution of Kiva's Recent 1,000 Lendings by Top 10 Activity", 
        title.legend="Counts")

kiva.loan %>% 
  group_by(geocode.country.region) %>% 
  count(sort=TRUE) %>% 
  ungroup() 
kiva.loan %>% 
  group_by(geocode.country.name) %>% 
  count(sort=TRUE) %>% 
  ungroup() %>% 
  slice_head(n=10) 

# [그림 8-23]
library(ggridges)
library(hrbrthemes)
windows(width=7.0, height=5.0)
ggplot(kiva.loan, aes(x=loanAmount, y=fct_rev(geocode.country.region), fill=..x..)) +
  geom_density_ridges_gradient(scale=3, rel_min_height=0.01) +
  scale_fill_viridis_c(option="plasma", begin=0.1) +
  labs(x="Loan (dollars)", y="Region", 
       title="Kiva Loan",
       subtitle="Distribution of loan amount by region",
       caption="Source: Kiva") +
  theme_ipsum(axis_title_size=12,
              plot_margin=margin(10, 10, 10, 10)) +
  theme(legend.position="none")

# [그림 8-24]
map.data <- map_data(map="world")
windows(width=7.0, height=5.0)
ggplot(map.data) + 
  geom_map(aes(map_id=region), map=map.data, 
           fill="darkolivegreen", color="snow", size=0.5) + 
  expand_limits(x=map.data$long, y=map.data$lat) + 
  coord_fixed(xlim=c(-180, 180), ylim=c(-55, 90), ratio=1.3) +
  geom_point(data=arrange(kiva.loan, loanAmount), shape=20, 
             aes(x=geocode.longitude, y=geocode.latitude, size=loanAmount, 
                 color=loanAmount, alpha=loanAmount)) +
  scale_size_continuous(name="Loan (dollars)", range=c(1, 10)) +
  scale_alpha_continuous(name="Loan (dollars)", range=c(0.1, 0.9)) +
  scale_color_viridis_c(name="Loan (dollars)", option="plasma") +
  labs(title="Kiva Loan",
       subtitle="Distribution of borrowers",
       caption="Source: Kiva") +
  theme_void() +
  guides(size=guide_legend(nrow=1), color=guide_legend(nrow=1),
         alpha=guide_legend(nrow=1)) +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=8), 
        legend.text=element_text(size=8))

library(sf)
library(spData)
world
unique(world$continent)

map.data <- filter(world, continent=="South America") %>% 
  select(name_long, continent, geom)
country.center <- st_coordinates(st_centroid(st_make_valid(map.data$geom))) 
map.data <- cbind(map.data, country.center)
map.data

map.samerica <- left_join(map.data, kiva.loan, by=c("continent"="geocode.country.region"))
names(map.samerica)

# [그림 8-25]
library(ggspatial)
library(ggrepel)
library(scales)
windows(width=4.5, height=8.0)
ggplot(data=arrange(map.samerica, loanAmount)) +
  geom_sf(aes(geometry=geom), fill="azure", color="dimgray") +
  annotation_scale(location="tr", width_hint=0.3) +
  geom_point(shape=20, position="jitter",
             aes(x=geocode.longitude, y=geocode.latitude, size=loanAmount, 
                 color=loanAmount, alpha=loanAmount)) +
  geom_text_repel(data=map.data, aes(x=X, y=Y, label=name_long), 
                  color="darkcyan", fontface="bold") +
  scale_size_continuous(name="Loan (dollars)", range=c(1, 15), labels=comma) +
  scale_alpha_continuous(name="Loan (dollars)", range=c(0.1, 0.9), labels=comma) +
  scale_color_distiller(name="Loan (dollars)", palette="RdYlBu", direction=-1, labels=comma) +
  labs(title="Kiva Loan",
       subtitle="Distribution of borrowers in South America",
       caption="Source: Kiva") +
  theme_void() +
  guides(size=guide_legend(nrow=1), color=guide_legend(nrow=1),
         alpha=guide_legend(nrow=1)) +
  theme(plot.title=element_text(face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=8), 
        legend.text=element_text(size=8))

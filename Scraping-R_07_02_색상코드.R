
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제7장 JSON ##
################

##############
## 7.3 사례 ##
##############

## 색상 코드 @GitHub

library(jsonlite)
url1 <- "https://raw.githubusercontent.com/bahamas10/css-color-names/master/css-color-names.json"
colorcode <- fromJSON(url1)
head(colorcode)
colorcode <- unlist(colorcode)
head(colorcode)

download.file(url1, "github-colorcode.json")

library(tidyverse)
colorcode.df <- colorcode %>% 
  toupper(.) %>% 
  enframe(name="color", value="colorcode")
colorcode.df

url2 <- "https://raw.githubusercontent.com/corysimmons/colors.json/master/colors.json"
colorRGB <- fromJSON(url2)
head(colorRGB)

download.file(url2, "github-colorRGB.json")

colorRGB %>% 
  enframe(name="color", value="RGB")

colorRGB.df <- colorRGB %>% 
  enframe(name="color", value="RGB") %>%  
  unnest_wider(col=RGB, names_sep="_") %>% 
  rename_with(.fn=function(x) c("color", "R", "G", "B", "alpha")) 
colorRGB.df

colors <- inner_join(colorcode.df, colorRGB.df, by="color")
colors

save(colors, file="github-colors.rda")
load("github-colors.rda")

colors.table <- bind_cols(colors,
                          tibble(y=seq(nrow(colors)-1, 0) %% 37,
                                 x=seq(0, nrow(colors)-1) %/% 37))
slice_head(colors.table, n=5); slice_tail(colors.table, n=5)

# [그림 7-2]
windows(width=7.0, height=9.0)
ggplot() +
  geom_rect(data=colors.table, 
            mapping=aes(xmin=x, xmax=x+1, ymin=y, ymax=y+1), fill="white") +
  geom_rect(data=colors.table, 
            mapping=aes(xmin=x+0.05, xmax=x+0.95, ymin=y+0.5, ymax=y+1, 
                        fill=colorcode)) +
  geom_text(data=colors.table, 
            mapping=aes(x=x+0.5, y=y+0.5, 
                        label=paste(color, ";", colorcode, ";", "RGB", R, G, B)), 
            color="black", hjust=0.5, vjust=1, size=2) +
  scale_x_continuous(name=NULL, breaks=NULL, expand=c(0, 0)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand=c(0, 0)) +
  scale_fill_identity() +
  labs(title="Colors",
       subtitle="Color codes and RGB values",
       caption="Source: GitHub")

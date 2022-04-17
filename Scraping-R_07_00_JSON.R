
##########################################
## R을 이용한 웹스크레이핑과 데이터분석 ##
## (곽기영, 도서출판 청람)              ## 
##########################################

################
## 제7장 JSON ##
################

###################
## 7.1 JSON 구조 ##
###################

json.string <- c(
  '{"bondmovies" : [',
  '{',
  '"title" : "Goldfinger",',
  '"director" : "Guy Hamilton",',
  '"year" : 1964,',
  '"genre" : ["Action", "Adventure", "Thriller"],',
  '"runtime" : "110 min",',
  '"actor" : {',
  '"James Bond" : "Sean Connery",',
  '"Bond Girl" : "Pussy Galore"',
  '},',
  '"gross" : 51081062',
  '},',
  '{',
  '"title" : "The Man with the Golden Gun",',
  '"director" : "Guy Hamilton",',
  '"year" : 1974,',
  '"genre" : ["Action", "Adventure", "Thriller"],',
  '"runtime" : "123 min",',
  '"actor" : {',
  '"James Bond" : "Roger Moore",',
  '"Bond Girl" : "Britt Ekland"',
  '},',
  '"gross" : 20972000',
  '},',
  '{',
  '"title" : "License to Kill",',
  '"director" : "John Glen",',
  '"year" : 1989,',
  '"genre" : ["Action", "Adventure"],',
  '"runtime" : "133 min",',
  '"actor" : {',
  '"James Bond" : "Timothy Dalton",',
  '"Bond Girl" : "Carey Lowell"',
  '},',
  '"gross" : 34667015',
  '}]',
  '}'
)
writeLines(json.string, "bondmovies.json")

library(pander)
openFileInOS("bondmovies.json")

###################
## 7.2 JSON 파싱 ##
###################

## RJSONIO 패키지

library(RJSONIO)
isValidJSON("bondmovies.json")

bondmovies <- fromJSON("bondmovies.json")
class(bondmovies)
bondmovies

bondmovies.v <- unlist(bondmovies, recursive=TRUE, use.names=TRUE)
bondmovies.v

bondmovies.v[grepl("title", names(bondmovies.v))]

bondmovies.v[grepl("gross", names(bondmovies.v))]

bondmovies[[1]][[1]][["gross"]]
bondmovies[[1]][[2]][["gross"]]
bondmovies[[1]][[3]][["gross"]]

sapply(bondmovies[[1]], "[[", "gross")

bondmovies.df <- data.frame(t(sapply(bondmovies[[1]], c)))
bondmovies.df
bondmovies.df$title
bondmovies.df$genre

json.string <- c(
  '[',
  '{',
  '"name" : "Sean Connery",',
  '"sex" : "male",',
  '"age" : 32',
  '},',
  '{',
  '"name" : "Brit Ekland",',
  '"sex" : "female",',
  '"age" : 28',
  '},',
  '{',
  '"name" : "Roger Moore",',
  '"sex" : "male",',
  '"age" : null',
  '}',
  ']'
)
writeLines(json.string, "actors.json")
library(pander)
openFileInOS("actors.json")

library(RJSONIO)
isValidJSON("actors.json")

actors <- fromJSON("actors.json")
actors <- fromJSON("actors.json", nullValue=NA)
actors <- fromJSON("actors.json", nullValue=NA, simplify=FALSE)
actors

library(purrr)
library(tibble)
actors.df <- map_dfr(actors, as_tibble)
actors.df

actors.js <- toJSON(actors.df)
cat(actors.js)

actors.js <- toJSON(actors.df, pretty=TRUE)
cat(actors.js)

writeLines(actors.js, "actors.js")
library(pander)
openFileInOS("actors.js")

## jsonlite 패키지

search()
detach(package:RJSONIO)

library(jsonlite)

js <- '[1, 2, "zoo"]'
fromJSON(js)

js <- '[1, null, "zoo"]'
fromJSON(js)

js <- '[1, 2, true, false]'
fromJSON(js)

js <- '["zoo", true, false]'
fromJSON(js)

js <- '[null, true, false]'
fromJSON(js)

actors <- fromJSON("actors.json")
actors

bondmovies <- fromJSON("bondmovies.json")
bondmovies

bondmovies.df <- bondmovies$bondmovies
bondmovies.df <- bondmovies[[1]]
bondmovies.df
names(bondmovies.df)
bondmovies.df$title
bondmovies.df$genre

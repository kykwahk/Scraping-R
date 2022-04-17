
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

## eBook @프로젝트 구텐베르크

install.packages("gutenbergr")
library(gutenbergr)

gutenberg_metadata
names(gutenberg_metadata)

library(tidyverse)
gutenberg_metadata %>%
  filter(title=="Alice's Adventures in Wonderland")

gutenberg_works()

gutenberg_works(author=="Carroll, Lewis")
gutenberg_works(str_detect(author, "Carroll"))

gutenberg_download(11)

# 미러 사이트 https://www.gutenberg.org/MIRRORS.ALL
gutenberg_download(11, mirror="http://www.gutenberg.org/dirs/")

carroll <- gutenberg_download(c(11, 12), meta_fields="title")
carroll <- gutenberg_download(c(11, 12), meta_fields="title", 
                              mirror="http://www.gutenberg.org/dirs/")
carroll
carroll %>%
  count(title)

titles <- c("A Christmas Carol", "A Tale of Two Cities")
dickens <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields="title")
dickens <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields="title", mirror="http://www.gutenberg.org/dirs/")
dickens %>%
  count(title)

titles <- c("Around the World in Eighty Days", "Jane Eyre: An Autobiography",
            "Great Expectations", "Pride and Prejudice")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields="title")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields="title", mirror="http://www.gutenberg.org/dirs/")
books

save(books, file="gutenberg-books.rda")
load("gutenberg-books.rda")

books.chapter <- books %>%
  group_by(title) %>%
  mutate(chapter=cumsum(str_detect(text, regex("^chapter ", 
                                               ignore_case=TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(col=document, title, chapter, sep="_", remove=FALSE) %>%
  group_by(document, title, chapter) %>%
  summarise(text=paste(text, collapse=" ")) %>%
  ungroup()
books.chapter
books.chapter %>%
  count(title)

library(tidytext)
books.chapter.word <- books.chapter %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word")
books.chapter.word

books.word.top <- books.chapter.word %>%
  group_by(title) %>%
  count(word) %>%
  slice_max(n=10, order_by=n) %>%
  ungroup() 
books.word.top

# [그림 8-36]
windows(width=7.0, height=7.0)
ggplot(books.word.top, 
       aes(reorder_within(x=word, by=n, within=title), n, fill=title)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~title, scales="free") +
  scale_x_reordered() +
  scale_fill_viridis_d(end=0.75, option="viridis") +
  coord_flip() +
  theme_minimal() +
  labs(x=NULL, y="Frequency",
       title="Text Analysis",
       subtitle="Top 10 words of selective books",
       caption="Source: Project Gutenberg") +
  theme(strip.background=element_blank(), 
        strip.text=element_text(color="firebrick", face="bold"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(size=8, face="bold"))

books.chapter.word
books.sent <- books.chapter.word %>%
  group_by(title) %>% 
  mutate(word_count=1:n(),
         page=word_count %/% 250 + 1) %>% 
  inner_join(get_sentiments("bing"), by="word") %>%
  count(title, page, sentiment) %>%
  ungroup() %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=0) %>% 
  mutate(sentiment=positive - negative)
books.sent

# [그림 8-37]
windows(width=7.0, height=7.0)  
ggplot(books.sent, aes(page, sentiment, fill=title)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~title, ncol=2, scales="free_x") +
  labs(x="Pages by 250 Words", y="Sentiment Score",
       title="Sentiment Analysis",
       subtitle="Sentiment scores over the course of each book",
       caption="Source: Project Gutenberg") +
  theme(strip.text=element_text(color="coral", face="bold"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold", size=8),
        panel.grid.minor=element_blank())

books.chapter.word
books.sent.word <- books.chapter.word %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(title, word, sentiment) %>%
  group_by(title, sentiment) %>%
  slice_max(n=10, order_by=n, with_ties=FALSE) %>%
  ungroup()
books.sent.word

# [그림 8-38]
windows(width=7.0, height=9.0)  
ggplot(books.sent.word, 
       aes(reorder_within(x=word, by=n, within=title), n, fill=sentiment)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  facet_wrap(~ title + sentiment, scales="free", ncol=2) +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Contribution to Sentiment",
       title="Sentiment Analysis",
       subtitle="Top sentiment words",
       caption="Source: Project Gutenberg") +
  theme_bw() +
  theme(strip.background=element_blank(), 
        strip.text=element_text(color="goldenrod4", face="bold"),
        plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        panel.grid.minor=element_blank())

stop_words_custom <- bind_rows(tibble(word=c("miss", "master"), 
                                      lexicon=c("custom")), 
                               stop_words)
stop_words_custom

books.chapter
book <- books.chapter %>% 
  filter(title=="Around the World in Eighty Days") %>%
  unnest_tokens(sentence, text, token="sentences")
book

book.sent <- book %>%
  group_by(chapter) %>%
  mutate(sentence_count=1:n(),
         progress=round(sentence_count/n(), 2)) %>%
  group_by(chapter, progress) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn"), by="word") %>%
  summarise(sentiment=sum(value, na.rm=TRUE)) %>%
  arrange(desc(sentiment)) %>%
  ungroup()
book.sent

# [그림 8-39]
library(scales)
windows(width=8.0, height=8.0)
ggplot(book.sent, 
       aes(x=progress, 
           y=factor(chapter, levels=sort(unique(chapter), decreasing=TRUE)),
           fill=sentiment)) +
  geom_tile(color="white") +
  scale_fill_viridis_c(option="plasma") +
  scale_x_continuous(labels=percent, expand=c(0.01, 0)) +
  scale_y_discrete(expand=c(0, 0)) +
  labs(x="Chapter Progression", y="Chapter",
       title="Sentiment Analysis",
       subtitle="Sentiment score as progressing through each chapter of Around the World in Eighty Days",
       caption="Source: Project Gutenberg") +
  guides(fill=guide_colorbar(barwidth=10, barheight=0.7)) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom")

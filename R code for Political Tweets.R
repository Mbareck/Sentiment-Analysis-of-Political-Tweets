library(qdap)
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(tm)

nwa <- read.csv(file.choose(), stringsAsFactors = FALSE)
malvern  <- read.csv(file.choose(), stringsAsFactors = FALSE)
jonesboro  <- read.csv(file.choose(), stringsAsFactors = FALSE)

#combine
news <-  bind_rows(
  jonesboro, nwa, malvern) %>%
  mutate(date = mdy(date)) %>%
  mutate(date= format(date, "%Y-%m")) %>%
  select(text, date, Newspaper)

###----------------------------
# news bigrams / frames
###-----------------------------

news_bigrmas  <- news %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(Newspaper, bigram) %>%
  group_by(Newspaper) %>%
  count(bigram, sort = T)

# remove non-sensical bigrams
library(tidyr)
library(stringr)
news_bigrams_separated <- news_bigrmas %>%
  separate(bigram, c("word1", "word2"), sep = " ")

news_bigrams_filtered <- news_bigrams_separated %>%
  filter(!word1 %in% c(stop_words$word, "sept")) %>%
  filter(!word2 %in% c(stop_words$word, "1")) 

news_bigram_counts <- news_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) 
  
news_bigram_counts

news_bigrams_united <- news_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>%
  arrange(desc(n)) %>%
  mutate(total = sum(n)) %>%
  mutate(pct = n /(total)) %>%
  top_n(4, pct)
news_bigrams_united 

# plot with percentage
library(scales)
ggplot(news_bigrams_united)+
  aes(x = reorder(bigram, pct), pct, fill = Newspaper) +
  geom_col() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Newspaper, scales = "free_y")+
  coord_flip()+
  theme_bw()+
  scale_y_continuous(labels = percent_format()) +
  #expand_limits(y = c(0.00, 1.00, 2.00, 3.00, 4.00))+
  labs(
    y = "Percentage",
    x = "Frame"
  ) 

--------------------------------------------------------#
  #                         polarity / sentiment
  #--------------------------------------------------------#
library(lexicon)
library(sentimentr)
library(qdap)
library(magrittr)
library(syuzhet)
#polarity using "bing" becuase it has more neg/pos words
tidy_news <- news %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

(bing_key <- as_key(syuzhet:::bing))
bing_key <- update_key(bing_key, x = data.frame(
  words = c('mourning', 'violence'),
  polarity = c(-1, -1),
  stringsAsFactors = FALSE
)
)
#polarity without stemming using bing
(news_pol <- tidy_news %$% polarity(word, Newspaper, polarity.frame = bing_key, constrain = T))
plot(news_pol)
plot(scores(news_pol))
news_P_scores <- as.data.frame(scores(news_pol))
###


#------------------------------------------------#
#              Emotions
#------------------------------------------------#


#Emotions
news_scores <- tidy_news %>%
  group_by(Newspaper)%>%
  # Inner join to lexicon
  inner_join(get_sentiments("nrc")) %>% 
  # Drop positive or negative sentiments
  filter(!grepl("positive|negative", sentiment)) %>% 
  # Count by name and sentiment
  count(Newspaper, sentiment) %>%
  mutate(n = n/sum(n)) 
news_scores$n <- round(news_scores$n, 2)
news_scores <- news_scores %>%
  spread(Newspaper, n)
#plot
library(radarchart)
chartJSRadar(news_scores, main = "NRC Emontional Categories",
             responsive = F, labelSize = 18, polyAlpha = 0, lineAlpha = 0.5 )

###-----------------------------
              # setinemtn over time
###---------------------------------

newsEmotions_time <- tidy_news %>%
  inner_join(get_sentiments("bing")) %>%
  count(Newspaper, date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
#plot
ggplot(newsEmotions_time) +
  aes(x= date, y=sentiment, group=Newspaper, color=Newspaper) +
  geom_line()+
  theme_bw()+
  facet_wrap(~Newspaper, ncol = 1, scales = "free")+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5))+
  labs(
    x = "Time",
    y = "Sentiment"
  )




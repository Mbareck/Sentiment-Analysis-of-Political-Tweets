# load libs
library(qdap)
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(tm)

Col <- read.csv("Data/CollinsArchives.csv", stringsAsFactors = F)
Led <- read.csv("Data/LedingArchives.csv", stringsAsFactors = F)
Gar <- read.csv("Data/GarnerArchives.csv", stringsAsFactors = F)

Collins_gun_Tweets <- filter(Col, grepl ("gun|ccl|chcl|guns|murder|death|killer|violence|act562|act589|carry", text, ignore.case = TRUE))
Leding_gun_Tweets <- filter(Led, grepl ("gun|ccl|chcl|guns|murder|death|killer|violence|act562|act589|carry", text, ignore.case = TRUE))
Garner_gun_Tweets <- filter(Gar, grepl ("gun|ccl|chcl|guns|murder|death|killer|violence|act562|act589|carry", text, ignore.case = TRUE))
Collins_gun_Tweets$text[1:5]

##combine all tweets by rows
Collins_gun_Tweets <- Collins_gun_Tweets[-1]
all_tweets <- bind_rows(Collins_gun_Tweets %>% 
                          mutate(Politician = "Collins"),
                        Garner_gun_Tweets %>%
                          mutate(Politician = "Garner"),
                        Leding_gun_Tweets %>% 
                              mutate(Politician = "Leding")) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(created_at= month(created_at, label = T)) 

# Clean and tokenize the data and create a word colonm 
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tidy_tweets  <- all_tweets %>%
    filter(!str_detect(text, '^"')) %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

view(tidy_tweets$word)

#--------------------------------------------------------#
#                         Measure polarity              https://cran.r-project.org/web/packages/sentimentr/sentimentr.pdf
#--------------------------------------------------------#
library(lexicon)
library(sentimentr)
library(qdap)
library(magrittr)
library(syuzhet)
# create a bing key for polarity
(bing_key <- as_key(syuzhet:::bing))
bing_key <- update_key(bing_key, x = data.frame(
  words = c('mourning', 'violence'),
  polarity = c(-1, -1),
  stringsAsFactors = FALSE
)
)

#calculate polarity 
(pol <- tidy_tweets %$% polarity(word, Politician, polarity.frame = bing_key, constrain = T))

plot(pol, low = "blue", high = "red")
plot(scores(pol))
#polarity scores as a data frame
p_counts <- as.data.frame(counts(pol))
p_scores <- as.data.frame(scores(pol))

###-------------------------
                    #find each tweet's sentiment using zyushet, method "bing"
###-------------------------------

#get tweets as a character vector of sentences 
Collins_gun_Tweets <- as.character(Collins_gun_Tweets$text)
Leding_gun_Tweets <- as.character(Leding_gun_Tweets$text)
Garner_gun_Tweets <- as.character(Garner_gun_Tweets$text)
head(Col_gun_Tweets)
#get sentiment for each politician
Gar_sentiment <- get_sentiment(Garner_gun_Tweets, method = "bing")
# combine tweets with  sentiment
sent_polarity <- data.frame(Garner_gun_Tweets, Gar_sentiment)
#animation

#--------------------------------------------------------#
#             calculate common words
#--------------------------------------------------------#
# At this stage, I need to stem the words to aggregate plurals, singulars and other word's forms

stemWord <- tidy_tweets$word #this will work as a word completion dictioinary after stemming


#repeat again in case you want to stem
tidy_tweets  <- all_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>%
  mutate(word = stemDocument(word)) %>% # here we stem the words
  mutate(word = stemCompletion(word, dictionary = stemWord, type = "shortest"))

words <- tidy_tweets %>%
  group_by(Politician) %>%
  filter(!str_detect(word, '#|@|arkansas')) %>%
  count(Politician, word, sort = T) %>%
  arrange(desc(n)) %>%
  mutate(total = sum(n)) %>%
  mutate(pct = n /(total)) %>%
  top_n(5, pct)
  
# make the percent colunm show %
words$pct <- scales::percent(words$pct)
View(words)

# plot top words percent
ggplot(words)+
  aes(x = word, y=pct, fill = Politician)+
  geom_col(position = position_dodge(preserve = "total")) +
  theme_bw()+
  geom_text(aes(label = pct), check_overlap = TRUE, color = "black", vjust = 0.15)+
  labs(
    y = "Percentage",
    x = "Word"
  )
# plot top words count
ggplot(words)+
  aes(x = word, y=n, fill = Politician)+
  geom_col(position = position_dodge(preserve = "total")) +
  geom_text(aes(label = n), check_overlap = TRUE, color = "black", vjust = 0.15)+
  expand_limits(y = c(15, 100))+
  labs(
    y = "Count",
    x = "Word"
  )+
  theme_bw()

#----------------------------------------------------------------#
# word comparison   https://www.tidytextmining.com/tidytext.html
#----------------------------------------------------------------#

# at this point, i stemmed the tweets to aggregate words to avoid overlapping 
stemWord <- tidy_tweets$word #this will work as dictioinary for stemming


#repeat again in case you want to stem
tidy_tweets  <- all_tweets %>%
filter(!str_detect(text, '^"')) %>%
mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>%
  mutate(word = stemDocument(word)) %>% # here we stem the words
  mutate(word = stemCompletion(word, dictionary = stemWord, type = "shortest"))

# calculate words freqs
frequency <- tidy_tweets %>%
  group_by(Politician) %>%
  filter(!str_detect(word, '#|@')) %>%
  count(Politician, word, sort = T) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Politician, proportion) %>% 
  gather(Politician, proportion, `Leding`:`Garner`) 

library(scales)
ggplot(frequency, aes(x = proportion, y = `Collins`, color = abs(`Collins` - proportion))) +
  geom_abline(color = "black", lty = 3) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkgreen", high = "brown") +
  facet_wrap(~Politician, ncol = 2) +
  theme_bw()+
  theme(legend.position="none") +
  labs(y = "Collins", x = "Word's Percentage")



##-----------------------------------## 
##          calculate Emotions
##-----------------------------------------##

scores <- tidy_tweets %>%
  group_by(Politician)%>%
  # Inner join to lexicon
  inner_join(get_sentiments("nrc")) %>% 
  # Drop positive or negative sentiments
  filter(!grepl("positive|negative", sentiment)) %>% 
  # Count by name and sentiment
  count(Politician, sentiment) %>%
  mutate(n = n/sum(n)) 
scores$n <- round(scores$n, 2)
scores <- scores %>%
  spread(Politician, n)
#plot
library(radarchart)
chartJSRadar(scores, main = "NRC Emontional Categories",
             responsive = F, labelSize = 18, polyAlpha = 0, lineAlpha = 0.5 )

###-----------------------------------
                     #sentiment over time
###--------------------------------------

# this is done without stemming to keep original word order for a better sentiment detection
# go back and create a new tidy tweets without stemming

sent_time <- tidy_tweets %>%
  select(Politician, created_at, word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Politician, created_at, sentiment) %>%
  spread(sentiment, n, fill = Inf) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
  ggplot(sent_time) +
  aes(x= created_at, y=sentiment, group=Politician, color=Politician) +
  geom_line()+
  geom_smooth()+
  facet_wrap(~Politician, ncol = 1, scales = "free")+
    labs(
      x = "Year: 2018",
      y = "Sentiment"
    )+
  theme_bw()



 
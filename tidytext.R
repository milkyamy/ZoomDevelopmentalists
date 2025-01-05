library(readxl)
text <- read_excel("D:/Amy Su/Cornell/Graduate research/1 - Researching children in the digital age/2_Data/text.xlsx")
View(text)

#TidyTextDataset(p.2-3)#

library(dplyr)
text_df <- data_frame(line=1:29, text = text)
text_df

#Tokenization(p.3)#

library(tidytext)
text_df %>%  
  unnest_tokens(word, text)

#WordFrequencies(p.6-7)#

dim(text_df[text_df$word=="online",])


data("stop_words")

text_df <- text_df %>%
  anti_join(stop_words)

text_df %>%
  count(word, sort = TRUE)

library(ggplot2)

text_df %>%
  count(word, sort = TRUE) %>% View()
  filter(n > 20) %>%
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#Wordclouds(p.25)#
library(wordcloud)

text_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Wordclouds+SentimentAnalysis(p.26)#

library(reshape2)

text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

#SentimentAnlysis(p.17)#
library(textdata)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_pos <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

nrc_neg <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

text_df %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
  
text_df %>%
  inner_join(nrc_pos) %>%
  count(word, sort = TRUE)

text_df %>%
  inner_join(nrc_neg) %>%
  count(word, sort = TRUE)
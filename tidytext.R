library(readxl)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(textdata)

text <- read_excel("C:/Users/Amy Su/Desktop/Amy Su/Research/1 - Researching children in the digital age/2_Data/text.xlsx")
View(text)

#TidyTextDataset(p.2-3)#
#text_df <- tibble(line=1:29, text = text)#
#text_df#

#Tokenization(p.3)#

text_df <- text %>%
  unnest_tokens(word, text)

#WordFrequencies(p.6-7)#

dim(text_df[text_df$word=="online",])

data("stop_words")

text_df <- text_df %>%
  anti_join(stop_words)

text_df %>%
  count(word, sort = TRUE)

text_df %>%
  count(word, sort = TRUE) %>% View()
  filter(n > 20) %>%
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#Wordclouds(p.25)#

custom_stop_words <- tibble(word = c("e.g", "i.e"))  
all_stop_words <- stop_words %>%
  bind_rows(custom_stop_words)

text_df_clean <- text_df %>%
  anti_join(all_stop_words, by = "word")

text_df %>%
    anti_join(all_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 80))

#Wordclouds+SentimentAnalysis(p.26)#

text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80,
                   scale = c(3, 0.5),         # reduce font size
                   random.order = FALSE)      # center big words first

#SentimentAnlysis(p.17)#

#nrc_joy <- get_sentiments("nrc") %>%#
  #filter(sentiment == "joy")#

#nrc_pos <- get_sentiments("nrc") %>%# 
  #filter(sentiment == "positive")#

#nrc_neg <- get_sentiments("nrc") %>%# 
  #filter(sentiment == "negative")#

text_df %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
  
text_df %>%
  inner_join(nrc_pos) %>%
  count(word, sort = TRUE)

text_df %>%
  inner_join(nrc_neg) %>%
  count(word, sort = TRUE)
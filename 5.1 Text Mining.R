library(tidytext)
library(dplyr)
library(ggplot2)

#### Setting Up Workspace ####

rm(list = ls())
gc()

setwd("C:/Users/Eric/Desktop/Data for Class/")
#setwd("C:/Users/yukev/Documents")





#### Intro to dplyr ####

head(mtcars) 

mtcars <- mutate(mtcars, id = seq(from = 1, to = nrow(mtcars), by = 1) )

test <- mtcars
test <- filter(test, mpg > 20)
test <- select(test, id, mpg, cyl)
test <- mutate(test, mpg_rounded = round(mpg))

test <- mtcars %>%
  filter(mpg > 20) %>%
  select(id, mpg, cyl) %>%
  mutate(mpg_rounded = round(mpg))

test <- test %>%
  select(-cyl, -mpg) %>%
  left_join(mtcars, by = "id")

rm(test, mtcars)
gc()





#### Setting Up Data ####

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, score)

stopwords <- stop_words %>%
  filter(lexicon == "snowball") %>%
  select(-lexicon)

reviews <- read.csv(file = "yelp_reviews.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
reviews$date <- as.POSIXct(reviews$date, format = "%Y-%m-%d")

str(AFINN)
str(stopwords)
str(reviews)

head(AFINN)
head(stopwords)
head(reviews, 1)





#### Analyzing Reviews ####

qplot(reviews$stars, geom = 'histogram')

#let's take a look at Las Vegas, Pittsburgh, and Black Canyon City
temp <- reviews %>%
  filter(city == "Pittsburgh")
qplot(temp$stars, geom = 'histogram')

#let's take a look at good vs bad businesses
temp <- reviews %>%
  filter(business_rating > 3)
qplot(temp$stars, geom = 'histogram')





#### Setting Up Words ####

words <- reviews %>%
  unnest_tokens(output = word, input = text, token = "words")%>%
  anti_join(stopwords, by = "word") %>%
  left_join(AFINN, by = "word")





#### Word Frequency ####

#looking at the frequency of words
word_frequency <- words %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#looking at the frequency of words in good vs bad reviews
word_frequency_good <- words %>%
  filter(stars > 3) %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
word_frequency_bad <- words %>%
  filter(stars < 3) %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#looking at words that only appear in good vs bad reviews
good <- word_frequency_good %>%
  anti_join(word_frequency_bad, by = "word") %>%
  arrange(desc(count))
bad <- word_frequency_bad %>%
  anti_join(word_frequency_good, by = "word") %>%
  arrange(desc(count))

rm(word_frequency, word_frequency_bad, word_frequency_good, good, bad)
gc()

#looking at word frequency vs the average score it is used in
words_frequency_by_score <- words %>%
  group_by(word) %>%
  summarise(count = n(), avg_stars = mean(stars)) %>%
  filter(count > 100, count < 10000)

ggplot(words_frequency_by_score, aes(count, avg_stars)) + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  geom_hline(yintercept = (mean(words_frequency_by_score$avg_stars) + sd(words_frequency_by_score$avg_stars)), color = "red", lty = 2) +
  geom_hline(yintercept = (mean(words_frequency_by_score$avg_stars) - sd(words_frequency_by_score$avg_stars)), color = "red", lty = 2)





#### Word Sentiment ####

reviews_sentiment <- words %>%
  filter(!is.na(score)) %>%
  group_by(review_id) %>%
  summarise(avg_sentiment = mean(score)) %>%
  ungroup() %>%
  inner_join(reviews, by = "review_id")

ggplot(reviews_sentiment, aes(stars, avg_sentiment, group = stars)) + 
  geom_boxplot()


#looking at avg_stars by frequency, coloured by sentiment
words_freq_star_score <- words_frequency_by_score %>%
  inner_join(AFINN, by = "word")

ggplot(words_freq_star_score, aes(count, avg_stars, color = score)) + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  geom_hline(yintercept = (mean(words_freq_star_score$avg_stars) + sd(words_freq_star_score$avg_stars)), color = "red", lty = 2) +
  geom_hline(yintercept = (mean(words_freq_star_score$avg_stars) - sd(words_freq_star_score$avg_stars)), color = "red", lty = 2) +
  scale_color_gradient2(low = "red", high = "blue", midpoint = 0, mid = "gray")


#looking at avg_stars by frequency, coloured by sentiment for low sentiment words
words_freq_star_score_good <- words_freq_star_score %>%
  filter(score > 0)

ggplot(words_freq_star_score_good, aes(count, avg_stars, color = score)) + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = FALSE, vjust = 1, hjust = 1) +
  geom_hline(yintercept = (mean(words_freq_star_score_good$avg_stars) + sd(words_freq_star_score_good$avg_stars)),
             color = "red", lty = 2) +
  geom_hline(yintercept = (mean(words_freq_star_score_good$avg_stars) - sd(words_freq_star_score_good$avg_stars)),
             color = "red", lty = 2) +
  scale_x_log10()



words_freq_star_score_bad <- words_freq_star_score %>%
  filter(score < 0)

ggplot(words_freq_star_score_bad, aes(count, avg_stars, color = score)) + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = FALSE, vjust = 1, hjust = 1) +
  geom_hline(yintercept = (mean(words_freq_star_score_bad$avg_stars) + sd(words_freq_star_score_bad$avg_stars)),
             color = "red", lty = 2) +
  geom_hline(yintercept = (mean(words_freq_star_score_bad$avg_stars) - sd(words_freq_star_score_bad$avg_stars)),
             color = "red", lty = 2) +
  scale_x_log10()
  #scale_color_gradient2(low = "red", high = "blue", midpoint = 0, mid = "gray")





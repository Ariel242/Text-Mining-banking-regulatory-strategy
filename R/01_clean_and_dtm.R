## 01_clean_and_dtm.R
## Text cleaning, corpus creation, unigrams/bigrams DTM

library(tm)
library(textstem)
library(stringi)
library(qdap)
library(remotes)
library("ldatuning")
library(Rmpfr)
library(topicmodels)
library(RWeka)
library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)
library(LDAvis)
library(servr)




strategy_db$text <- gsub("http[^\\s]+|www[^\\s]+", " ", strategy_db$text)
strategy_db$text <- gsub("\\S+@\\S+", " ", strategy_db$text)
strategy_db$text <- gsub("[^[:alnum:] ]", " ", strategy_db$text)
strategy_db$text <- gsub("\\s+", " ", strategy_db$text)
strategy_db$text <- trimws(strategy_db$text)

texts <- data.frame(
  doc_id = seq_len(nrow(strategy_db)),
  text = strategy_db$text,
  stringsAsFactors = FALSE
)

strategy_db <- VCorpus(DataframeSource(texts))
class(strategy_db)

strategy_db <- tm_map(strategy_db, content_transformer(tolower))
strategy_db <- tm_map(strategy_db, content_transformer(lemmatize_strings))
custom.stopwords <- c(stopwords("english"), "e", "s", "g", "c", "will", "also", "may", "can",
                      "must", "include","pra", "cfpb", "apra", "use", "ensure",
                      "make", "consider", "need")
strategy_db <- tm_map(strategy_db, removeWords, custom.stopwords)
strategy_db <- tm_map(strategy_db, removePunctuation)
strategy_db <- tm_map(strategy_db, removeNumbers)
strategy_db <- tm_map(strategy_db, stripWhitespace)

DTM <- DocumentTermMatrix(strategy_db)
DTM
dim(DTM)

inspect(DTM[1:10, 1:20])

unigram_freq <- colSums(as.matrix(DTM))
unigram_freq <- sort(unigram_freq, decreasing = TRUE)
head(unigram_freq, 200)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm_bigram <- DocumentTermMatrix(strategy_db, control = list(tokenize = BigramTokenizer))

bigram_freq <- colSums(as.matrix(dtm_bigram))
bigram_freq <- sort(bigram_freq, decreasing = TRUE)
head(bigram_freq, 200)

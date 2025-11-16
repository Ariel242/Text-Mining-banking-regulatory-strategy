## 02_visualizations_tfidf.R
## Unigram/bigram plots and TF-IDF visualizations

library(tm)
library(RWeka)
library(ggplot2)
library(ggthemes)

#================#
#  visualization #
#================#

unigram_df <- data.frame(word = names(unigram_freq), frequency = unigram_freq)
unigram_df$word <- factor(unigram_df$word, levels = unigram_df$word)
unigram_df$word <- factor(unigram_df$word, levels = rev(unigram_df$word))

ggplot(unigram_df[1:50, ], aes(x = word, y = frequency)) +
  geom_bar(stat = "identity", fill = "firebrick", width = 0.2) +
  geom_point(aes(x = word, y = frequency), color = "black", size = 3) +
  coord_flip() +
  theme_minimal(base_size = 4) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(title = "Top 50 Unigrams", x = "Word", y = "Frequency") +
  geom_text(aes(label = frequency), hjust = -0.6 , color = "black", size = 3.5)

# Bigrams

bigram_df <- data.frame(word = names(bigram_freq), frequency = bigram_freq)
bigram_df$word <- factor(bigram_df$word, levels = bigram_df$word)
bigram_df$word <- factor(bigram_df$word, levels = rev(bigram_df$word))

ggplot(bigram_df[1:50, ], aes(x = word, y = frequency)) +
  geom_bar(stat = "identity", fill = "firebrick", width = 0.2) +
  geom_point(aes(x = word, y = frequency), color = "black", size = 3) +
  coord_flip() +
  theme_minimal(base_size = 4) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(title = "Top 50 Bigrams", x = "Bigram", y = "Frequency") +
  geom_text(aes(label = frequency), hjust = -0.6, color = "black", size = 3.5)

#===========#
#   TF-IDF  #
#===========#

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

dtm_bigram_tfidf <- DocumentTermMatrix(strategy_db, control = list(
  tokenize = BigramTokenizer,
  weighting = weightTfIdf
))

tfidf_mat <- as.matrix(dtm_bigram_tfidf)
bigram_tfidf_scores <- colSums(tfidf_mat)
bigram_tfidf_scores <- sort(bigram_tfidf_scores, decreasing = TRUE)

bigram_tfidf_df <- data.frame(word = names(bigram_tfidf_scores),
                              score = bigram_tfidf_scores)
bigram_tfidf_df$word <- factor(bigram_tfidf_df$word, levels = rev(bigram_tfidf_df$word))

ggplot(bigram_tfidf_df[1:50, ], aes(x = word, y = score)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.2) +
  geom_point(color = "black", size = 3) +
  coord_flip() +
  theme_minimal(base_size = 4) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(title = "Top 50 Bigrams by TF-IDF", x = "Bigram", y = "TF-IDF Score") +
  geom_text(aes(label = round(score, 3)), hjust = -0.6, color = "black", size = 3.5)

# UNIGRAMS TF-IDF

dtm_unigram_tfidf <- DocumentTermMatrix(strategy_db, control = list(
  weighting = weightTfIdf
))

tfidf_uni_mat <- as.matrix(dtm_unigram_tfidf)
unigram_tfidf_scores <- colSums(tfidf_uni_mat)
unigram_tfidf_scores <- sort(unigram_tfidf_scores, decreasing = TRUE)

unigram_tfidf_df <- data.frame(
  word = names(unigram_tfidf_scores),
  score = unigram_tfidf_scores
)
unigram_tfidf_df$word <- factor(unigram_tfidf_df$word, levels = rev(unigram_tfidf_df$word))

ggplot(unigram_tfidf_df[1:50, ], aes(x = word, y = score)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.2) +
  geom_point(color = "black", size = 3) +
  coord_flip() +
  theme_minimal(base_size = 4) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(title = "Top 50 Unigrams by TF-IDF", x = "Word", y = "TF-IDF Score") +
  geom_text(aes(label = round(score, 3)), hjust = -0.6, color = "black", size = 3.5)

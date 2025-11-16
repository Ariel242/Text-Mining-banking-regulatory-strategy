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
custom.stopwords <- c(stopwords("english"), "e", "s", "g", "c", "will", "also", "may", "can", "must", "include","pra", "cfpb", "apra", "use", "ensure", "make", "consider", "need")
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

#================#
#  visualization #
#                #
#  ויזואליזציה   #
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


# Bigrams#

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

bigram_tfidf_df <- data.frame(word = names(bigram_tfidf_scores), score = bigram_tfidf_scores)
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


#UNIGRAMS#

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

#==================#


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


#==================#
#  Topic Modeling  #
#==================#


result <- FindTopicsNumber(
  DTM,
  topics = seq(1, 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 10, alpha = 0.1),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)


lda <- LDA(DTM, k = 7, control = list(seed = 10, alpha = 0.1), method = "Gibbs")
termsByTopic <- terms(lda, 20)
termsByTopic

# פוסטריור – התפלגות נושאים ומילים
posterior(lda)$topics[1:10,]  # עשרת המסמכים הראשונים
posterior(lda)$terms[,1:10]   # עשרת המונחים הראשונים

#########

topics <- topics(lda)

topics.labeled <- recode(topics,
                         '1' = 'Banking Supervision & Strategy',
                         '2' = 'Regulation & Prudential Policy',
                         '3' = 'Financial Services & Markets',
                         '4' = 'Consumer Protection & Law',
                         '5' = 'Agencies & Community Engagement',
                         '6' = 'Capital & Liquidity Management',
                         '7' = 'Risk Assessment & Resolution'
)

table(topics.labeled)

#=========#
# LDA-vis #
#=========#

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]],
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

json_vis <- topicmodels2LDAvis(lda)

# הצגה 
serVis(json_vis)

output_dir <- "C:/Users/ariel/Desktop/TM-Work/lda_vis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
serVis(json_vis, out.dir = output_dir, open.browser = FALSE)

#====================#
# Country Comparison #
#                    #
# השוואה בין מדינות  #
#====================#

.orig <- read.csv("Textual Corpus – Strategy Documents.csv",
                  stringsAsFactors = FALSE, fileEncoding = "latin1")


.country_candidates <- tolower(c("Country","COUNTRY","country","מדינה",
                                 "Jurisdiction","State","AuthorityCountry"))
.colmap <- tolower(names(.orig))
.country_pos <- which(.colmap %in% .country_candidates)
stopifnot(length(.country_pos) >= 1)
country_vec <- .orig[[ .country_pos[1] ]]
country_vec[is.na(country_vec) | trimws(country_vec)== ""] <- "UNKNOWN"

# אימות יישור לשורות ה-DTM/ביגרם)

stopifnot(length(country_vec) == dim(DTM)[1], dim(DTM)[1] == dim(dtm_bigram)[1])

# פונקציית עזר קטנה להפקת טבלת טופ-N "מונחים-בעמודות-מדינות"

.build_topN_table <- function(mat_rowsum, N = 20) {
  countries <- rownames(mat_rowsum)
  top_lists <- lapply(seq_len(nrow(mat_rowsum)), function(i){
    v <- mat_rowsum[i, ]
    v <- sort(v, decreasing = TRUE)
    head(names(v)[v > 0], N)
  })
  max_len <- max(lengths(top_lists))
  out <- data.frame(
    rank = seq_len(max_len),
    do.call(cbind, lapply(top_lists, function(x){ length(x) <- max_len; x })),
    check.names = FALSE
  )
  colnames(out)[-1] <- countries
  out
}

# יוניגרמים: אגרגציה לפי מדינה והפקת טופ-20 
mat_uni <- as.matrix(DTM)
agg_uni <- rowsum(mat_uni, group = factor(country_vec))
top20_unigrams_by_country <- .build_topN_table(agg_uni, N = 20)


# ביגרמים: אגרגציה לפי מדינה והפקת טופ-20 
mat_bi <- as.matrix(dtm_bigram)
agg_bi <- rowsum(mat_bi, group = factor(country_vec))
top20_bigrams_by_country <- .build_topN_table(agg_bi, N = 20)


# Topic Modeling: לפי מדינה

theta <- topicmodels::posterior(lda)$topics   
theta_by_country <- rowsum(theta, group = factor(country_vec))
theta_by_country <- sweep(theta_by_country, 1, rowSums(theta_by_country), FUN = "/")


# תצוגה מהירה
print(head(top20_unigrams_by_country, 20))
print(head(top20_bigrams_by_country, 20))
print(theta_by_country)

#==============================#

# Table of topics by countries #

#  טבלת נושאים לפי מדינות      #

#==============================#


topic_names <- c(
  "Banking Supervision & Strategy",
  "Regulation & Prudential Policy",
  "Financial Services & Markets",
  "Consumer Protection & Law",
  "Agencies & Community Engagement",
  "Capital & Liquidity Management",
  "Risk Assessment & Resolution"
)

stopifnot(exists("theta_by_country"))
stopifnot(ncol(theta_by_country) == length(topic_names))

# שמות לעמודות

colnames(theta_by_country) <- topic_names

#  מדינה + נושאים

theta_tbl <- data.frame(
  Country = rownames(theta_by_country),
  round(theta_by_country, 3),
  check.names = FALSE,
  row.names = NULL
)

print(theta_tbl)

# גרסת אחוזים
theta_tbl_pct <- data.frame(
  Country = rownames(theta_by_country),
  round(theta_by_country * 100, 1),
  check.names = FALSE,
  row.names = NULL
)

print(theta_tbl_pct)

#=========#
# מפת חום #
#=========#

stopifnot(exists("theta_by_country"))

if (!exists("topic_names")) {
  topic_names <- c(
    "Banking Supervision & Strategy",
    "Regulation & Prudential Policy",
    "Financial Services & Markets",
    "Consumer Protection & Law",
    "Agencies & Community Engagement",
    "Capital & Liquidity Management",
    "Risk Assessment & Resolution"
  )
}
if (is.null(colnames(theta_by_country)) || all(grepl("^[0-9]+$", colnames(theta_by_country)))) {
  stopifnot(ncol(theta_by_country) == length(topic_names))
  colnames(theta_by_country) <- topic_names
}

if (!exists("hm_df")) {
  hm_df <- as.data.frame(as.table(theta_by_country))
  colnames(hm_df) <- c("Country", "Topic", "Share")
  hm_df$Country <- factor(hm_df$Country, levels = rownames(theta_by_country))
  hm_df$Topic   <- factor(hm_df$Topic,   levels = colnames(theta_by_country))
  hm_df$label   <- sprintf("%.1f%%", hm_df$Share * 100)
}

# פונקציית גלישת טקסט
wrap_labels <- function(x, width = 22) {
  sapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"))
}


p_heat_top_wrapped <- ggplot(hm_df, aes(x = Topic, y = Country, fill = Share)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Share (θ)") +
  scale_x_discrete(position = "top", labels = function(x) wrap_labels(x, width = 22)) +  # שמות נושאים למעלה + גלישה
  labs(title = "Topic Mix by Country (LDA)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, face = "bold"),  # ישר ובמודגש
    axis.text.y = element_text(face = "bold"),
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  ) +
  coord_cartesian(clip = "off")

print(p_heat_top_wrapped)











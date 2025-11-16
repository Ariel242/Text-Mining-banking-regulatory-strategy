## 04_country_comparison_heatmap.R
## Country-level comparison of topics and heatmap

library(stringr)
library(ggplot2)

.orig <- read.csv("Textual Corpus – Strategy Documents.csv",
                  stringsAsFactors = FALSE, fileEncoding = "latin1")

.country_candidates <- tolower(c("Country","COUNTRY","country","מדינה",
                                 "Jurisdiction","State","AuthorityCountry"))
.colmap <- tolower(names(.orig))
.country_pos <- which(.colmap %in% .country_candidates)
stopifnot(length(.country_pos) >= 1)
country_vec <- .orig[[ .country_pos[1] ]]
country_vec[is.na(country_vec) | trimws(country_vec)== ""] <- "UNKNOWN"

# אימות יישור לשורות ה-DTM/ביגרם
stopifnot(length(country_vec) == dim(DTM)[1], dim(DTM)[1] == dim(dtm_bigram)[1])

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

# יוניגרמים
mat_uni <- as.matrix(DTM)
agg_uni <- rowsum(mat_uni, group = factor(country_vec))
top20_unigrams_by_country <- .build_topN_table(agg_uni, N = 20)

# ביגרמים
mat_bi <- as.matrix(dtm_bigram)
agg_bi <- rowsum(mat_bi, group = factor(country_vec))
top20_bigrams_by_country <- .build_topN_table(agg_bi, N = 20)

# Topic Modeling לפי מדינה
theta <- topicmodels::posterior(lda)$topics
theta_by_country <- rowsum(theta, group = factor(country_vec))
theta_by_country <- sweep(theta_by_country, 1, rowSums(theta_by_country), FUN = "/")

print(head(top20_unigrams_by_country, 20))
print(head(top20_bigrams_by_country, 20))
print(theta_by_country)

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

colnames(theta_by_country) <- topic_names

theta_tbl <- data.frame(
  Country = rownames(theta_by_country),
  round(theta_by_country, 3),
  check.names = FALSE,
  row.names = NULL
)
print(theta_tbl)

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

wrap_labels <- function(x, width = 22) {
  sapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"))
}

p_heat_top_wrapped <- ggplot(hm_df, aes(x = Topic, y = Country, fill = Share)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Share (θ)") +
  scale_x_discrete(position = "top", labels = function(x) wrap_labels(x, width = 22)) +
  labs(title = "Topic Mix by Country (LDA)", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  ) +
  coord_cartesian(clip = "off")

print(p_heat_top_wrapped)

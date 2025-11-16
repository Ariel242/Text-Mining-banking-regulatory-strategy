## 03_lda_topics.R
## LDA topic modeling and LDAvis

library(ldatuning)
library(topicmodels)
library(LDAvis)
library(ggplot2)
library(dplyr)

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

posterior(lda)$topics[1:10,]
posterior(lda)$terms[,1:10]   

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

serVis(json_vis)

output_dir <- "C:/Users/ariel/Desktop/TM-Work/lda_vis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
serVis(json_vis, out.dir = output_dir, open.browser = FALSE)

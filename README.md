# Comparing Banking Supervisory Strategies via Text Mining

This project applies text mining and topic modeling techniques to official strategy documents of banking regulators from several advanced economies. The goal is to systematically compare supervisory priorities, key risks, and strategic focus across countries based on the language used in their public strategy publications.

Originally, this project was submitted as a term paper in a Text Mining course at Bar-Ilan University (School of Business Administration).

---

## Research Questions

1. **Which topics repeatedly appear in the strategic documents of different banking regulators, and how does the emphasis on these topics vary across countries?**
2. **Can we identify common or unique patterns in how regulators define risks, priorities, and supervisory objectives?**

---

## Data

The corpus consists of English-language strategy and risk-outlook documents downloaded from the official websites of major banking supervisors, including (for example):

- UK – Prudential Regulation Authority (PRA)  
- Euro Area – European Central Bank (ECB) / SSM  
- US – Office of the Comptroller of the Currency (OCC)  
- US – Federal Deposit Insurance Corporation (FDIC)  
- Canada – Office of the Superintendent of Financial Institutions (OSFI)

Each row in the final dataset represents a section or thematic block within a strategy document, with the following key fields:

- `Global_ID` – unique identifier for each strategy text block  
- `Country` – country name  
- `Regulator` – name of the authority  
- `Year` – publication year  
- `Title` – section title or short description  
- `Text` – raw text of the section  
- `Doc_ID` – document identifier within a regulator

**Important:** Due to copyright and licensing considerations, the raw documents are *not* included in this repository. Instead, a short data guide explains how to reconstruct or download them from the regulators’ official websites.

---

## Methods

The analysis combines “classical” text mining with more modern embedding-based similarity:

### 1. Text Cleaning and Pre-processing

Using R and the `tm` ecosystem, the text is processed through:

- Removal of URLs and emails  
- Removal of non-letter characters and extra spaces  
- Lower-casing  
- Conversion of the `Text` column into a corpus  
- **Lemmatization** (preferred over stemming in order to preserve interpretability)  
- Removal of stopwords (generic + domain-specific, including regulator names)  
- Removal of punctuation and digits 

### 2. Feature Construction

- **Bag-of-Words (BoW)** representation with **unigrams** and **bigrams**  
- Construction of **Document-Term Matrices (DTM/TDM)** with raw counts  
- Vocabulary filtering via stopwords and lemmatization  
- Topic-based features: for each document, the LDA topic distribution (7 topics) is used as a numeric profile of thematic focus by country  
- Embedding-based features using **FinBERT**:
  - Sentence/document embeddings for each strategy block
  - Cosine similarity between countries
  - Hierarchical clustering of countries based on their embeddings

### 3. Topic Modeling (LDA)

- Latent Dirichlet Allocation (LDA) is applied on the unigram DTM  
- Multiple candidate values of *K* (number of topics) are evaluated using topic coherence measures following Röder et al. (2015)  
- **K=7** is selected as a balance between coherence, parsimony, and interpretability  
- Topics are interpreted using:
  - Top unigrams and bigrams per topic
  - LDAvis interactive visualization
  - Country-level topic distributions (heatmaps)

---


## Results and Interpretation

### Topic-level findings

The LDA model with **K = 7 topics** produced coherent and interpretable themes. At a high level, the topics can be grouped into four broad areas:

- **Prudential supervision and risk management** – capital, liquidity, stress testing, risk appetite, ICAAP/ILAAP.
- **Supervisory strategy and planning** – supervisory priorities, planning cycles, resource allocation, thematic reviews.
- **Financial services and market development** – competition, innovation, fintech, market structure, cross-border activity.
- **Consumer protection, conduct and community outcomes** – fair treatment, complaints handling, community obligations, financial inclusion.

Across regulators, prudential and risk-focused topics dominate the corpus, but the relative weight of consumer-oriented vs. prudential language varies by country.

### Heatmap: topic mix by country

A heatmap of topic shares by country highlights clear differences in strategic focus:

- **Euro area (ECB/SSM)** – Strong emphasis on *supervisory strategy and prudential risk*. The strategy documents focus heavily on capital, liquidity, stress testing and the coordination role of the SSM over a large number of banks.
- **United Kingdom (PRA/BoE)** – High weight on *risk management* and *financial services/markets*. The texts reflect both prudential stability and the goal of positioning the UK as an attractive, competitive and stable financial centre in the post-Brexit environment.
- **United States (OCC/FDIC/other agencies)** – Marked focus on *consumer protection, compliance and community obligations*. The documents refer frequently to fair treatment, access to banking services and community-oriented outcomes, including topics that appear less central for some of the other regulators.
- **Canada (OSFI)** – Mixed profile: *prudential stability* alongside *community and ESG-related themes*. Canada is the only jurisdiction in this sample that clearly emphasises diversity, climate-related risks and similar issues.
- **Australia (APRA)** – Concentrated on *prudential regulation and financial stability*, with relatively less explicit discussion of consumer conduct compared to North America.

Overall, all regulators devote substantial space to prudential risk and stability, but the balance between prudential focus and consumer/community focus differs systematically across jurisdictions.

### Embedding-based clustering (FinBERT)

To complement the topic model, FinBERT embeddings were computed for each strategy text and used to build a cosine-similarity matrix between documents. Hierarchical clustering on these embeddings shows:

- Overall cosine similarities are high (**0.94–0.97**), meaning the regulators largely talk about a similar universe of risks and supervisory themes.
- The most similar pair in this sample is **EU–Australia**, reflecting very similar prudential language.
- Two main blocks emerge:
  - **EU–UK–Australia** – a predominantly **prudential / risk-oriented block**, with strong focus on capital, liquidity, governance and traditional supervisory tools.
  - **US–Canada** – a more **consumer / community-oriented block**, with relatively higher weight on fairness, community obligations, inclusion and ESG-related themes.

This suggests that while all supervisors share a common base of prudential concerns, North-American authorities place more explicit emphasis on consumer outcomes and community impact, whereas European and Australian authorities emphasise prudential stability and supervisory processes.


---


## Repository Structure

A suggested structure for this repository:

```text
.
├─ R/                    # R scripts (data loading, cleaning, LDA, plots)
│  ├─ 01_load_and_clean.R
│  ├─ 02_dtm_and_features.R
│  ├─ 03_lda_topics.R
│  └─ 04_visualizations.R
├─ python/               # Optional: FinBERT embedding script(s)
├─ data/
│  ├─ data_raw/          # (Not tracked) raw documents if licensing allows
│  └─ data_processed/    # Cleaned datasets / matrices ready for analysis
├─ docs/
│  ├─ report_hebrew.pdf  # Original course report (Hebrew)
│  └─ appendices.pdf     # Plots and tables (unigrams, bigrams, heatmaps…)
├─ figures/              # Exported PNG/PDF figures for the README
├─ README.md
├─ README_hebrew.md      # Optional Hebrew version of this README
├─ LICENSE
└─ .gitignore

# Credit Risk Scorecard

A production-style points-based credit scorecard built in R using Weight of Evidence binning and logistic regression.

![R](https://img.shields.io/badge/R-4.5-276DC3?logo=r&logoColor=white)
![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)

---

## Project Overview

This project builds an end-to-end credit risk scorecard on 150,000 consumer loan records from the *Give Me Some Credit* dataset. It follows the standard industry workflow — exploratory analysis, WoE binning, logistic regression, scorecard scaling, and threshold optimisation — producing a model that assigns each borrower an integer score where a higher score indicates lower default risk. Scorecards of this type are the backbone of retail lending decisions at banks and fintechs, valued for their auditability and regulatory transparency.

---

## Key Results

| Metric | Value |
|---|---|
| AUC | 0.853 |
| Gini coefficient | 0.706 |
| KS statistic | 54.8 pp |
| Optimal F1 cut-off | Score ≤ 555 |
| Decile-3 bad capture | **81% of all bads** caught by declining the bottom 30% of applicants by score |

---

## Project Structure

```
credit-risk-scorecard/
│
├── data/
│   ├── cs-training.csv       # Raw dataset (150k rows, semicolon-delimited)
│   ├── train.rds             # Preprocessed training set (70%)
│   └── test.rds              # Preprocessed test set (30%)
│
├── scripts/
│   ├── 01_load_and_explore.R # EDA: dimensions, summary stats, distributions, class balance
│   ├── 02_preprocessing.R    # Stratified split, median imputation, percentile winsorisation
│   ├── 03_scorecard_model.R  # WoE binning, IV filtering, logistic regression, scorecard scaling
│   ├── 04_evaluation.R       # AUC, Gini, KS, ROC curve, KS chart, gains table
│   ├── 05_report.Rmd         # R Markdown source for the full PDF report
│   └── 06_cutoff_analysis.R  # F1-optimal threshold search with precision/recall/F1 plot
│
└── output/
    ├── variable_distributions.png  # Histograms for all 11 variables
    ├── roc_curve.png               # ROC curve with AUC annotation
    ├── ks_chart.png                # KS chart by score decile
    ├── gains_table.png             # Formatted gains table (decile breakdown)
    ├── cutoff_analysis.png         # Precision, recall, F1 vs score threshold
    ├── scorecard.rds               # Fitted scorecard object
    ├── scored_test.rds             # Test set with per-variable and total scores
    └── 05_report.pdf               # Full model development report
```

---

## How to Run

> Requires R ≥ 4.0. Packages are installed automatically where not present.

1. **Clone the repo and open the project directory.**

2. **Place the raw data file** (`cs-training.csv`) in `data/`.  
   Download it from [Kaggle](https://www.kaggle.com/c/GiveMeSomeCredit/data).

3. **Run the scripts in order** from the project root:

   ```r
   source("scripts/01_load_and_explore.R")
   source("scripts/02_preprocessing.R")
   source("scripts/03_scorecard_model.R")
   source("scripts/04_evaluation.R")
   source("scripts/06_cutoff_analysis.R")
   ```

4. **Knit the PDF report** (requires pandoc — bundled with RStudio):

   ```r
   rmarkdown::render("scripts/05_report.Rmd", output_file = "../output/05_report.pdf")
   ```

All outputs are written to `output/`.

---

## Methods Used

- **Exploratory data analysis** — distribution plots, missing value audit, class balance check
- **Stratified train/test split** — 70/30 split preserving the 6.68% bad rate in both sets
- **Median imputation** — computed on the training set only and applied to both sets to prevent data leakage
- **Winsorisation** — outlier capping at the 1st/99th percentile using training-set percentiles
- **Weight of Evidence (WoE) binning** — monotonic transformation of all predictors; Information Value used to rank predictive strength
- **Logistic regression** — industry-standard algorithm for interpretable, auditable scorecards
- **Scorecard scaling** — raw log-odds converted to integer points (base score 600, PDO 20)
- **Threshold optimisation** — F1-optimal score cut-off identified by sweeping all 204 unique score values on the test set

---

## Dataset

*Give Me Some Credit* — Kaggle competition dataset (2011):  
<https://www.kaggle.com/c/GiveMeSomeCredit/data>

---

## Author

**Andy Zhang**  
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Connect-0A66C2?logo=linkedin&logoColor=white)](https://linkedin.com/in/your-profile)
[![GitHub](https://img.shields.io/badge/GitHub-Follow-181717?logo=github&logoColor=white)](https://github.com/your-username)

if (!requireNamespace("scorecard", quietly = TRUE)) {
  install.packages("scorecard", repos = "https://cloud.r-project.org")
}
library(scorecard)

# ── 1. Load preprocessed train / test sets ────────────────────────────────────
train <- readRDS("data/train.rds")
test  <- readRDS("data/test.rds")

target     <- "serious_dlq_in_2yrs"
predictors <- setdiff(names(train), target)

# ── 2. WoE binning on the training set ───────────────────────────────────────
# woebin() finds optimal cut-points that maximise the separation between
# good (0) and bad (1) borrowers. positive="1" confirms that class 1 is bad.
bins <- woebin(train, y = target, x = predictors,
               positive = "1", print_step = 0)

# ── 3. Print WoE tables and flag variables with IV < 0.02 ─────────────────────
# Extract one total-IV value per variable from the bin data frames
iv_df <- data.frame(
  variable = names(bins),
  iv       = sapply(bins, function(b) b$total_iv[[1]]),
  row.names = NULL
)
iv_df <- iv_df[order(-iv_df$iv), ]

cat("=== Information Values (sorted descending) ===\n")
iv_df$strength <- ifelse(
  iv_df$iv < 0.02,  "WEAK — consider dropping",
  ifelse(iv_df$iv < 0.10,  "Weak",
  ifelse(iv_df$iv < 0.30,  "Medium",
  ifelse(iv_df$iv < 0.50,  "Strong", "Very strong / check for leakage")))
)
print(iv_df, row.names = FALSE)
cat("\n")

cat("=== WoE bins per variable ===\n")
for (v in names(bins)) {
  cat("\n---", v, "---\n")
  b <- bins[[v]][, c("bin", "count", "count_distr", "woe", "bin_iv", "total_iv")]
  print(b, row.names = FALSE)
}
cat("\n")

# ── 4. Apply WoE transformation to train and test ─────────────────────────────
# woebin_ply() returns a data frame with the target column plus one <var>_woe
# column per predictor — ready to pass straight into glm().
train_woe <- woebin_ply(train, bins, print_step = 0)
test_woe  <- woebin_ply(test,  bins, print_step = 0)

# ── 5. Logistic regression on WoE-transformed predictors ──────────────────────
# The formula "~ ." picks up every _woe column; the target is the only
# non-_woe column in train_woe.
model <- glm(serious_dlq_in_2yrs ~ .,
             data   = train_woe,
             family = binomial(link = "logit"))

cat("=== Logistic regression summary ===\n")
print(summary(model))
cat("\n")

# ── 6. Convert logistic regression to a points-based scorecard ────────────────
# Scaling parameters:
#   points0 = 600  → base score (odds0 = 1:19 bad-to-good, ~5 % bad rate)
#   pdo     = 20   → every 20 points doubles the good-to-bad odds
card <- scorecard(bins, model, points0 = 600, odds0 = 1 / 19, pdo = 20)

cat("=== Scorecard points per bin ===\n")
for (v in names(card)) {
  cat("\n---", v, "---\n")
  print(card[[v]], row.names = FALSE)
}
cat("\n")

# ── 7. Score train and test sets ───────────────────────────────────────────────
# scorecard_ply() walks the original (non-WoE) data through the scorecard and
# returns a data frame with one column per variable's partial score plus a
# final "score" column that is their sum.
train_scored <- scorecard_ply(train, card, only_total_score = FALSE,
                               print_step = 0)
test_scored  <- scorecard_ply(test,  card, only_total_score = FALSE,
                               print_step = 0)

# Attach the true label for downstream evaluation
test_scored[[target]] <- test[[target]]

# ── 8. Print score distribution summary ───────────────────────────────────────
score_stats <- function(scored_df, label) {
  s <- scored_df$score
  cat(sprintf("%s — n=%d | min=%.0f | mean=%.1f | max=%.0f\n",
              label, length(s), min(s), mean(s), max(s)))
}
cat("=== Score distribution ===\n")
score_stats(train_scored, "Train")
score_stats(test_scored,  "Test ")

# ── 9. Save artefacts ─────────────────────────────────────────────────────────
saveRDS(card,         "output/scorecard.rds")
saveRDS(test_scored,  "output/scored_test.rds")
cat("\nSaved: output/scorecard.rds\n")
cat("Saved: output/scored_test.rds\n")

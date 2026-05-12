library(dplyr)

# ── 1. Load data ──────────────────────────────────────────────────────────────
df <- read.csv("data/cs-training.csv", sep = ";")

# ── 2. Rename columns to snake_case ───────────────────────────────────────────
df <- df |>
  rename(
    serious_dlq_in_2yrs   = SeriousDlqin2yrs,
    revolving_utilization  = RevolvingUtilizationOfUnsecuredLines,
    age                    = age,
    n_times_30_59_dpd      = NumberOfTime30.59DaysPastDueNotWorse,
    debt_ratio             = DebtRatio,
    monthly_income         = MonthlyIncome,
    n_open_credit_lines    = NumberOfOpenCreditLinesAndLoans,
    n_times_90_days_late   = NumberOfTimes90DaysLate,
    n_real_estate_loans    = NumberRealEstateLoansOrLines,
    n_times_60_89_dpd      = NumberOfTime60.89DaysPastDueNotWorse,
    n_dependents           = NumberOfDependents
  )

# ── 3. Remove the row-index column (first unnamed column from the CSV) ─────────
df <- df |> select(-any_of(c("X", "")))

# ── 4. Stratified 70/30 train/test split ──────────────────────────────────────
# Sample 70 % from each class independently so the class ratio is preserved.
set.seed(42)
train_idx <- unlist(lapply(unique(df$serious_dlq_in_2yrs), function(cls) {
  idx <- which(df$serious_dlq_in_2yrs == cls)
  sample(idx, size = floor(length(idx) * 0.7))
}))
train <- df[ train_idx, ]
test  <- df[-train_idx, ]

# Record missing-value counts BEFORE imputation for the summary
na_before_train <- colSums(is.na(train))
na_before_test  <- colSums(is.na(test))

# ── 5. Median imputation (medians computed on train only) ─────────────────────
numeric_cols <- names(train)[sapply(train, is.numeric)]
# Exclude the binary target — no imputation needed there
impute_cols  <- setdiff(numeric_cols, "serious_dlq_in_2yrs")

train_medians <- sapply(train[, impute_cols], median, na.rm = TRUE)

for (col in impute_cols) {
  train[[col]][is.na(train[[col]])] <- train_medians[[col]]
  test[[col]][is.na(test[[col]])]   <- train_medians[[col]]
}

# ── 6. Outlier capping at 1st / 99th percentile (train percentiles only) ──────
# Applied to every numeric predictor; preserves the target's 0/1 range.
for (col in impute_cols) {
  lo <- quantile(train[[col]], 0.01, na.rm = TRUE)
  hi <- quantile(train[[col]], 0.99, na.rm = TRUE)
  train[[col]] <- pmin(pmax(train[[col]], lo), hi)
  test[[col]]  <- pmin(pmax(test[[col]],  lo), hi)
}

# ── 7. Summary report ─────────────────────────────────────────────────────────
cat("=== Row counts ===\n")
cat(sprintf("  Train : %d rows\n", nrow(train)))
cat(sprintf("  Test  : %d rows\n", nrow(test)))

cat("\n=== Missing values BEFORE imputation ===\n")
cat("Train:\n"); print(na_before_train[na_before_train > 0])
cat("Test:\n");  print(na_before_test[na_before_test  > 0])

cat("\n=== Missing values AFTER imputation ===\n")
cat("Train:\n"); print(colSums(is.na(train)))
cat("Test:\n");  print(colSums(is.na(test)))

cat("\n=== Class distribution: serious_dlq_in_2yrs ===\n")
fmt_class <- function(x, label) {
  tbl <- table(x)
  pct <- round(prop.table(tbl) * 100, 2)
  cat(label, "\n")
  print(rbind(Count = as.integer(tbl), Percent = pct))
}
fmt_class(train$serious_dlq_in_2yrs, "Train:")
fmt_class(test$serious_dlq_in_2yrs,  "Test:")

# ── 8. Save processed sets ────────────────────────────────────────────────────
saveRDS(train, "data/train.rds")
saveRDS(test,  "data/test.rds")
cat("\nSaved: data/train.rds and data/test.rds\n")

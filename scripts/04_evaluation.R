library(ggplot2)
library(pROC)

# ── 1. Load scored test set ───────────────────────────────────────────────────
d      <- readRDS("output/scored_test.rds")
target <- "serious_dlq_in_2yrs"
n_bad  <- sum(d[[target]] == 1)
n_good <- sum(d[[target]] == 0)
n_tot  <- nrow(d)

# ── 2. Performance metrics ────────────────────────────────────────────────────

# AUC via pROC
# direction=">": controls (good, 0) have higher scores than cases (bad, 1)
roc_obj <- roc(response  = d[[target]],
               predictor = d$score,
               direction = ">",
               levels    = c(0, 1),
               quiet     = TRUE)
auc_val  <- as.numeric(auc(roc_obj))
gini_val <- 2 * auc_val - 1

# KS: maximum separation between cumulative bad and good distributions
# Sort observations from lowest score (highest risk) to highest
d_sorted     <- d[order(d$score), ]
cum_bad_obs  <- cumsum(d_sorted[[target]] == 1) / n_bad
cum_good_obs <- cumsum(d_sorted[[target]] == 0) / n_good
ks_stat      <- max(abs(cum_bad_obs - cum_good_obs))

cat("=== Model Performance Metrics ===\n")
cat(sprintf("  AUC  : %.4f\n", auc_val))
cat(sprintf("  Gini : %.4f\n", gini_val))
cat(sprintf("  KS   : %.4f\n\n", ks_stat))

# ── 3a. ROC curve ─────────────────────────────────────────────────────────────
# pROC orders points from high to low threshold; rev() walks (0,0) → (1,1)
roc_df <- data.frame(
  fpr = rev(1 - roc_obj$specificities),
  tpr = rev(roc_obj$sensitivities)
)

roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey60") +
  annotate("text", x = 0.58, y = 0.14,
           label = sprintf("AUC  = %.4f\nGini = %.4f", auc_val, gini_val),
           hjust = 0, size = 4, family = "mono") +
  labs(title = "ROC Curve — Test Set",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  theme_minimal(base_size = 12) +
  coord_fixed()

ggsave("output/roc_curve.png", roc_plot, width = 6, height = 6, dpi = 150)
cat("Saved: output/roc_curve.png\n")

# ── 3b. KS chart by score decile ──────────────────────────────────────────────
# Decile 1 = lowest score = highest risk; decile 10 = lowest risk
breaks  <- unique(quantile(d$score, probs = seq(0, 1, 0.1)))
d$decile <- as.integer(cut(d$score, breaks = breaks,
                            labels = FALSE, include.lowest = TRUE))

# Aggregate counts per decile and compute cumulative distributions
decile_tbl <- do.call(rbind, lapply(sort(unique(d$decile)), function(dec) {
  s <- d[d$decile == dec, ]
  data.frame(decile = dec,
             n      = nrow(s),
             n_bad  = sum(s[[target]] == 1),
             n_good = sum(s[[target]] == 0))
}))
decile_tbl$cum_bad_pct  <- cumsum(decile_tbl$n_bad)  / n_bad
decile_tbl$cum_good_pct <- cumsum(decile_tbl$n_good) / n_good
decile_tbl$ks_gap       <- abs(decile_tbl$cum_bad_pct - decile_tbl$cum_good_pct)

# Locate decile with maximum gap for annotation
ks_dec   <- decile_tbl$decile[which.max(decile_tbl$ks_gap)]
ks_y_bad <- decile_tbl$cum_bad_pct[decile_tbl$decile  == ks_dec]
ks_y_gd  <- decile_tbl$cum_good_pct[decile_tbl$decile == ks_dec]
ks_y_mid <- (ks_y_bad + ks_y_gd) / 2
# Keep annotation label inside the plot
txt_x <- if (ks_dec >= 8) ks_dec - 2.2 else ks_dec + 0.3

ks_long <- rbind(
  data.frame(decile  = decile_tbl$decile,
             cum_pct = decile_tbl$cum_bad_pct,  dist = "Bad (1)"),
  data.frame(decile  = decile_tbl$decile,
             cum_pct = decile_tbl$cum_good_pct, dist = "Good (0)")
)

ks_plot <- ggplot(ks_long, aes(x = decile, y = cum_pct, colour = dist)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  annotate("segment",
           x    = ks_dec, xend = ks_dec,
           y    = ks_y_gd, yend = ks_y_bad,
           colour = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = txt_x, y = ks_y_mid,
           label = sprintf("KS = %.4f", ks_stat),
           hjust = 0, size = 3.8, family = "mono") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("Bad (1)" = "firebrick", "Good (0)" = "steelblue")) +
  labs(title    = "KS Chart — Cumulative Distributions by Score Decile",
       subtitle = "Decile 1 = Lowest Score (Highest Risk)",
       x = "Score Decile", y = "Cumulative Proportion", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("output/ks_chart.png", ks_plot, width = 7, height = 5, dpi = 150)
cat("Saved: output/ks_chart.png\n")

# ── 3c. Gains table ───────────────────────────────────────────────────────────
gains <- data.frame(
  Decile   = decile_tbl$decile,
  Accounts = decile_tbl$n,
  Bads     = decile_tbl$n_bad,
  bad_rate = decile_tbl$n_bad / decile_tbl$n * 100,
  cum_capt = decile_tbl$cum_bad_pct * 100
)

# Build a long data frame of (x=column, y=row, text) for geom_text rendering
col_labels <- c("Decile", "Accounts", "Bads", "Bad Rate (%)", "Cum Bad Capture (%)")
n_dec <- nrow(gains)

# Header lives at y = n_dec + 1 (top); decile 1 at y = n_dec; decile 10 at y = 1
cells <- rbind(
  data.frame(x = 1:5, y = n_dec + 1,
             text      = col_labels,
             is_header = TRUE),
  do.call(rbind, lapply(seq_len(n_dec), function(i) {
    data.frame(
      x         = 1:5,
      y         = n_dec + 1 - i,
      text      = c(as.character(gains$Decile[i]),
                    format(gains$Accounts[i], big.mark = ","),
                    as.character(gains$Bads[i]),
                    sprintf("%.2f%%", gains$bad_rate[i]),
                    sprintf("%.2f%%", gains$cum_capt[i])),
      is_header = FALSE
    )
  }))
)

gains_plot <- ggplot(cells, aes(x = x, y = y)) +
  geom_tile(aes(fill = is_header), colour = "grey70", linewidth = 0.35,
            width = 0.98, height = 0.9) +
  geom_text(aes(label = text,
                fontface = ifelse(is_header, "bold", "plain")),
            size = 3.2) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#cfe2f3")) +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(title = "Gains Table by Score Decile  |  Decile 1 = Highest Risk") +
  theme_void(base_size = 11) +
  theme(legend.position  = "none",
        plot.title       = element_text(hjust = 0.5, face = "bold",
                                        margin = margin(b = 6)),
        plot.margin      = margin(10, 10, 10, 10))

ggsave("output/gains_table.png", gains_plot,
       width = 9, height = 4.5, dpi = 150)
cat("Saved: output/gains_table.png\n\n")

# ── 4. Plain-English interpretation ───────────────────────────────────────────
bad_capt_at_ks <- decile_tbl$cum_bad_pct[decile_tbl$decile  == ks_dec] * 100
good_capt_at_ks <- decile_tbl$cum_good_pct[decile_tbl$decile == ks_dec] * 100

cat("=== Model Performance Interpretation ===\n")
cat(strwrap(sprintf(
  paste(
    "The scorecard achieves an AUC of %.4f on the held-out test set of %s accounts,",
    "meaning that in %.1f%% of randomly drawn good-bad pairs the model correctly assigns",
    "the higher score to the non-defaulting borrower. The corresponding Gini coefficient",
    "of %.4f places the model firmly in the strong discriminatory range — retail credit",
    "scorecards are generally considered acceptable at Gini > 0.40 and strong above 0.50.",
    "The KS statistic of %.4f (%.1f percentage points of separation) is reached at score",
    "decile %d, where %.1f%% of all bad accounts have already been captured while only",
    "%.1f%% of good accounts have been included; this large lift at the high-risk end of",
    "the score distribution is exactly the pattern lenders rely on to set cut-off thresholds.",
    "Overall the model demonstrates solid rank-ordering ability, though performance is",
    "heavily driven by the four delinquency-history variables; teams should monitor",
    "Population Stability Index (PSI) over time and recalibrate if the observed bad rate",
    "shifts materially from the ~%.1f%% seen during development."
  ),
  auc_val,
  format(n_tot, big.mark = ","),
  auc_val * 100,
  gini_val,
  ks_stat,
  ks_stat * 100,
  ks_dec,
  bad_capt_at_ks,
  good_capt_at_ks,
  n_bad / n_tot * 100
), width = 90), sep = "\n")
cat("\n")

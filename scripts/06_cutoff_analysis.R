library(ggplot2)

# ── Load data ─────────────────────────────────────────────────────────────────
d      <- readRDS("output/scored_test.rds")
target <- "serious_dlq_in_2yrs"
actual <- d[[target]]
score  <- d$score

# ── Evaluate precision, recall, F1 at every unique score threshold ─────────────
# Decision rule: predict bad (1) when score <= threshold.
# Lower score = higher risk in a credit scorecard, so declining below the
# threshold captures bads at the cost of some good accounts.
thresholds <- sort(unique(score))

metrics <- do.call(rbind, lapply(thresholds, function(thr) {
  predicted <- as.integer(score <= thr)
  tp <- sum(predicted == 1 & actual == 1)
  fp <- sum(predicted == 1 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)

  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  f1        <- if (is.na(precision) || is.na(recall) ||
                   (precision + recall) == 0) NA_real_ else
               2 * precision * recall / (precision + recall)

  data.frame(threshold = thr, precision = precision,
             recall = recall, f1 = f1)
}))

# ── Find the threshold that maximises F1 ──────────────────────────────────────
best_idx <- which.max(metrics$f1)
best     <- metrics[best_idx, ]

cat("=== Optimal cutoff (max F1 on test set) ===\n")
cat(sprintf("  Score threshold : %d\n",   best$threshold))
cat(sprintf("  Precision       : %.4f\n", best$precision))
cat(sprintf("  Recall          : %.4f\n", best$recall))
cat(sprintf("  F1 score        : %.4f\n", best$f1))

# Confusion matrix at the optimal threshold
predicted_opt <- as.integer(score <= best$threshold)
tp <- sum(predicted_opt == 1 & actual == 1)
fp <- sum(predicted_opt == 1 & actual == 0)
fn <- sum(predicted_opt == 0 & actual == 1)
tn <- sum(predicted_opt == 0 & actual == 0)
cat("\n=== Confusion matrix at optimal threshold ===\n")
cat(sprintf("  TP (bad caught)   : %d\n",  tp))
cat(sprintf("  FP (good declined): %d\n",  fp))
cat(sprintf("  FN (bad missed)   : %d\n",  fn))
cat(sprintf("  TN (good approved): %d\n",  tn))
cat(sprintf("  Decline rate      : %.1f%%\n", (tp + fp) / length(actual) * 100))

# ── Plot precision, recall, and F1 vs score threshold ─────────────────────────
# Reshape to long format for ggplot
plot_df <- rbind(
  data.frame(threshold = metrics$threshold,
             value     = metrics$precision, metric = "Precision"),
  data.frame(threshold = metrics$threshold,
             value     = metrics$recall,    metric = "Recall"),
  data.frame(threshold = metrics$threshold,
             value     = metrics$f1,        metric = "F1")
)
plot_df$metric <- factor(plot_df$metric, levels = c("Precision", "Recall", "F1"))

p <- ggplot(plot_df, aes(x = threshold, y = value, colour = metric)) +
  geom_line(linewidth = 0.9) +
  # Vertical line at the optimal threshold
  geom_vline(xintercept = best$threshold,
             linetype = "dashed", colour = "grey30", linewidth = 0.7) +
  # Dot and label for the best F1
  geom_point(data = data.frame(threshold = best$threshold,
                                value     = best$f1,
                                metric    = factor("F1", levels(plot_df$metric))),
             size = 3, colour = "#009E73") +
  annotate("text",
           x     = best$threshold + 2,
           y     = best$f1 - 0.03,
           label = sprintf("Optimal cut-off: %d\nF1 = %.4f", best$threshold, best$f1),
           hjust = 0, size = 3.4) +
  scale_colour_manual(
    values = c(Precision = "#E69F00", Recall = "#56B4E9", F1 = "#009E73")
  ) +
  scale_x_continuous(breaks = seq(450, 660, by = 25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Precision, Recall, and F1 by Score Threshold — Test Set",
    subtitle = sprintf(
      "Decision rule: predict bad if score ≤ threshold  |  %d bads, %d goods",
      sum(actual == 1), sum(actual == 0)
    ),
    x      = "Score Threshold",
    y      = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

ggsave("output/cutoff_analysis.png", p, width = 8, height = 5, dpi = 150)
cat("\nSaved: output/cutoff_analysis.png\n")

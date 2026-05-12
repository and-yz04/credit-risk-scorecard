library(ggplot2)

# 1. Load the CSV and print dimensions
# The file uses semicolons as delimiters and includes a row-index column
df <- read.csv("data/cs-training.csv", sep = ";")
# Drop the unnamed row-index column that the CSV includes as its first field
df <- df[, !names(df) %in% c("X", "")]
cat("Dimensions (rows x cols):", nrow(df), "x", ncol(df), "\n\n")

# 2. Print the first 6 rows
cat("--- First 6 rows ---\n")
print(head(df))
cat("\n")

# 3. Full summary of all columns
cat("--- Summary ---\n")
print(summary(df))
cat("\n")

# 4. Count and print missing values per column
cat("--- Missing values per column ---\n")
na_counts <- colSums(is.na(df))
print(na_counts)
cat("\n")

# 5. Plot a histogram for each numeric variable and save as a combined PNG
numeric_cols <- names(df)[sapply(df, is.numeric)]

# Build one ggplot per numeric column
plots <- lapply(numeric_cols, function(col) {
  ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 40, fill = "steelblue", colour = "white", linewidth = 0.2) +
    labs(title = col, x = NULL, y = "Count") +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(size = 8, face = "bold"))
})

# Arrange plots in a grid using base-R graphics via a helper
# ggplot2 doesn't ship a multi-plot layout function, so we use
# grid.newpage / grid.draw from the grid package (part of base R)
n     <- length(plots)
ncols <- 3L
nrows <- ceiling(n / ncols)

png("output/variable_distributions.png",
    width = ncols * 900, height = nrows * 700, res = 150)

# Convert each ggplot to a grob and draw in a grid layout
library(grid)
grid.newpage()
vp_layout <- function(row, col) {
  viewport(layout.pos.row = row, layout.pos.col = col)
}
pushViewport(viewport(layout = grid.layout(nrows, ncols)))
for (i in seq_along(plots)) {
  row <- ceiling(i / ncols)
  col <- ((i - 1L) %% ncols) + 1L
  print(plots[[i]], vp = vp_layout(row, col))
}
dev.off()
cat("Saved: output/variable_distributions.png\n\n")

# 6. Class balance of the target variable SeriousDlqin2yrs
cat("--- Class balance: SeriousDlqin2yrs ---\n")
tbl   <- table(df$SeriousDlqin2yrs)
pct   <- round(prop.table(tbl) * 100, 2)
result <- rbind(Count = tbl, Percent = pct)
print(result)

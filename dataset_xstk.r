# =============================================================
# PACKAGE INSTALLATION AND LOADING (Auto-installs if missing)
# =============================================================
packages <- c("gridExtra", "grid", "ggplot2", "reshape2", "caret", 
              "pROC", "e1071", "randomForest")

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing package: %s\n", package))
    install.packages(package, dependencies = TRUE, repos = "https://cloud.r-project.org")
    library(package, character.only = TRUE)
  }
}

cat("Loading required packages...\n")
invisible(sapply(packages, install_if_missing))
cat("All packages loaded successfully!\n\n")

# =============================================================
# UTILITY FUNCTION: CREATE FORMATTED TABLE VIEW (HEAD ... TAIL)
# =============================================================
create_view <- function(df_in, title_str) {
  # Get the first 5 rows
  head_part <- head(df_in, 5)
  # Get the last 5 rows
  tail_part <- tail(df_in, 5)
  n <- nrow(df_in)

  # Create a separator row "..."
  dots_part <- df_in[1, ] # Borrow structure from the first row
  dots_part[] <- "..." # Replace content with ...

  # Bind rows together
  display_df <- rbind(head_part, dots_part, tail_part)

  # Create simulated row indices (0, 1, 2 ... n) to match Python style
  row_indices <- c(
    "0", "1", "2", "3", "4", "...",
    as.character(n - 5), as.character(n - 4),
    as.character(n - 3), as.character(n - 2), as.character(n - 1)
  )
  rownames(display_df) <- row_indices

  # Render the table
  grid.newpage()
  title_grob <- textGrob(title_str, gp = gpar(fontsize = 14, fontface = "bold"))
  my_theme <- ttheme_default(
    core = list(bg_params = list(fill = "white"), fg_params = list(fontsize = 10)),
    colhead = list(bg_params = list(fill = "#f0f0f0"), fg_params = list(fontsize = 10, fontface = "bold"))
  )
  grid.arrange(top = title_grob, tableGrob(display_df, theme = my_theme))
}

# =============================================================
# STEP 1: READ DATA AND FEATURE SELECTION
# =============================================================
file_path <- "add.csv"
raw_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)

# Select 5 key columns (Exclude the first index column)
df <- raw_data[, c(2, 3, 4, 5, ncol(raw_data))]
colnames(df) <- c("height", "width", "ratio(width/height)", "local", "ad status")

# DISPLAY 1: Raw Data (Containing '?' symbols)
create_view(df, "1. Raw Data (With '?' symbols)")

# =============================================================
# STEP 2: DATA STANDARDIZATION (CLEANING & ENCODING)
# =============================================================
# 2.1 Replace '?' with NA
df[] <- lapply(df, trimws)
df[df == "?"] <- NA

# 2.2 Encode target labels: ad. -> 1, nonad. -> 0
df$`ad status` <- ifelse(df$`ad status` == "ad.", 1, 0)

# 2.3 Convert columns to numeric type for calculation
df$height <- as.numeric(df$height)
df$width <- as.numeric(df$width)
df$`ratio(width/height)` <- as.numeric(df$`ratio(width/height)`)

# DISPLAY 2: Data after replacing '?' with NA & Encoding
create_view(df, "2. Data after replacing '?' with NA & Encoding")

# =============================================================
# STEP 3: MEAN IMPUTATION
# =============================================================
# Calculate Mean (ignoring NA values)
mean_h <- mean(df$height, na.rm = TRUE)
mean_w <- mean(df$width, na.rm = TRUE)
mean_r <- mean(df$`ratio(width/height)`, na.rm = TRUE)

# Impute missing values (NA) with Mean
df$height[is.na(df$height)] <- mean_h
df$width[is.na(df$width)] <- mean_w
df$`ratio(width/height)`[is.na(df$`ratio(width/height)`)] <- mean_r

# Round numbers for better visualization
df$height <- round(df$height, 1)
df$width <- round(df$width, 1)
df$`ratio(width/height)` <- round(df$`ratio(width/height)`, 4)

# DISPLAY 3: Data after Mean Imputation
create_view(df, "3. Data after Mean Imputation")

# =============================================================
# STEP 4: FINAL CLEANING
# =============================================================
# 4.1 Drop remaining rows with NA (specifically in 'local' column)
df_clean <- na.omit(df)

# 4.2 Drop the first garbage row (Row containing 0, 1, 2...)
if (df_clean[1, "height"] == 0 && df_clean[1, "width"] == 1) {
  df_clean <- df_clean[-1, ]
}

# 4.3 Remove Duplicates
df_final <- unique(df_clean)

# 4.4 Reset Index
rownames(df_final) <- NULL

# DISPLAY 4: Final Complete Data
create_view(df_final, paste0("4. Final Clean Data (Unique Rows: ", nrow(df_final), ")"))

# Print final results to Console
cat("=== DATA PROCESSING COMPLETED ===\n")
cat("Initial rows:", nrow(raw_data), "\n")
cat("Final rows:", nrow(df_final), "\n")

# Assign to main_data for visualization steps
main_data <- df_final

# Re-assign column names to match standard variable names for visualization code
# (Assuming visualization code expects Height, Width, Aratio, etc.)
colnames(main_data) <- c("Height", "Width", "Aratio", "Local", "Class_Binary")

# =============================================================
# DATA VISUALIZATION
# =============================================================

# --- PREPARATION: Create Log-transformed Data ---
# (Execute this once before plotting)
main_data$log_Height <- log(main_data$Height + 1)
main_data$log_Width <- log(main_data$Width + 1)
main_data$log_Aratio <- log(main_data$Aratio + 1)

# Define Common Theme (Centered title, minimal background)
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )

# =============================================================
# 1. VISUALIZATION FOR HEIGHT - (LIGHTBLUE)
# =============================================================

# Figure 1: Height - Before Log Transformation
p1 <- ggplot(main_data, aes(x = Height)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Height (Before Log)",
    x = "Height", y = "Count"
  ) +
  my_theme
print(p1) # Display Figure 1

# Figure 2: Height - After Log Transformation
p2 <- ggplot(main_data, aes(x = log_Height)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Height (After Log)",
    x = "Log(Height + 1)", y = "Count"
  ) +
  my_theme
print(p2) # Display Figure 2


# =============================================================
# 2. VISUALIZATION FOR WIDTH - (LIGHTGREEN)
# =============================================================

# Figure 3: Width - Before Log Transformation
p3 <- ggplot(main_data, aes(x = Width)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Width (Before Log)",
    x = "Width", y = "Count"
  ) +
  my_theme
print(p3) # Display Figure 3

# Figure 4: Width - After Log Transformation
p4 <- ggplot(main_data, aes(x = log_Width)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Width (After Log)",
    x = "Log(Width + 1)", y = "Count"
  ) +
  my_theme
print(p4) # Display Figure 4


# =============================================================
# 3. VISUALIZATION FOR ARATIO (ASPECT RATIO) - (LIGHTPINK)
# =============================================================

# Figure 5: Aratio - Before Log Transformation
p5 <- ggplot(main_data, aes(x = Aratio)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Aspect Ratio (Before Log)",
    x = "Aspect Ratio", y = "Count"
  ) +
  my_theme
print(p5) # Display Figure 5

# Figure 6: Aratio - After Log Transformation
p6 <- ggplot(main_data, aes(x = log_Aratio)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of Aspect Ratio (After Log)",
    x = "Log(Aspect Ratio + 1)", y = "Count"
  ) +
  my_theme
print(p6) # Display Figure 6

# Scatter Plot: Height vs Width (Log Scale) - Green
scatter1 <- ggplot(main_data, aes(x = log_Height, y = log_Width)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(
    title = "Scatter Plot of Height and Width (Log scale)",
    x = "Log(Height)",
    y = "Log(Width)"
  ) +
  my_theme
print(scatter1)

# Scatter Plot: Height vs Aspect Ratio (Log Scale) - Red
scatter2 <- ggplot(main_data, aes(x = log_Height, y = log_Aratio)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(
    title = "Scatter Plot of Height and Aspect Ratio",
    x = "Log(Height)",
    y = "Log(Aratio)"
  ) +
  my_theme
print(scatter2)
# =============================================================
# 3b. VISUALIZATION: BOXPLOTS (DISTRIBUTION BY CLASS)
# =============================================================

# Create a temporary dataset with clear labels "NonAd" and "Ad" for plotting
plot_data <- main_data
plot_data$Class_Label <- factor(plot_data$Class_Binary, 
                                levels = c(0, 1), 
                                labels = c("NonAd", "Ad"))

# Define specific theme for Boxplots (Minimalist style)
my_boxplot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 10),
    legend.position = "none",            # Hide legend since X-axis labels are sufficient
    panel.grid.major.x = element_blank() # Remove vertical grid lines for a cleaner look
  )

# Define custom colors: NonAd (Blue), Ad (Red) to match reference style
my_colors <- c("NonAd" = "#5DA5DA", "Ad" = "#F15854")

# --- (a) Height by Class ---
bp1 <- ggplot(plot_data, aes(x = Class_Label, y = Height, fill = Class_Label)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1, alpha = 0.8) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(a) Height by Class", x = "Class", y = "Height") +
  my_boxplot_theme

# --- (b) Width by Class ---
bp2 <- ggplot(plot_data, aes(x = Class_Label, y = Width, fill = Class_Label)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1, alpha = 0.8) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(b) Width by Class", x = "Class", y = "Width") +
  my_boxplot_theme

# --- (c) Aspect Ratio by Class ---
bp3 <- ggplot(plot_data, aes(x = Class_Label, y = Aratio, fill = Class_Label)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1, alpha = 0.8) +
  scale_fill_manual(values = my_colors) +
  labs(title = "(c) Aspect Ratio by Class", x = "Class", y = "Aspect Ratio") +
  my_boxplot_theme

# =============================================================
# 4. CORRELATION MATRIX HEATMAP
# =============================================================

# IMPORTANT: Convert Local to numeric (it may be stored as character/factor)
main_data$Local <- as.numeric(as.character(main_data$Local))

# 1. Select relevant columns from main_data
# Note: Use 'Class_Binary' (1=Ad, 0=NonAd) and 'Local' (0/1)
cor_data <- main_data[, c("Height", "Width", "Aratio", "Local", "Class_Binary")]

# 2. Rename columns to match the desired output format
colnames(cor_data) <- c("height", "width", "ratio(width/height)", "local", "ad status")

# 3. Compute Correlation Matrix
cor_matrix <- cor(cor_data)

# 4. Round values to 2 decimal places for display
cor_matrix <- round(cor_matrix, 2)

# 5. Melt the correlation matrix for ggplot2
melted_cor <- melt(cor_matrix)

# 6. Plot Heatmap
heatmap_plot <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  # Draw colored tiles
  geom_tile(color = "white") +

  # Display correlation values inside tiles
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +

  # Adjust color scale: Blue (Negative) - White (0) - Red (Positive)
  scale_fill_gradient2(
    low = "#4575B4", # Deep Blue (Negative)
    mid = "#E0F3F8", # Neutral Light (Zero)
    high = "#D73027", # Deep Red (Positive)
    midpoint = 0,
    limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +

  # Set title and layout
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5), # X-axis text
    axis.text.y = element_text(size = 12), # Y-axis text
    panel.grid.major = element_blank(), # Remove grid
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = "right", # Standard legend position
    legend.direction = "vertical"
  ) +
  coord_fixed() # Ensure tiles are square

print(heatmap_plot)

# =============================================================
# 7. INFERENTIAL STATISTICS - LOGISTIC REGRESSION
# =============================================================
cat("\n\n=== INFERENTIAL STATISTICS: LOGISTIC REGRESSION ===\n\n")

# Prepare data for modeling
main_data$Class_Binary <- as.factor(main_data$Class_Binary)
modeling_data <- main_data[, c("Height", "Width", "Aratio", "Local", "Class_Binary")]

cat("Dataset:", nrow(modeling_data), "observations\n")
cat("Class distribution:", table(modeling_data$Class_Binary), 
    sprintf("(%.1f%% ads)\n", mean(modeling_data$Class_Binary == 1) * 100))

# Train/test split (70/30)
set.seed(123)
train_idx <- createDataPartition(modeling_data$Class_Binary, p = 0.7, list = FALSE)
train_data <- modeling_data[train_idx, ]
test_data <- modeling_data[-train_idx, ]

cat(sprintf("Training: %d | Testing: %d observations\n", nrow(train_data), nrow(test_data)))

# -------------------------------------------------------------
# 1. Basic Logistic Regression
# -------------------------------------------------------------
cat("\n--- Basic Logistic Regression ---\n")
logistic_model <- glm(Class_Binary ~ ., data = train_data, family = binomial)

# Predictions and evaluation
prob_pred <- predict(logistic_model, test_data, type = "response")
class_pred <- as.factor(ifelse(prob_pred >= 0.5, 1, 0))
conf_matrix <- confusionMatrix(class_pred, test_data$Class_Binary, positive = "1")

# Display results
cat(sprintf("Accuracy: %.1f%% | Precision: %.1f%% | Recall: %.1f%% | F1: %.2f\n",
            conf_matrix$overall['Accuracy'] * 100,
            conf_matrix$byClass['Precision'] * 100,
            conf_matrix$byClass['Sensitivity'] * 100,
            conf_matrix$byClass['F1']))

# ROC curve
roc_obj <- roc(test_data$Class_Binary, prob_pred)
cat(sprintf("AUC: %.3f\n", auc(roc_obj)))

# Confusion matrix visualization
conf_df <- as.data.frame(conf_matrix$table)
colnames(conf_df) <- c("Predicted", "Actual", "Count")

ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = Count), size = 10, fontface = "bold") +
  scale_fill_gradient(low = "#E8F4F8", high = "#1E88E5") +
  labs(title = "Confusion Matrix - Basic Logistic Regression",
       x = "Predicted (0=Non-ad, 1=Ad)", y = "Actual (0=Non-ad, 1=Ad)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# ROC curve
ggroc(roc_obj, color = "steelblue", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = sprintf("ROC Curve (AUC = %.3f)", auc(roc_obj)),
       x = "Specificity", y = "Sensitivity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Coefficients
coef_df <- data.frame(Feature = names(coef(logistic_model)),
                      Coefficient = coef(logistic_model),
                      Odds_Ratio = exp(coef(logistic_model)),
                      row.names = NULL)
cat("\nModel Coefficients:\n")
print(coef_df)

# -------------------------------------------------------------
# 2. Model Improvements - Addressing Class Imbalance
# -------------------------------------------------------------
cat("\n\n--- Model Improvements: Addressing Class Imbalance ---\n")

# Calculate class weights (inverse frequency)
class_counts <- table(train_data$Class_Binary)
weights <- sapply(class_counts, function(x) 1/x)
weights <- weights / sum(weights) * 2  # Normalize
cat(sprintf("Class weights: non-ad=%.3f, ad=%.3f (ratio=%.1fx)\n", 
            weights[1], weights[2], weights[2]/weights[1]))

# Weighted Logistic Regression
sample_wts <- ifelse(train_data$Class_Binary == 1, weights[2], weights[1])
weighted_model <- glm(Class_Binary ~ ., data = train_data, 
                      family = binomial, weights = sample_wts)

# Find optimal threshold using Youden's Index
weighted_probs <- predict(weighted_model, test_data, type = "response")
weighted_roc <- roc(test_data$Class_Binary, weighted_probs)
opt_threshold <- coords(weighted_roc, "best", best.method = "youden")$threshold

cat(sprintf("Optimal threshold: %.3f (vs default 0.5)\n", opt_threshold))

weighted_preds <- as.factor(ifelse(weighted_probs >= opt_threshold, 1, 0))
conf_weighted <- confusionMatrix(weighted_preds, test_data$Class_Binary, positive = "1")

# Random Forest with class weights
set.seed(123)
rf_model <- randomForest(Class_Binary ~ ., data = train_data, ntree = 500,
                         importance = TRUE, classwt = c(weights[1], weights[2]))

rf_preds <- predict(rf_model, test_data, type = "class")
rf_probs <- predict(rf_model, test_data, type = "prob")[, "1"]
conf_rf <- confusionMatrix(rf_preds, test_data$Class_Binary, positive = "1")
roc_rf <- roc(test_data$Class_Binary, rf_probs)

# -------------------------------------------------------------
# 3. Model Comparison
# -------------------------------------------------------------
cat("\n=== MODEL COMPARISON ===\n")

comparison <- data.frame(
  Model = c("Basic Logistic", "Weighted Logistic", "Random Forest"),
  Accuracy = c(conf_matrix$overall['Accuracy'], 
               conf_weighted$overall['Accuracy'],
               conf_rf$overall['Accuracy']) * 100,
  Precision = c(conf_matrix$byClass['Precision'],
                conf_weighted$byClass['Precision'],
                conf_rf$byClass['Precision']) * 100,
  Recall = c(conf_matrix$byClass['Sensitivity'],
             conf_weighted$byClass['Sensitivity'],
             conf_rf$byClass['Sensitivity']) * 100,
  F1 = c(conf_matrix$byClass['F1'],
         conf_weighted$byClass['F1'],
         conf_rf$byClass['F1']),
  AUC = c(auc(roc_obj), auc(weighted_roc), auc(roc_rf))
)

comparison_display <- comparison
comparison_display[, 2:6] <- round(comparison[, 2:6], 2)
print(comparison_display)

# Performance comparison chart
comp_long <- data.frame(
  Model = rep(comparison$Model, 4),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1-Score"), each = 3),
  Value = c(comparison$Accuracy, comparison$Precision, 
            comparison$Recall, comparison$F1 * 100)
)

ggplot(comp_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = c("#E57373", "#FFB74D", "#81C784")) +
  labs(title = "Model Performance Comparison", y = "Score (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# ROC comparison
roc_df <- data.frame(
  FPR = c(1 - rev(roc_obj$specificities), 
          1 - rev(weighted_roc$specificities),
          1 - rev(roc_rf$specificities)),
  TPR = c(rev(roc_obj$sensitivities),
          rev(weighted_roc$sensitivities),
          rev(roc_rf$sensitivities)),
  Model = rep(c("Basic", "Weighted", "RF"),
              c(length(roc_obj$sensitivities),
                length(weighted_roc$sensitivities),
                length(roc_rf$sensitivities)))
)

ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#E57373", "#FFB74D", "#81C784")) +
  labs(title = sprintf("ROC Curves (AUC: Basic=%.2f, Weighted=%.2f, RF=%.2f)",
                       auc(roc_obj), auc(weighted_roc), auc(roc_rf)),
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

# Feature importance
imp <- data.frame(Feature = rownames(importance(rf_model)),
                  Importance = importance(rf_model)[, "MeanDecreaseGini"])
imp <- imp[order(-imp$Importance), ]

ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#81C784") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "", y = "Importance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# -------------------------------------------------------------
# Summary
# -------------------------------------------------------------
best_idx <- which.max(comparison$F1)
cat(sprintf("\n=== SUMMARY ===\n"))
cat(sprintf("Best Model: %s (F1=%.2f, AUC=%.2f)\n", 
            comparison$Model[best_idx], comparison$F1[best_idx], comparison$AUC[best_idx]))
cat(sprintf("Recall Improvement: %.1f%% → %.1f%% (%.1fx)\n",
            comparison$Recall[1], comparison$Recall[best_idx],
            comparison$Recall[best_idx] / comparison$Recall[1]))
cat("\nTop 3 Features:\n")
for(i in 1:3) cat(sprintf("  %d. %s\n", i, imp$Feature[i]))
cat("\Analysis complete!\n")
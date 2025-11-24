library(gridExtra)
library(grid)
library(ggplot2)
library(reshape2) # Required for melting correlation matrix
# ==============================================================================
# UTILITY FUNCTION: CREATE FORMATTED TABLE VIEW (HEAD ... TAIL)
# ==============================================================================
create_view <- function(df_in, title_str) {
  # Get the first 5 rows
  head_part <- head(df_in, 5)
  # Get the last 5 rows
  tail_part <- tail(df_in, 5)
  n <- nrow(df_in)
  
  # Create a separator row "..."
  dots_part <- df_in[1, ] # Borrow structure from the first row
  dots_part[] <- "..."    # Replace content with ...
  
  # Bind rows together
  display_df <- rbind(head_part, dots_part, tail_part)
  
  # Create simulated row indices (0, 1, 2 ... n) to match Python style
  row_indices <- c("0", "1", "2", "3", "4", "...", 
                   as.character(n-5), as.character(n-4), 
                   as.character(n-3), as.character(n-2), as.character(n-1))
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

# ==============================================================================
# STEP 1: READ DATA AND FEATURE SELECTION
# ==============================================================================
file_path <- "C:/Users/admin/Desktop/BTL/BTL XSTK 251/add.csv"
raw_data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)

# Select 5 key columns (Exclude the first index column)
df <- raw_data[, c(2, 3, 4, 5, ncol(raw_data))]
colnames(df) <- c("height", "width", "ratio(width/height)", "local", "ad status")

# DISPLAY 1: Raw Data (Containing '?' symbols)
create_view(df, "1. Raw Data (With '?' symbols)")

# ==============================================================================
# STEP 2: DATA STANDARDIZATION (CLEANING & ENCODING)
# ==============================================================================
# 2.1 Replace '?' with NA
df[] <- lapply(df, trimws)
df[df == "?"] <- NA

# 2.2 Encode target labels: ad. -> 1, nonad. -> 0
df$`ad status` <- ifelse(df$`ad status` == "ad.", 1, 0)

# 2.3 Convert columns to numeric type for calculation
df$height <- as.numeric(df$height)
df$width  <- as.numeric(df$width)
df$`ratio(width/height)` <- as.numeric(df$`ratio(width/height)`)

# DISPLAY 2: Data after replacing '?' with NA & Encoding
create_view(df, "2. Data after replacing '?' with NA & Encoding")

# ==============================================================================
# STEP 3: MEAN IMPUTATION
# ==============================================================================
# Calculate Mean (ignoring NA values)
mean_h <- mean(df$height, na.rm = TRUE)
mean_w <- mean(df$width, na.rm = TRUE)
mean_r <- mean(df$`ratio(width/height)`, na.rm = TRUE)

# Impute missing values (NA) with Mean
df$height[is.na(df$height)] <- mean_h
df$width[is.na(df$width)]   <- mean_w
df$`ratio(width/height)`[is.na(df$`ratio(width/height)`)] <- mean_r

# Round numbers for better visualization
df$height <- round(df$height, 1)
df$width  <- round(df$width, 1)
df$`ratio(width/height)` <- round(df$`ratio(width/height)`, 4)

# DISPLAY 3: Data after Mean Imputation
create_view(df, "3. Data after Mean Imputation")

# ==============================================================================
# STEP 4: FINAL CLEANING
# ==============================================================================
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

# ==============================================================================
# DATA VISUALIZATION
# ==============================================================================

# --- PREPARATION: Create Log-transformed Data ---
# (Execute this once before plotting)
main_data$log_Height <- log(main_data$Height + 1)
main_data$log_Width  <- log(main_data$Width + 1)
main_data$log_Aratio <- log(main_data$Aratio + 1)

# Define Common Theme (Centered title, minimal background)
my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12))

# ==============================================================================
# 1. VISUALIZATION FOR HEIGHT - (LIGHTBLUE)
# ==============================================================================

# Figure 1: Height - Before Log Transformation
p1 <- ggplot(main_data, aes(x = Height)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Height (Before Log)",
       x = "Height", y = "Count") +
  my_theme
print(p1) # Display Figure 1

# Figure 2: Height - After Log Transformation
p2 <- ggplot(main_data, aes(x = log_Height)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Height (After Log)",
       x = "Log(Height + 1)", y = "Count") +
  my_theme
print(p2) # Display Figure 2


# ==============================================================================
# 2. VISUALIZATION FOR WIDTH - (LIGHTGREEN)
# ==============================================================================

# Figure 3: Width - Before Log Transformation
p3 <- ggplot(main_data, aes(x = Width)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Width (Before Log)",
       x = "Width", y = "Count") +
  my_theme
print(p3) # Display Figure 3

# Figure 4: Width - After Log Transformation
p4 <- ggplot(main_data, aes(x = log_Width)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Width (After Log)",
       x = "Log(Width + 1)", y = "Count") +
  my_theme
print(p4) # Display Figure 4


# ==============================================================================
# 3. VISUALIZATION FOR ARATIO (ASPECT RATIO) - (LIGHTPINK)
# ==============================================================================

# Figure 5: Aratio - Before Log Transformation
p5 <- ggplot(main_data, aes(x = Aratio)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Aspect Ratio (Before Log)",
       x = "Aspect Ratio", y = "Count") +
  my_theme
print(p5) # Display Figure 5

# Figure 6: Aratio - After Log Transformation
p6 <- ggplot(main_data, aes(x = log_Aratio)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Aspect Ratio (After Log)",
       x = "Log(Aspect Ratio + 1)", y = "Count") +
  my_theme
print(p6) # Display Figure 6

# Scatter Plot: Height vs Width (Log Scale) - Green
scatter1 <- ggplot(main_data, aes(x = log_Height, y = log_Width)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(title = "Scatter Plot of Height and Width (Log scale)",
       x = "Log(Height)",
       y = "Log(Width)") +
  my_theme
print(scatter1)

# Scatter Plot: Height vs Aspect Ratio (Log Scale) - Red
scatter2 <- ggplot(main_data, aes(x = log_Height, y = log_Aratio)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(title = "Scatter Plot of Height and Aspect Ratio",
       x = "Log(Height)",
       y = "Log(Aratio)") +
  my_theme
print(scatter2)

# ==============================================================================
# 4. CORRELATION MATRIX HEATMAP
# ==============================================================================

# 1. Chọn các cột cần thiết từ main_data
# Lưu ý: Hình mẫu có 'ad status', ta dùng cột 'Class_Binary' (1=Ad, 0=NonAd) đã tạo ở dataset.r
cor_data <- main_data[, c("Height", "Width", "Aratio", "Local", "Class_Binary")]

# === [BỔ SUNG ĐOẠN NÀY ĐỂ SỬA LỖI] ===
# Ép tất cả các cột về dạng số (đặc biệt là cột Local)
cor_data$Height       <- as.numeric(cor_data$Height)
cor_data$Width        <- as.numeric(cor_data$Width)
cor_data$Aratio       <- as.numeric(cor_data$Aratio)
cor_data$Local        <- as.numeric(cor_data$Local)       # <--- Nguyên nhân chính gây lỗi
cor_data$Class_Binary <- as.numeric(cor_data$Class_Binary)
# 2. Đổi tên cột cho GIỐNG HỆT hình mẫu bạn gửi
colnames(cor_data) <- c("height", "width", "ratio(width/height)", "local", "ad status")

# 3. Tính ma trận tương quan
cor_matrix <- cor(cor_data)

# 4. Làm tròn số liệu lấy 2 chữ số thập phân (để hiển thị đẹp như hình)
cor_matrix <- round(cor_matrix, 2)

# 5. Chuyển đổi dữ liệu sang dạng dài (Long format) để vẽ bằng ggplot2
melted_cor <- melt(cor_matrix)

# ==============================================================================
# BƯỚC 2: VẼ HEATMAP
# ==============================================================================

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  # Vẽ các ô vuông màu
  geom_tile(color = "white") + 
  
  # Hiển thị con số tương quan ở giữa ô
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +
  
  # Chỉnh màu sắc: Xanh (Âm) - Trắng (0) - Đỏ (Dương) giống hình mẫu
  scale_fill_gradient2(low = "#4575B4",   # Màu xanh đậm (Negative)
                       mid = "#E0F3F8",   # Màu nhạt trung tính (Zero)
                       high = "#D73027",  # Màu đỏ đậm (Positive)
                       midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  
  # Thiết lập tiêu đề và giao diện
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Tiêu đề căn giữa
    axis.text.x = element_text(angle = 0, vjust = 1, size = 12, hjust = 0.5), # Chữ trục X
    axis.text.y = element_text(size = 12), # Chữ trục Y
    panel.grid.major = element_blank(), # Bỏ lưới nền
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.25, 0.2), # Đẩy thanh legend sang phải giống hình
    legend.direction = "vertical"
  ) +
  coord_fixed() # Đảm bảo các ô là hình vuông



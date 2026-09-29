# SETUP
library(tidyverse)   # dplyr, ggplot2, tidyr, readr, purrr
library(janitor)      # clean_names(), tabyl()
library(lubridate)    # date parsing
library(patchwork)    # combining ggplot panels
library(scales)       # dollar_format(), comma()
library(corrplot)     # correlation heatmap

# Reproducibility
set.seed(42)

# "Corporate Navy / Gray" chart theme
navy       <- "#1E2761"
navy_soft  <- "#3A4A9E"
gray_mid   <- "#8A93A6"
gray_light <- "#E7EAF2"
accent     <- "#E8871E"
ice       <- "#E7F0F9"

theme_exec <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", color = navy, size = base_size + 3, hjust = 0),
      plot.subtitle    = element_text(color = gray_mid, size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = gray_light, linewidth = 0.4),
      axis.title       = element_text(color = "#22262F"),
      axis.text        = element_text(color = "#22262F"),
      legend.position   = "bottom"
    )
}

# 1. LOAD

raw <- read_csv("used_cars_me.csv", show_col_types = FALSE) %>%
  clean_names()  # enforce snake_case, strip whitespace/punctuation from headers

glimpse(raw)
n_raw <- nrow(raw)

# 2. DATA CLEANING
# Documented, reproducible cleaning pipeline. Each filter reports how many rows
# it removes so the cleaning log below can be dropped straight into the
# "Data Cleaning & QA" slide.

cleaning_log <- tibble(step = character(), rows_removed = integer())

log_step <- function(log, step_name, before, after) {
  bind_rows(log, tibble(step = step_name, rows_removed = before - after))
}

# 2.1 Drop columns that carry no analytical signal for this EDA
#     (raw URLs/images are not modeled; VIN is 91% missing; county is 100% missing;
#      free-text description is retained separately for text-mining, out of scope here)
df <- raw %>%
  select(-url, -region_url, -image_url, -county, -vin, -description)

# 2.2 Enforce data types
df <- df %>%
  mutate(
    price         = as.numeric(price),
    year          = as.integer(year),
    odometer      = as.numeric(odometer),
    posting_date  = ymd_hms(posting_date, tz = "UTC"),
    manufacturer  = factor(manufacturer),
    condition     = factor(condition, levels = c("salvage", "fair", "good", "excellent", "like new", "new")),
    cylinders     = factor(cylinders),
    fuel          = factor(fuel),
    title_status  = factor(title_status),
    transmission  = factor(transmission),
    drive         = factor(drive),
    size          = factor(size),
    type          = factor(type),
    paint_color   = factor(paint_color)
  )

# 2.3 Remove duplicate listings (exact duplicate rows, then duplicate listing IDs)
before <- nrow(df)
df <- df %>% distinct() %>% distinct(id, .keep_all = TRUE)
cleaning_log <- log_step(cleaning_log, "Duplicate rows / duplicate IDs", before, nrow(df))

# 2.4 Remove invalid, non-positive prices (price <= 0 = "contact for price"
#     placeholder listings with no real asking price captured)
before <- nrow(df)
df <- df %>% filter(price > 0)
cleaning_log <- log_step(cleaning_log, "Non-positive price (placeholder listings)", before, nrow(df))

# 2.5 Remove the systematic $99 "teaser price" artifact.
#     EDA revealed 293 listings across 20+ brands all priced at exactly $99 —
#     a dealer template default, not a real transaction price. Left in, it
#     would fabricate a fake low-price cluster and distort every price statistic.
before <- nrow(df)
df <- df %>% filter(price != 99)
cleaning_log <- log_step(cleaning_log, "$99 placeholder-price artifact", before, nrow(df))

# 2.6 Remove rows with a missing model (cannot be categorized or matched to a brand tier)
before <- nrow(df)
df <- df %>% filter(!is.na(model))
cleaning_log <- log_step(cleaning_log, "Missing model", before, nrow(df))

# 2.7 Remove invalid odometer readings (0 miles paired with non-new "condition",
#     or > 400,000 miles, both far outside plausible passenger-vehicle range)
before <- nrow(df)
df <- df %>% filter(odometer > 0, odometer <= 400000)
cleaning_log <- log_step(cleaning_log, "Invalid odometer (0 or > 400k mi)", before, nrow(df))

# 2.8 Remove implausible model years (> the analysis year, i.e. cannot yet exist
#     as a "used" vehicle in this listing window)
before <- nrow(df)
df <- df %>% filter(year <= 2021)
cleaning_log <- log_step(cleaning_log, "Model year beyond listing window", before, nrow(df))

# 2.9 Missing-value strategy for remaining categorical fields:
#     recode as an explicit "unknown" level rather than deleting the row —
#     this preserves sample size for price/mileage analysis while keeping
#     missingness visible and excludable from brand/segment-specific cuts.
cat_cols <- c("manufacturer", "condition", "cylinders", "transmission",
              "drive", "size", "type", "paint_color")

df <- df %>%
  mutate(across(all_of(cat_cols), ~ fct_na_value_to_level(., "unknown")))

# 2.10 Derived fields used throughout the EDA
df <- df %>%
  mutate(
    age = 2021L - year,                      # vehicle age at listing time
    price_per_1k_miles = price / (odometer / 1000)
  )

n_clean <- nrow(df)

cleaning_log <- cleaning_log %>%
  add_row(step = "TOTAL removed", rows_removed = n_raw - n_clean) %>%
  add_row(step = "Final analysis-ready rows", rows_removed = n_clean)

print(cleaning_log)
# Raw rows: 2,800 -> Final analysis-ready rows: 2,290 (~81.8% retained)

write_csv(df, "used_cars_me_clean.csv")

# 3. DESCRIPTIVE STATISTICS

library(e1071)  # skewness()

numeric_summary <- function(x) {
  tibble(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    q1     = quantile(x, .25, na.rm = TRUE),
    q3     = quantile(x, .75, na.rm = TRUE),
    iqr    = IQR(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE),
    skew   = skewness(x, na.rm = TRUE)
  )
}

price_stats    <- numeric_summary(df$price)
odometer_stats <- numeric_summary(df$odometer)
age_stats      <- numeric_summary(df$age)

# Key results (see presentation for full figures):
#   Price    : median $12,988 | mean $16,802 | skew  2.86 (strong right skew)
#   Odometer : median 107,206 mi | mean 108,567 mi | skew 0.29 (mild right skew)
#   Age      : median 9 yrs | mean 9 yrs | skew 0.49

# Median price by brand (brands with >= 20 listings, for a stable estimate)
brand_price <- df %>%
  filter(manufacturer != "unknown") %>%
  count(manufacturer, name = "n") %>%
  filter(n >= 20) %>%
  inner_join(
    df %>% group_by(manufacturer) %>%
      summarize(median_price = median(price), mean_price = mean(price), .groups = "drop"),
    by = "manufacturer"
  ) %>%
  arrange(desc(median_price))

# IQR-based outlier bounds for price (used for flagging, not blanket deletion —
# a $95k truck is a legitimate high-end listing, not a data error)
q1 <- quantile(df$price, .25); q3 <- quantile(df$price, .75); iqr <- q3 - q1
outlier_hi <- q3 + 1.5 * iqr
df <- df %>% mutate(price_outlier = price > outlier_hi)
n_outliers <- sum(df$price_outlier)  # 76 listings flagged as statistical high-price outliers

# 4. VISUALIZATIONS

## 4.1 Univariate — price distribution
p_price_hist <- ggplot(df, aes(price)) +
  geom_histogram(bins = 40, fill = navy_soft, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = mean(df$price), color = accent, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(df$price), color = navy, linewidth = 1) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  labs(title = "Asking-Price Distribution — Right-Skewed Market",
       x = "Listing Price", y = "Number of Listings") +
  theme_exec()

## 4.2 Univariate — odometer distribution
p_odo_hist <- ggplot(df, aes(odometer)) +
  geom_histogram(bins = 40, fill = navy_soft, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = mean(df$odometer), color = accent, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(df$odometer), color = navy, linewidth = 1) +
  scale_x_continuous(labels = comma_format()) +
  labs(title = "Mileage Distribution — Mild Right Skew",
       x = "Odometer (miles)", y = "Number of Listings") +
  theme_exec()

## 4.3 Bivariate — price vs. odometer, with trend line
p_price_odo <- ggplot(df, aes(odometer, price)) +
  geom_point(alpha = 0.35, color = navy_soft, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, color = accent, linewidth = 1.3) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  scale_x_continuous(labels = comma_format()) +
  labs(title = "Price Declines as Mileage Rises",
       x = "Odometer (miles)", y = "Listing Price") +
  theme_exec()

## 4.4 Bivariate — price vs. age (depreciation), with trend line
p_price_age <- ggplot(df, aes(age, price)) +
  geom_point(alpha = 0.35, color = navy_soft, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, color = accent, linewidth = 1.3) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  labs(title = "Depreciation Trend — Price Declines Steadily with Age",
       x = "Vehicle Age (years)", y = "Listing Price") +
  theme_exec()

## 4.5 Categorical — median price by brand (top 12 by volume)
p_brand <- brand_price %>%
  slice_max(n, n = 12) %>%
  mutate(manufacturer = fct_reorder(manufacturer, median_price)) %>%
  ggplot(aes(median_price, manufacturer)) +
  geom_col(fill = navy_soft) +
  geom_text(aes(label = dollar(median_price, accuracy = 1)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"),
                      expand = expansion(mult = c(0, .18))) +
  labs(title = "Median Price by Brand (Top 12 by Volume)",
       x = "Median Listing Price", y = NULL) +
  theme_exec()

## 4.6 Categorical — price spread by fuel type (box plot)
p_fuel_box <- ggplot(df, aes(fuel, price)) +
  geom_boxplot(fill = navy_soft, color = navy, outlier.alpha = 0.25) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  labs(title = "Price Spread by Fuel Type", x = NULL, y = "Listing Price") +
  theme_exec()

## 4.7 Multivariate — correlation heatmap (price, odometer, age, year)
corr_mat <- df %>% select(price, odometer, age, year) %>% cor(use = "complete.obs")
corrplot(corr_mat, method = "color", type = "upper", addCoef.col = "black",
         col = colorRampPalette(c(navy, "white", accent))(200),
         tl.col = navy, tl.srt = 30, title = "Feature Correlation Matrix", mar = c(0,0,2,0))

## 4.8 Outlier spotlight — statistical high-price outliers on the cleaned data
p_outlier_box <- ggplot(df, aes(x = price, y = "")) +
  geom_boxplot(fill = navy_soft, color = navy, outlier.color = accent, outlier.alpha = 0.5) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  labs(title = paste0("Cleaned Price Distribution — ", n_outliers, " IQR Outliers Flagged"),
       x = "Listing Price", y = NULL) +
  theme_exec()

# Combine univariate panels for a compact "Slide 4" export
(p_price_hist | p_odo_hist)
ggsave("chart_univariate.png", width = 11, height = 5, dpi = 200)

(p_price_odo | p_price_age)
ggsave("chart_bivariate.png", width = 11, height = 5, dpi = 200)

(p_brand | p_fuel_box)
ggsave("chart_categorical.png", width = 11, height = 5.5, dpi = 200)

ggsave("chart_outliers.png", plot = p_outlier_box, width = 9, height = 3, dpi = 200)

## 4.9 Outlier spotlight — the $99 placeholder-price artifact
##     (uses the RAW data, before the $99 filter is applied, to show the artifact)
raw_for_plot <- raw %>% filter(price > 0, price < 3000)
teaser_n <- sum(raw$price == 99, na.rm = TRUE)

p_price_anomaly <- ggplot(raw_for_plot, aes(price)) +
  geom_histogram(bins = 60, fill = gray_mid, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 99, color = accent, linewidth = 1.5) +
  annotate("text", x = 99, y = Inf,
           label = paste0("$99 placeholder price\n(", teaser_n, " listings)"),
           color = accent, fontface = "bold", hjust = -0.05, vjust = 1.3, size = 4) +
  labs(title = "Data Anomaly: Systematic $99 Placeholder Pricing",
       x = "Listing Price (zoomed to $0\u20133,000)", y = "Number of Listings") +
  theme_exec()

ggsave("chart_price_anomaly.png", plot = p_price_anomaly, width = 9, height = 5, dpi = 200)

## 4.10 Transmission mix (donut) + reported condition (bar), side by side
trans_counts <- df %>%
  count(transmission) %>%
  mutate(pct = n / sum(n),
         label = paste0(str_to_title(transmission), " (", scales::percent(pct, accuracy = 1), ")"))

p_transmission <- ggplot(trans_counts, aes(x = 2, y = n, fill = transmission)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c(automatic = navy, manual = navy_soft,
                               other = ice, unknown = gray_mid),
                    labels = trans_counts$label) +
  labs(title = "Transmission Mix", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", color = navy, size = 15, hjust = 0.5),
        legend.position = "right")

p_condition <- df %>%
  filter(condition != "unknown") %>%
  count(condition) %>%
  ggplot(aes(fct_reorder(condition, -n), n)) +
  geom_col(fill = navy_soft) +
  labs(title = "Reported Condition (excl. unspecified)", x = NULL, y = "Number of Listings") +
  theme_exec() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

(p_transmission | p_condition)
ggsave("chart_transmission_condition.png", width = 11, height = 5, dpi = 200)


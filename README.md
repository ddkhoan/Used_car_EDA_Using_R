# Used Car Market Intelligence: Maine Craigslist EDA

An exploratory data analysis of 2,290 used vehicle listings to uncover depreciation signals, market pricing clusters, and regional inventory trends. 

## Business Problem
Understanding how fast vehicle value erodes with age and mileage informs trade-in timing and pricing strategy. Furthermore, scraped marketplace data often contains placeholder prices that can silently distort downstream predictive models if not identified and neutralized.

## Dataset
* **Source:** Sampled from a larger Craigslist used-vehicle dataset (Kaggle), focusing on a Maine regional sample.
* **Size:** 2,800 raw listings, covering 38 manufacturers and model years 2000–2021.
* **Cleaning Summary:** Enforced snake_case naming, converted data types, handled missing categorical variables with an explicit "unknown" level, and removed non-positive prices and invalid odometers, resulting in 2,290 analysis-ready rows (81.8% retained).

## Tools & Skills Demonstrated
* **Language:** R
* **Libraries:** `tidyverse` (dplyr, ggplot2, tidyr, readr, purrr), `janitor`, `lubridate`, `patchwork`, `scales`, `corrplot`, `e1071`
* **Techniques:** Data Cleaning, Categorical Imputation, Statistical Summaries, Outlier Detection (IQR method), Correlation Heatmaps, Data Storytelling.

## Key Insights
* **Right-Skewed Pricing:** The market is strongly right-skewed with a median asking price of $12,988, heavily pulled by a long tail of premium trucks/SUVs (mean $16,802).
* **Depreciation Signals:** Mileage and vehicle age show a moderate negative correlation with price (r ≈ -0.41 to -0.43). A logarithmic fit against age better captures a constant percentage of value loss.
* **Anomaly Detection:** Identified and removed a systematic $99 "placeholder price" artifact across 293 listings that would have fabricated a false low-price cluster.
* **Brand Premiums:** Truck-oriented brands dictate the highest median prices in this regional market, led by Ram ($33,444), GMC ($24,345), and Ford ($18,995).

## Visualizations
*(View the `/charts` folder for full-resolution exports)*
1. `chart_univariate.png`: Skewed distributions of price and mileage.
2. `chart_price_anomaly.png`: Spotlight on the $99 placeholder pricing artifact.
3. `chart_categorical.png`: Median price spread by brand and fuel type.

## How to Reproduce
1. Clone this repository: `git clone https://github.com/Username/used-car-market-eda-r.git`
2. Open `used_cars_eda_Doan-Duy-Khoa-Nguyen.R` in RStudio.
3. Ensure the `used_cars_me.csv` file is in the working directory and execute the script.

## What I Learned / Next Steps
Detecting the $99 placeholder artifact reinforced that real-world data requires aggressive domain-specific QA before any modeling begins. Next steps involve building a multivariate pricing model to isolate the independent effects of brand, age, and mileage.

## Contact
* LinkedIn: [Your LinkedIn URL]
* Email: [Your Email]

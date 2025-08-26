# Load essential libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stats)
library(lubridate)

# === Step 1: Load Taiwan Data ===
file_path <- "C:/Users/User/OneDrive/Documents/Chow_research/UN Comtrade/TaiwanProx_X_USA_2018_2024/20250508＿Monthly data of Taiwanexport to the US by commodities.xlsx"
df <- read_excel(file_path, sheet = 1)

# Filter only Taiwan to US trade
df <- df %>% filter(對象國 == "US")

# Drop trailing metadata columns
df <- df[, 1:which(colnames(df) == "2025年3月出口貿易金額")]

# Clean month column names (e.g., "2016年1月出口貿易金額" -> "2016-01")
colnames(df)[5:ncol(df)] <- colnames(df)[5:ncol(df)] %>%
  gsub("出口貿易金額", "", .) %>%
  gsub("年", "-", .) %>%
  gsub("月", "", .) %>%
  sprintf("%s-01", .) %>%
  as.Date("%Y-%m-%d") %>%
  format("%Y-%m")

# Step 2: Monthly export total
monthly_data <- df[, 5:ncol(df)]
monthly_data <- as.data.frame(sapply(monthly_data, as.numeric))
monthly_totals <- colSums(monthly_data, na.rm = TRUE)

monthly_export_df <- data.frame(
  Month = names(monthly_totals),
  Total_Export_Value = monthly_totals * 1e6
)

# Step 3: Prepare for TSI Calculation
monthly_data_with_hs6 <- df[, c("hs6", colnames(df)[5:ncol(df)])]

# Step 4: TSI Calculation Loop
years <- 2018:2024
months <- 1:12
TSI_results <- list()

for (year in years) {
  for (month in months) {
    month_string <- sprintf("%04d-%02d", year, month)
    if (!(month_string %in% colnames(monthly_data_with_hs6))) next
    
    # China path and data
    file_path_china <- sprintf("C:/Users/user/OneDrive/Documents/Chow_research/UN Comtrade/China_X_USA_2018_2024/%d_CN.xlsx", year)
    China_exports <- read_excel(file_path_china)
    China_month_exports <- China_exports %>% filter(refMonth == month)
    total_exports_China_month <- sum(China_month_exports$primaryValue, na.rm = TRUE)
    
    # Taiwan data for this month
    Taiwan_month_exports <- monthly_data_with_hs6 %>%
      select(cmdCode = hs6, primaryValue = all_of(month_string)) %>%
      filter(!is.na(primaryValue))
    total_exports_Taiwan_month <- sum(Taiwan_month_exports$primaryValue, na.rm = TRUE)
    
    # Match common HS6 codes
    common_hs_codes <- intersect(China_month_exports$cmdCode, Taiwan_month_exports$cmdCode)
    China_common_exports <- China_month_exports %>% filter(cmdCode %in% common_hs_codes)
    Taiwan_common_exports <- Taiwan_month_exports %>% filter(cmdCode %in% common_hs_codes)
    
    # Calculate market shares
    China_market_share <- China_common_exports %>%
      mutate(MarketShare = primaryValue / total_exports_China_month * 100)
    Taiwan_market_share <- Taiwan_common_exports %>%
      mutate(MarketShare = primaryValue / total_exports_Taiwan_month * 100)
    
    # Calculate TSI per HS code
    combined_market_shares <- merge(China_market_share, Taiwan_market_share, by = "cmdCode", suffixes = c("_China", "_Taiwan"))
    combined_market_shares$TSI_per_HS <- pmin(combined_market_shares$MarketShare_China, combined_market_shares$MarketShare_Taiwan)
    TSI_results[[paste(year, month, sep = "-")]] <- sum(combined_market_shares$TSI_per_HS)
  }
}

# Step 5: Assemble TSI Data
TSI_data_frame <- do.call(rbind, lapply(names(TSI_results), function(x) {
  data.frame(Date = x, TSI = TSI_results[[x]])
}))

# Step 6: Format trade data for correlation
Taiwan_trade_values <- monthly_export_df
Taiwan_trade_values <- Taiwan_trade_values[-c(1:3), ]
Taiwan_trade_values$Month <- as.Date(paste0(Taiwan_trade_values$Month, "-01"))
Taiwan_trade_values <- Taiwan_trade_values %>% arrange(Month)

# Step 7: Year-over-Year Changes
Taiwan_YTY_change <- Taiwan_trade_values %>%
  mutate(YoY_Change = Total_Export_Value - lag(Total_Export_Value, 12)) %>%
  filter(!is.na(YoY_Change)) %>%
  transmute(Month = format(Month, "%Y-%m"), YoY_Change)

Taiwan_YearlyGrowth_values <- Taiwan_trade_values %>%
  mutate(Yearly_Growth = (Total_Export_Value - lag(Total_Export_Value, 12)) / Total_Export_Value) %>%
  filter(!is.na(Yearly_Growth)) %>%
  transmute(Month = format(Month, "%Y-%m"), Yearly_Growth)

# Step 8: Combine All Data
dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
date_df <- data.frame(Month = format(dates, "%Y-%m"))
Taiwan_combined_df <- date_df %>%
  left_join(Taiwan_trade_values %>% mutate(Month = format(Month, "%Y-%m")), by = "Month") %>%
  left_join(Taiwan_YTY_change, by = "Month") %>%
  left_join(Taiwan_YearlyGrowth_values, by = "Month") %>%
  rename(Date = Month)

TSI_data_frame$Date <- format(as.Date(paste0(TSI_data_frame$Date, "-01")), "%Y-%m")
Taiwan_TSI_combined <- merge(Taiwan_combined_df, TSI_data_frame, by = "Date", all = TRUE)

# Step 9: Correlation Tests
correlation_test <- cor.test(Taiwan_TSI_combined$Total_Export_Value, Taiwan_TSI_combined$TSI, use = "complete.obs")
correlation_test2 <- cor.test(Taiwan_TSI_combined$Yearly_Growth, Taiwan_TSI_combined$TSI, use = "complete.obs")
correlation_test3 <- cor.test(Taiwan_TSI_combined$YoY_Change, Taiwan_TSI_combined$TSI, use = "complete.obs")

print(paste("Correlation coefficient between TradeValue and TSI:", correlation_test$estimate))
print(paste("P-value of the correlation:", correlation_test$p.value))

print(paste("Correlation coefficient between [Year to Year Change] and TSI:", correlation_test3$estimate))
print(paste("P-value of the correlation:", correlation_test3$p.value))

print(paste("Correlation coefficient between YearlyGrowth and TSI:", correlation_test2$estimate))
print(paste("P-value of the correlation:", correlation_test2$p.value))

# Step 10: Export Combined Data
write.xlsx(Taiwan_TSI_combined, file = "C:/Users/User/OneDrive/Documents/Chow_research/UN Comtrade/DataFor_TSI_Plot/Taiwan_TSI_COR.xlsx", sheetName = "TSI_dataframe", overwrite = TRUE)

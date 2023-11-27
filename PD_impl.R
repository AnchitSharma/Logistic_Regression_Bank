library(dplyr)
library(quantmod)
library(tidyquant)
market_data <- read.csv("USOI.csv")

# Convert Date column to Date format
market_data$Date <- as.Date(market_data$Date)


# Download market data using tidyquant
market_data <- tq_get("UBSG.SW", from = min(market_data$Date), to = max(market_data$Date)) %>%
  select(date, adjusted, volume) %>%
  rename(Date = date, Close = adjusted, Volume = volume)


# Step 2: Calculate Market Value of Equity (VE) and returns volatility
market_data$MarketCap <- market_data$Close * market_data$Volume
market_data$Returns <- c(NA, diff(log(market_data$Close)))

volatility <- sd(market_data$Returns, na.rm = TRUE)


# Step 3: Download balance sheet data
# Assuming you have a CSV file with balance sheet data
balance_sheet <- read.csv("balancesheet.csv")

# Convert Date column to Date format
balance_sheet$Date <- as.Date(balance_sheet$X)


# Extract relevant columns
balance_sheet <- balance_sheet %>%
  select(Date, Total.Debt, Total.Assets, ExchangeRate)  # Assuming 'Debt' and 'Assets' are relevant columns

# Correct values
balance_sheet$Debt <- balance_sheet$Total.Debt * balance_sheet$ExchangeRate
balance_sheet$Assets <- balance_sheet$Total.Assets * balance_sheet$ExchangeRate


# Step 5: Combine market and balance sheet data
merged_data <- merge(market_data, balance_sheet, by = "Date", all = TRUE)



# Step 6: Calculate the Probability of Default (PD) using the structural model
# You would need to implement the specific model based on your assumptions

# For simplicity, let's assume a basic model using Market Value of Equity and Debt
merged_data$PD <- with(merged_data, pnorm(-log(Debt/MarketCap) + (0.5 * volatility^2), 
                                          mean = 0, sd = volatility))

# Print the resulting data
print(merged_data)


library(tidyverse)

stock_data <- read.csv("UBSG.SW.csv")
# Download EURIBOR rates from ECB 
rates_data <- read_csv("euribor_rates.csv") 

# Load quarterly balance sheet data from CSV
balancesheet_data <- read_csv("balancesheet.csv")

stock_data$shares.outstanding <- 3230000000
# Compute market value of equity 
mv_equity <- Cl(stock_data) * stock_data$shares.outstanding


# Compute stock return volatility
# stock_vol <- volatility(stock_data, calc="close") 

# volatility <- sd(market_data$Returns, na.rm = TRUE)

daily_diff <- diff(log(Cl(stock_data))$Close)
stock_vol <- sqrt(252) * sd(daily_diff)

rates_data$exchangerates <- rates_data$`Euribor 3-month - Historical close, average of observations through period (FM.M.U2.EUR.RT.MM.EURIBOR3MD_.HSTA)`
rates_data$DATE <- as.Date(rates_data$DATE)

rates_data <- rates_data %>%
  select(DATE, exchangerates)

# Convert Date column to Date format
balancesheet_data$Date <- as.Date(balancesheet_data$...1)

balancesheet_data <- balancesheet_data %>%
  select("Date", "Total Debt", "Total Assets")

balancesheet_data <- merge(balancesheet_data, rates_data, by.x = c("Date"), by.y = c("DATE"))

# Convert balance sheet to Euro 
# balancesheet_data <- balancesheet_data %>%
#   mutate(across(where(is.numeric), ~.x * rates_data$exchangerates))

# Extract debt and asset values
debt <- balancesheet_data$`Total Debt` * balancesheet_data$exchangerates
assets <- balancesheet_data$`Total Assets` * balancesheet_data$exchangerates

# stock_vol is a single value
# stock_vol = 0.2969402

# Loop through each quarter  
dt_defaults <- numeric(length(assets))
for(i in 1:length(assets)){
  
  # Extract asset and debt values
  A = assets[i]  
  D = debt[i]    
  
  # Compute leverage
  L = D/A  
  
  # Extract risk free rate
  mu = rates_data[i,2]   
  
  # Calculate distance to default
  xpm <- (log(A/D) + (mu + 0.5*stock_vol^2)*1) / (stock_vol*sqrt(1))
  dt_defaults[i] = xpm[[1]]
}

# Take the average  
dt_default <- mean(dt_defaults)

# Map distance to default to probability of default  
# Using normal CDF as no access to Moody's mapping
pd <- pnorm(dt_default)



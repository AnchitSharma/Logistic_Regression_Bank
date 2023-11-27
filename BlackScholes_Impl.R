
BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type == "C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)
  }
  
  if(type == "P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- (K*exp(-r*T)*pnorm(-d2)  - S*pnorm(-d1))
    return(value)
  }
}

call <- BlackScholes(110, 100, 0.04, 1, 0.2, "C")
put <- BlackScholes(110, 100, 0.04, 1, 0.2, "P")


data <- read.csv("AAPL.csv")

# retrieve the first 50 rows
recent_rows = data[1:50, ]

# Use the arrange function in dplyr package to get old values at top.
# This would guarantee that the calculations are performed with the 
# oldest values of the series at first.

library(dplyr)
recent_data <- arrange(recent_rows, -row_number())

# Add a new column with the returns of prices.
# Input NA to the first value of the column in order 
# to fit the column in the recent_data data frame.

recent_data$returns <- c('NA', round(diff(log(recent_data$Close)), 3))

# convert the column to numeric
recent_data$returns <- as.numeric(recent_data$returns)

# calculate the standard deviation of the log returns
standard_deviation <- sd(recent_data$returns, na.rm = TRUE)

annual_sigma <- standard_deviation*sqrt(250)

















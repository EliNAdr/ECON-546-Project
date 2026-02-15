# libraries
library(vars)
library(BVAR)
library(here)
library(tseries)
library(dplyr)

# loading data
rate <- read.csv(here("Canada_data", "BOC_rate.csv"))
unemp <- read.csv(here("Canada_data", "unemployment.csv"))
inf <- read.csv(here("Canada_data", "CPI.csv"))
GDP <- read.csv(here("Canada_data", "rGDP.csv"))

# merging data, selecting variables, converting to time series, and first differencing for stationarity
merged_data <- merge(rate, unemp, by = "date")
merged_data <- merge(merged_data, inf, by = "date")
merged_data <- merge(merged_data, GDP, by = "date")

merged_data$log_real_GDP <- log(merged_data$real_GDP)

basic_data <- merged_data[c("overnight_rate","unemp_rate","log_real_GDP","CPI_MEDIAN")]

basic_ts <- ts(basic_data, start = c(1995, 1), end = c(2020, 1), frequency = 12)

basic_ts <- diff(basic_ts)

# checking for stationarity - all stationary
#print(adf.test(basic_ts[,"overnight_rate"]))
#print(adf.test(basic_ts[,"unemp_rate"]))
#print(adf.test(basic_ts[,"log_real_GDP"]))
#print(adf.test(basic_ts[,"CPI_MEDIAN"]))

# var model
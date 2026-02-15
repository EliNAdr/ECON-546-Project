# libraries
library(vars)
library(here)
library(tseries)
library(dplyr)
library(ggplot2)

# loading data & standardizing formats
rate <- read.csv(here("Canada_data", "BOC_rate.csv"))
unemp <- read.csv(here("Canada_data", "unemployment.csv"))
inf <- read.csv(here("Canada_data", "CPI.csv"))
GDP <- read.csv(here("Canada_data", "rGDP.csv"))
exch <- read.csv(here("Canada_data", "CAN2US.csv"))
oil <- read.csv(here("Canada_data", "WTI.csv"))

GDP$date <- as.Date(paste0("01 ", GDP$date), format = "%d %B %Y")
GDP$rGDP <- as.numeric(gsub("[^0-9.-]", "", GDP$Dollars))

# merging data, selecting variables, converting to time series, and first differencing log rGDP
merged_data <- merge(rate, unemp, by = "date")
merged_data <- merge(merged_data, inf, by = "date")
merged_data <- merge(merged_data, GDP, by = "date")
merged_data <- merge(merged_data, exch, by = "date")
merged_data <- merge(merged_data, oil, by = "date")

merged_data$log_real_GDP <- log(merged_data$rGDP)
basic_data <- merged_data[c("WTISPLC", "log_real_GDP","CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate")]

basic_data$log_real_GDP <- c(NA, diff(basic_data$log_real_GDP))
basic_data$CAD2USD <- c(NA, diff(basic_data$CAD2USD))
basic_data$WTISPLC <- c(NA, diff(basic_data$WTISPLC))

basic_ts <- ts(basic_data, start = c(1997, 1), end = c(2020, 1), frequency = 12)
basic_ts <-  na.omit(basic_ts)

# var model & IRFs
var_model <- VAR(basic_ts[, c("log_real_GDP", "CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate")], type = "const", p = 12, exogen =  basic_ts[, "WTISPLC"])
#print(var_model)
irf_results <- get_var_irf(var_model, shock = "overnight_rate", resp = c("log_real_GDP", "CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate"), ortho = TRUE, horizon = 36, plot = TRUE)
print(irf_results$irf_chart)
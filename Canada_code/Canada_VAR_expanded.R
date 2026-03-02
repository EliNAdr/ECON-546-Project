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
merged_data$GFC <- ifelse(as.Date(merged_data$date) >= as.Date("2007-01-01") & merged_data$date <= as.Date("2009-12-31"), 1, 0)
merged_data$GFCt <- merged_data$GFC * as.numeric(difftime(merged_data$date, as.Date("2007-01-01"), units = "days")) / 30

basic_data <- merged_data[c("WTISPLC", "log_real_GDP","CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate", "GFC", "GFCt")]

basic_data$log_real_GDP <- c(NA, diff(basic_data$log_real_GDP))
basic_data$CAD2USD <- c(NA, diff(basic_data$CAD2USD))
basic_data$WTISPLC <- c(NA, diff(basic_data$WTISPLC))
basic_data$unemp_rate <- c(NA, diff(basic_data$unemp_rate))

basic_ts <- ts(basic_data, start = c(1997, 1), end = c(2020, 1), frequency = 12)
basic_ts <-  na.omit(basic_ts)

# var model & IRFs
#var_model <- VAR(basic_ts[, c("log_real_GDP", "CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate")], type = "const", p = 12, exogen =  basic_ts[, c("WTISPLC", "GFC", "GFCt")])
var_model <- VAR(basic_ts[, c("log_real_GDP", "CPI_MEDIAN", "unemp_rate", "CAD2USD", "overnight_rate")], type = "const", p = 12, exogen =  basic_ts[, "WTISPLC"])
colnames(var_model$y) <- c(
  "Real GDP",
  "CPI Median Inflation (YoY)",
  "Unemployment Rate",
  "CAD to USD Exchange Rate",
  "BoC Policy Rate"
)

#print(var_model)
irf_results <- get_var_irf(var_model, shock = "BoC Policy Rate", resp = c("Real GDP", "CPI Median Inflation (YoY)", "Unemployment Rate", "CAD to USD Exchange Rate", "BoC Policy Rate"), ortho = TRUE, horizon = 36, plot = TRUE)
print(irf_results$irf_chart)

ggsave(irf_results$irf_chart, filename = here("Canada_ext_12mth_irf.png"))
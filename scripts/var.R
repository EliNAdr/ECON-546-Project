library(vars)
library(BVAR)

source("scripts\\data.R")

series = c("FEDFUNDS", "UNRATE", "PCE", "INDPRO", "WTISPLC", "RTWEXBGS")

us_var_data <- get_fred_data(
    series = series, start = "1985-01-01", end = "2019-01-01"
)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(xts)
library(fredr)
library(cansim)

## --- Pulling data for the USA ---

us_params <- list(
    series_id = c("INDPRO", # industrial production
                  "UNRATE", # unemployment rate
                  "FEDFUNDS", # federal funds rate
                  "PCE", # CPI for urban consumers
                  "PCETRIM12M159SFRBDAL", # trimmed mean PCE
                  "WTISPLC", # WTI crude oil spot price
                  "RTWEXBGS"), # broad US dollar index
    frequency = c("m")
)

us_var_data <- purrr::pmap_dfr(
    .l = us_params,
    .f = ~ fredr(series_id = .x, frequency = .y)
)

write.csv(us_var_data, "us_var_data.csv")
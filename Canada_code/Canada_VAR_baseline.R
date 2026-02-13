# libraries
library(vars)
library(BVAR)
library(here)

# loading data
rate <- read.csv(here("Canada_data", "BOC_rate.csv"))
unemp <- read.csv(here("Canada_data", "Can_unemployment.csv"))
inf <- read.csv(here("Canada_data", "CPI.csv"))
GDP <- read.csv(here("Canada_data", "rGDP.csv"))

print(head(GDP))
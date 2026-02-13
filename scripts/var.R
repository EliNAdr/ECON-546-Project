source("./scripts/data.R")

series <- c("INDPRO", "UNRATE", "MEDCPIM158SFRBCLE", "FEDFUNDS", "WTISPLC")
us_var_data <- get_fred_data(series = series, start = "1985-01-01", end = "2019-01-01")
us_var_data_stn <- stationarity_check(us_var_data, lag = 1, order = 1)

lag_selection <- VARselect(us_var_data[, c(2:5)], lag.max = 10)
var_mod <- VAR(us_var_data_stn[, c(2:5)], p = 6)
irf <- irf(var_mod, ortho = TRUE, n.ahead = 12) %>% plot()


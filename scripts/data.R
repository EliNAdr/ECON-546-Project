library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(fredr)
library(docstring)

## --- Pulling data for the USA ---

get_fred_data <- function(series, freq = c("m"), start, end) {
    #' Pull data from FRED
    #' 
    #' Calls FRED to pull data on specified series with custom time windows
    #' 
    #' @param series A character vector of IDs corresponding to a FRED series. If using Cholesky identification,
    #' series must be arranged in order of decreasing exogeneity.
    #' @param freq One-element vector specifying data frequency, monthly by default
    #' @param start Start date for the filtered series
    #' @param end End date for the filtered series
    #' @return A filtered dataframe
    #' @examples 
    #' get_fred_data(c("FEDFUNDS"),"2007-01-01", "2010-01-01")

    # get info for FRED data series
    params <- list(series_id = series, frequency = freq)

    # call data from FRED
    data <- purrr::pmap_dfr(
        .l = params,
        .f = ~ fredr(series_id = .x, frequency = .y))   

    # filter data to match specified windows
    data_clean <- data %>%
        dplyr::select(
            date, series_id, value) %>%
        filter(
            date >= as_date(start) & 
            date <= as_date(end)) %>%
        pivot_wider(
            names_from = series_id,
            values_from = value)
    
    return(data_clean)
}

us_var_data <- get_fred_data(series = series, "1985-01-01", "2019-01-01")
stationarity_check <- function(df, lag = 1, order = 1) {
    #' Automated stationarity checks for time-series data
    #' 
    #' Checks dataframe containing time-series variables for stationarity via an ADF test
    #' and applies first-differencing for nonstationary variables
    #' 
    #' @param df A dataframe containing time-series data
    #' @param lag The lag to use if applying differencing
    #' @param diff The order of difference to use 
    #' @return A filtered dataframe
    #' @examples
    #' stationarity_check(var_data, lag = 1, diff = 1)
    
    # preliminary checks
    if (is.data.frame(df) != TRUE) {
        stop("df must be a dataframe")
    }

    # check columns iteratively for stationarity
    cols <- colnames(df[, -1])
    for (col in cols) {
        res <- tseries::adf.test(df[[col]])
        if (res$p.value >= 0.05) {
            df[[col]] <- collapse::fdiff(df[[col]], n = lag, diff = order, fill = NA)
        } else {
            df[[col]] <- df[[col]]
        }
    }
    df <- na.omit(df) # drop first NA row
    return(df)
}
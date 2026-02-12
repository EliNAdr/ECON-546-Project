library(dplyr)
library(tidyr)
library(purrr)
library(fredr)
library(docstring)
library(vars)

## --- Pulling data for the USA ---

get_fred_data <- function(series, freq = c("m"), start, end) {
    #' Pull data from FRED
    #' 
    #' Calls FRED to pull data on specified series with custom time windows
    #' 
    #' @param series A character vector of IDs corresponding to a FRED series
    #' @param freq One-element vector specifying data frequency, monthly by default
    #' @param start Start date for the filtered series
    #' @param end End date for the filtered series
    #' @return A filtered dataframe
    #' @examples 
    #' get_fred_data(c("FEDFUNDS"),"2007-01-01", "2010-01-01")

    # get info for FRED data series
    params <- list(
        series_id = series,
        frequency = freq)

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
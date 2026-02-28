library(fredr)
library(docstring)

# set API key for FRED
fredr_set_key("243ab59b7e8b76bd8bd9cd4c53e694d3")

## --- Pulling data for the USA ---
get_fred_data <- function(series, freq = c("m"), start, end) {
    #' Wrapper function to pull data from FRED
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
    data_clean <- data |>
        dplyr::select(
            date, series_id, value) |>
        dplyr::filter(
            date >= lubridate::as_date(start) & 
            date <= lubridate::as_date(end)) |>
        tidyr::pivot_wider(
            names_from = series_id,
            values_from = value)
    
    return(data_clean)
}

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
    
    # preliminary check
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
library(fredr)
library(docstring)

# set API key for FRED
fredr_set_key("")

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

stationarity_check <- function(df, lag = 1, order = 1,
                               keep_levels = c("UNRATE", "FEDFUNDS")) {

  if (!is.data.frame(df)) stop("df must be a dataframe")

  diffed <- c()
  levels <- c()

  cols <- colnames(df[, -1])

  for (col in cols) {

    # Force some variables to stay in levels
    if (col %in% keep_levels) {
      levels <- c(levels, col)
      next
    }

    # Otherwise use ADF decision
    res <- tseries::adf.test(df[[col]])
    if (res$p.value >= 0.05) {
      df[[col]] <- collapse::fdiff(df[[col]], n = lag, diff = order, fill = NA)
      diffed <- c(diffed, col)
    } else {
      levels <- c(levels, col)
    }
  }

  df <- na.omit(df)

  return(list(
    data = df,
    differenced = diffed,
    levels = levels
  ))
}

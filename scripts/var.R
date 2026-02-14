library(vars)
source("./scripts/data.R")

series <- c("INDPRO", "UNRATE", "MEDCPIM158SFRBCLE", "FEDFUNDS", "WTISPLC")
us_var_data <- get_fred_data(series = series, start = "1985-01-01", end = "2019-01-01")
us_var_data_stn <- stationarity_check(us_var_data, lag = 1, order = 1)

get_var_irf <- function(y_t, p_max, ortho = FALSE, k_steps, 
                        plot = FALSE, which_var = NULL) {
    #' Wrapper function to fit a VAR model with automatic lag selection
    #' 
    #' Fits a VAR model for the supplied matrix Y_t with lag length automatically determined 
    #' via AIC and returns impulse response functions
    #' 
    #' @param y_t Dataframe containing endogenous variables for the VAR
    #' @param p_max Maximum number of lags to consider for lag selection
    #' @param ortho Whether to compute orthogonalised shocks. FALSE by default.
    #' @param k_steps Time horizon k for the impulse responses
    #' @param plot Return plots of computed impulse responses?
    #' @param which_var Variable to return impulse responses for
    #' @return A list containing criteria for lag selection, fitted coefficients, and impulse responses
    #' @examples
    #' get_var_irf(y_t = us_var_data, max_lag = 10, k_steps = 12)
    
    # preliminary check
    if (is.data.frame(y_t) != TRUE) {
        stop("y_t must be a data frame")
    }

    # run lag selection via AIC
    lagselect <- VARselect(y_t, lag.max = p_max)
    optim_lag <- lagselect$selection[1]

    # fit VAR with optimal lag length
    mod <- VAR(y_t, p = optim_lag)
    irf <- irf(mod, ortho = ortho, n.ahead = k_steps)$irf

    # collect results
    out <- list(
        lag_selection = lagselect$criteria,
        var_coefs = mod$varresults,
        irf = irf)
    
    if (plot == TRUE) {
        irf_data <- irf[[which_var]] |> 
            as.data.frame() |>
            pivot_longer(cols = c(colnames(y_t)),
                names_to = "variable",
                values_to = "value") |>
            dplyr::mutate(t = rep(c(1:(k_steps+1)), length(colnames(y_t))))
        
        irf_plot <- ggplot2::ggplot(
            data = irf_data, aes(x = t, y = value)) +
            geom_line(linewidth = 1.2, color = "black") + 
            facet_wrap(~ variable)
        
        out$plot <- irf_plot
    }
    return(out)
}

test <- get_var_irf(y_t = us_var_data[, 2:5], p_max = 10, k_steps = 12, 
                    ortho = TRUE, plot = TRUE, which_var = "FEDFUNDS")
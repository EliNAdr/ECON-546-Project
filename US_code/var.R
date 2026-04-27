# ============================================================
# VAR + IRF pipeline (top-to-bottom)
# - Uses FRED via fredr
# - Optional EBP series read from Excel
# - Fits VAR (Cholesky ordering)
# - Computes IRFs and returns output in the SAME format as "their code":
#     out$irf (tidy df) and out$irf_chart (ggplot, optional)
# ============================================================

# =========================
# 0) Packages
# =========================
suppressPackageStartupMessages({
  library(fredr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(vars)
  library(tseries)
  library(ggplot2)
  library(readxl)
  library(lubridate)
})

# Prefer: set your key in .Renviron as FRED_API_KEY="..."
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# =========================
# 1) Read EBP from Excel
# =========================
read_ebp_excel <- function(path, sheet = 1, date_col = "date", ebp_col = "ebp") {

  ebp_raw <- readxl::read_excel(path, sheet = sheet)

  # make names consistent
  names(ebp_raw) <- tolower(names(ebp_raw))
  date_col <- tolower(date_col)
  ebp_col  <- tolower(ebp_col)

  if (!(date_col %in% names(ebp_raw))) stop("EBP file missing date column: ", date_col)
  if (!(ebp_col  %in% names(ebp_raw))) stop("EBP file missing ebp column: ", ebp_col)

  ebp <- ebp_raw %>%
    transmute(
      date = .data[[date_col]],
      ebp  = as.numeric(.data[[ebp_col]])
    ) %>%
    mutate(
      date = case_when(
        inherits(date, "Date") ~ date,
        is.numeric(date) ~ as.Date(date, origin = "1899-12-30"), # Excel numeric dates
        TRUE ~ as.Date(date)
      ),
      # Force to month start to align with monthly FRED dates
      date = as.Date(format(date, "%Y-%m-01"))
    ) %>%
    arrange(date)

  ebp
}

# =========================
# 2) Pull data (raw) from FRED
# =========================
get_fred_data <- function(series, start, end) {

  stopifnot(is.character(series), length(series) >= 1)

  raw <- purrr::map_dfr(series, ~ fredr(series_id = .x))

  raw_wide <- raw %>%
    select(date, series_id, value) %>%
    filter(date >= as.Date(start), date <= as.Date(end)) %>%
    pivot_wider(names_from = series_id, values_from = value) %>%
    arrange(date)

  raw_wide
}

# =========================
# 3) Transformations
# =========================
dlog  <- function(x) c(NA, diff(log(x)))
diff1 <- function(x) c(NA, diff(x))
dlog12 <- function(x) c(rep(NA, 12), diff(log(x), lag = 12))

transform_us_monetary_var <- function(df) {
  needed <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH", "ebp")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing series in df: ", paste(miss, collapse = ", "))

  out <- df %>%
    transmute(
      date     = date,
      oil      = 100 * dlog(WTISPLC),
      g_indpro = 100 * dlog(INDPRO),
      unrate   = UNRATE,
      pi_cpi   = 100 * dlog(CPILFESL),
      exp_infl = MICH,       # inflation expectations in level (%)
      ebp      = ebp,        # level
      fedfunds = FEDFUNDS    # level (%)
    )

  ledger <- tibble::tribble(
    ~variable, ~source,      ~transform,               ~units,
    "oil",      "WTISPLC",   "100 * Δlog(WTISPLC)",    "percent (m/m)",
    "g_indpro", "INDPRO",    "100 * Δlog(INDPRO)",     "percent (m/m)",
    "unrate",   "UNRATE",    "level",                  "percent",
    "pi_cpi",   "CPILFESL ",  "100 * Δlog(CPILFESL )",   "percent (y/y)",
    "exp_infl", "MICH",      "level",                  "percent",
    "ebp",      "Excel",     "level",                  "spread (level)",
    "fedfunds", "FEDFUNDS",  "level",                  "percent"
  )

  list(data = out %>% tidyr::drop_na(), ledger = ledger)
}

# =========================
# 4) Stationarity diagnostics (report only)
# =========================
adf_report <- function(df, cols = names(df)[names(df) != "date"]) {
  purrr::map_dfr(cols, function(v) {
    x <- df[[v]]
    res <- tseries::adf.test(x)
    tibble(
      variable = v,
      adf_stat = unname(res$statistic),
      p_value  = res$p.value
    )
  }) %>% arrange(p_value)
}

# =========================
# 5) Fit VAR (Cholesky ordering)
# =========================
fit_var_cholesky <- function(df, order, p_fixed = 6, type = "const") {

  if (!is.numeric(p_fixed) || length(p_fixed) != 1 || is.na(p_fixed) || p_fixed < 1) {
    stop("p_fixed must be a single integer >= 1")
  }

  p <- as.integer(p_fixed)

  Y <- df %>% dplyr::select(dplyr::all_of(order))
  mod <- vars::VAR(Y, p = p, type = type)

  list(
    model      = mod,
    p          = p,
    lag_method = paste0("fixed p=", p),
    ordering   = order
  )
}

# =========================
# 6) IRF wrapper (UPDATED: formats output like your new code)
# =========================
get_var_irf <- function(mod, shock, resp = NULL, ortho = FALSE, horizon, plot = FALSE) {
  #' Wrapper function to get impulse responses from a VAR model
  #'
  #' Computes impulse responses for a specified VAR model, aggregates the results
  #' into a dataframe and returns plots of the IRFs if required.
  #'
  #' @param mod Object of class 'varest'; a fitted VAR model
  #' @param shock A string indicating the variable to shock when computing IRFs
  #' @param resp A character vector indicating the variable(s) for which to calculate impulses responses
  #' @param ortho Whether to compute orthogonalised shocks; FALSE by default
  #' @param horizon Time horizon for the impulse responses; choose wisely
  #' @param plot Return plots of computed impulse responses? FALSE by default
  #' @return A list containing a dataframe of impulse responses and an optional ggplot2 object
  #' @examples
  #' get_var_irf(mod = var_mod, shock = "FEDFUNDS", resp = "INDPRO", horizon = 12)

  # preliminary check
  if (class(mod) != "varest") {
    stop("model provided must be of class 'varest'")
  }

  if (missing(horizon) || length(horizon) != 1 || horizon < 1) {
    stop("horizon must be a single integer >= 1")
  }

  # compute IRFs
  irf_out <- vars::irf(
    mod,
    impulse  = shock,
    response = resp,
    ortho    = ortho,
    n.ahead  = horizon
  )

  # extract IRF coefficients and CI bands and collapse to dataframe
  irf_to_df <- function(irf_obj) {
    df <- purrr::map_df(names(irf_obj$irf), function(shock) {
      irfs  <- irf_obj$irf[[shock]]
      lower <- irf_obj$Lower[[shock]]
      upper <- irf_obj$Upper[[shock]]

      purrr::map_df(seq_len(ncol(irfs)), function(j) {
        tibble::tibble(
          horizon     = 0:(nrow(irfs) - 1),
          shock       = shock,
          response    = colnames(irfs)[j],
          coefficient = as.numeric(irfs[, j]),
          lower       = as.numeric(lower[, j]),
          upper       = as.numeric(upper[, j])
        )
      })
    })
    return(df)
  }

  out <- list(irf = irf_to_df(irf_out))

  # generate plot of IRFs if required
  if (plot == TRUE) {
    chart <- ggplot2::ggplot(
      data = out$irf, ggplot2::aes(x = horizon)
    ) +
      ggplot2::geom_hline(
        yintercept = 0.0, linetype = "dashed"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = coefficient), linewidth = 1.2, color = "blue"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = lower), linewidth = 1.0, linetype = "dashed"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = upper), linewidth = 1.0, linetype = "dashed"
      ) +
      ggplot2::facet_wrap(
        ~ response, ncol = 2, scales = "free_y"
      ) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 14, face = "bold")
      )

    out$irf_chart <- chart
  }

  return(out)
}

# ============================================================
# 7) RUN (US example): Oil + Inflation Expectations + EBP
# ============================================================

# --- choose sample window
start_date <- "1985-01-01"
end_date   <- "2007-01-01"

# --- FRED series (monthly)
series_us <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH")

raw_us <- get_fred_data(series_us, start = start_date, end = end_date)

# --- Read EBP from Excel (EDIT THESE)
ebp <- read_ebp_excel(
  path = "/Users/sophiascarcella/Desktop/Data.xlsx",
  sheet = 2,
  date_col = "observation_date",
  ebp_col  = "ebp"
)

# --- Merge EBP into panel (monthly align)
raw_us2 <- raw_us %>%
  mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
  left_join(ebp, by = "date")

# --- Transform
tr <- transform_us_monetary_var(raw_us2)
us <- tr$data

print(tr$ledger)
print(adf_report(us))

# --- Ordering (Cholesky)
order_us <- c("oil", "exp_infl", "g_indpro", "unrate", "pi_cpi", "ebp", "fedfunds")

# --- Fit VAR (fixed p=6)
fit_us <- fit_var_cholesky(us, order = order_us, p_fixed = 6, type = "const")
cat("Lag method:", fit_us$lag_method, "| p =", fit_us$p, "\n")
cat("Ordering:", paste(fit_us$ordering, collapse = " → "), "\n")

# --- IRF (UPDATED output object)
mod_irf <- get_var_irf(
  fit_us$model,
  shock   = "fedfunds",
  resp    = c("g_indpro", "unrate", "pi_cpi", "fedfunds"),
  ortho   = TRUE,
  horizon = 36,
  plot    = TRUE
)

# Optional: rename response labels (if you still want nice facet labels)
label_map <- c(
  g_indpro = "Industrial Production",
  unrate   = "Unemployment Rate",
  pi_cpi   = "CPI Inflation (MoM)",
  fedfunds = "Federal Funds Rate"
)

mod_irf$irf <- mod_irf$irf %>%
  mutate(response = as.character(response)) %>%
  mutate(response = dplyr::recode(response, !!!label_map))

# If you want the plot to reflect renamed labels, re-create the chart from mod_irf$irf:
mod_irf$irf_chart <- ggplot2::ggplot(mod_irf$irf, ggplot2::aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0.0, linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coefficient), linewidth = 1.2, color = "blue") +
  ggplot2::geom_line(ggplot2::aes(y = lower), linewidth = 1.0, linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = upper), linewidth = 1.0, linetype = "dashed") +
  ggplot2::facet_wrap(~ response, ncol = 2, scales = "free_y") +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = "bold"))

print(mod_irf$irf_chart)

ggsave(
  filename = "base_MoM_6.png",
  plot     = mod_irf$irf_chart,
  width    = 12,
  height   = 8
)
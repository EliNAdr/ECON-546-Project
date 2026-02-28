# =========================
# 0) Packages
# =========================
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

# Prefer: set your key in .Renviron as FRED_API_KEY="..."
fredr_set_key(Sys.getenv("FRED_API_KEY"))

read_ebp_excel <- function(path, sheet = 1, date_col = "date", ebp_col = "ebp") {

  ebp_raw <- readxl::read_excel(path, sheet = sheet)

  # Make names easier (optional)
  names(ebp_raw) <- tolower(names(ebp_raw))

  # If your columns aren’t literally "date" and "ebp", change date_col/ebp_col above
  date_col <- tolower(date_col)
  ebp_col <- tolower(ebp_col)

  if (!(date_col %in% names(ebp_raw))) stop("EBP file missing date column: ", date_col)
  if (!(ebp_col %in% names(ebp_raw))) stop("EBP file missing ebp column: ", ebp_col)

  ebp <- ebp_raw %>%
    transmute(
      date = .data[[date_col]],
      ebp = as.numeric(.data[[ebp_col]])
    ) %>%
    mutate(
      # Convert date formats robustly
      date = case_when(
        inherits(date, "Date") ~ date,
        is.numeric(date) ~ as.Date(date, origin = "1899-12-30"), # Excel numeric dates
        TRUE ~ as.Date(date)
      ),
      # Force to month start (matches your FRED monthly dates style)
      date = as.Date(format(date, "%Y-%m-01"))
    ) %>%
    arrange(date)

  ebp
}

# =========================
# 1) Pull data (raw)
# =========================
get_fred_data <- function(series, start, end) {

  stopifnot(is.character(series), length(series) >= 1)

  raw <- map_dfr(series, ~ fredr(series_id = .x))

  raw_wide <- raw %>%
    select(date, series_id, value) %>%
    filter(date >= as.Date(start), date <= as.Date(end)) %>%
    pivot_wider(names_from = series_id, values_from = value) %>%
    arrange(date)

  raw_wide
}

# =========================
# 2) Explicit transformations
# =========================
# Helpers
dlog <- function(x) c(NA, diff(log(x)))
diff1 <- function(x) c(NA, diff(x))

# -------------------------
# Add inflation expectations
# -------------------------
# Default series choice:
# - MICH: University of Michigan 1-year inflation expectations (monthly, percent)
# FRED series id: "MICH"
#
# Alternative options you might prefer (not used here):
# - "EXPINF1YR" (1-year expected inflation, percent) if available at your frequency/sample
# - "T10YIE" / breakevens are daily and need aggregation to monthly
#
# We'll include MICH as a LEVEL (percent), and put it BEFORE fedfunds in the Cholesky ordering.

transform_us_monetary_var <- function(df) {
#WTISPLC
#PPIACO
#CPIAUCSL
#CPILFESL
  needed <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH", "ebp")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing series in df: ", paste(miss, collapse = ", "))

  out <- df %>%
    transmute(
      date = date,
      oil = 100 * dlog(WTISPLC),
      g_indpro = 100 * dlog(INDPRO),
      unrate = UNRATE,
      pi_cpi = 100 * dlog(CPILFESL),
      # Inflation expectations (level, percent)
      exp_infl = MICH,
      # EBP in levels
      ebp = ebp,
      fedfunds = FEDFUNDS
    )

  ledger <- tibble::tribble(
    ~variable, ~fred_series, ~transform, ~units,
    "oil", "WTISPLC", "100 * Δlog(WTISPLC)", "percent (m/m)",
    "g_indpro", "INDPRO", "100 * Δlog(INDPRO)", "percent (m/m)",
    "unrate", "UNRATE", "level", "percent",
    "pi_cpi", "CPILFESL", "100 * Δlog(CPILFESL)", "percent (m/m)",
    "exp_infl", "MICH", "level", "percent",
    "fedfunds", "FEDFUNDS", "level", "percent",
    "ebp", "Excel", "level", "spread (level)"
  )

  list(data = out %>% tidyr::drop_na(), ledger = ledger)
}

# =========================
# 3) Stationarity diagnostics (REPORT ONLY)
# =========================
adf_report <- function(df, cols = names(df)[names(df) != "date"]) {

  map_dfr(cols, function(v) {
    x <- df[[v]]
    res <- tseries::adf.test(x)

    tibble(
      variable = v,
      adf_stat = unname(res$statistic),
      p_value = res$p.value
    )
  }) %>%
    arrange(p_value)
}

# =========================
# 4) Fit VAR with chosen ordering
# =========================
fit_var_cholesky <- function(df, order, lag_max = 12, ic = c("AIC", "HQ", "SC", "FPE"),
                             p_fixed = NULL, type = "const") {

  Y <- df %>% dplyr::select(dplyr::all_of(order))

  if (!is.null(p_fixed)) {
    if (!is.numeric(p_fixed) || length(p_fixed) != 1 || p_fixed < 1) {
      stop("p_fixed must be a single integer >= 1")
    }
    p_opt <- as.integer(p_fixed)
    mod <- vars::VAR(Y, p = p_opt, type = type)

    return(list(
      model = mod,
      p = p_opt,
      lag_select = NULL,
      lag_method = paste0("fixed p=", p_opt),
      ordering = order
    ))
  }

  ic <- match.arg(ic)
  ic_map <- c(
    AIC = "AIC(n)",
    HQ  = "HQ(n)",
    SC  = "SC(n)",  # Schwarz criterion = BIC
    FPE = "FPE(n)"
  )

  lag_sel <- vars::VARselect(Y, lag.max = lag_max, type = type)
  p_opt <- as.integer(lag_sel$selection[ ic_map[[ic]] ])
  mod <- vars::VAR(Y, p = p_opt, type = type)

  list(
    model = mod,
    p = p_opt,
    lag_select = lag_sel,
    lag_method = paste0("IC: ", ic, " (", ic_map[[ic]], ")"),
    ordering = order
  )
}

fit_var_cholesky_varx <- function(df, order_endog, X_exog, lag_max = 12, ic = c("AIC", "HQ", "SC", "FPE"),
                                  p_fixed = NULL, type = "const") {

  Y <- df %>% dplyr::select(dplyr::all_of(order_endog))

  X <- as.matrix(X_exog)
  storage.mode(X) <- "double"

  if (!is.null(p_fixed)) {
    p_opt <- as.integer(p_fixed)
    mod <- vars::VAR(Y, p = p_opt, type = type, exogen = X)

    return(list(
      model = mod,
      p = p_opt,
      lag_select = NULL,
      lag_method = paste0("fixed p=", p_opt),
      ordering = order_endog
    ))
  }

  ic <- match.arg(ic)
  ic_map <- c(AIC="AIC(n)", HQ="HQ(n)", SC="SC(n)", FPE="FPE(n)")

  lag_sel <- vars::VARselect(Y, lag.max = lag_max, type = type, exogen = X)
  p_opt <- as.integer(lag_sel$selection[ ic_map[[ic]] ])
  mod <- vars::VAR(Y, p = p_opt, type = type, exogen = X)

  list(
    model = mod,
    p = p_opt,
    lag_select = lag_sel,
    lag_method = paste0("IC: ", ic, " (", ic_map[[ic]], ")"),
    ordering = order_endog
  )
}

# =========================
# 5) Tidy IRFs + plotting
# =========================
get_var_irf_tidy <- function(mod, shock, horizon = 36, ortho = TRUE, boot = TRUE) {

  irf_out <- vars::irf(
    mod,
    impulse = shock,
    response = NULL,
    n.ahead = horizon,
    ortho = ortho,
    boot = boot
  )

  map_df(names(irf_out$irf), function(imp) {
    irfs <- irf_out$irf[[imp]]
    lower <- irf_out$Lower[[imp]]
    upper <- irf_out$Upper[[imp]]

    map_df(seq_len(ncol(irfs)), function(j) {
      tibble(
        horizon = 0:(nrow(irfs) - 1),
        impulse = imp,
        response = colnames(irfs)[j],
        coefficient = as.numeric(irfs[, j]),
        lower = as.numeric(lower[, j]),
        upper = as.numeric(upper[, j])
      )
    })
  })
}

plot_irf_facets <- function(irf_df, title = NULL, subtitle = NULL) {

  ggplot(irf_df, aes(x = horizon, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ response, ncol = 1, scales = "free_y") +
    labs(title = title, subtitle = subtitle, x = "Horizon", y = "Response") +
    theme_minimal()
}

# =========================
# 6) RUN (US) with OIL + inflation expectations
# =========================
# Added "MICH" for inflation expectations
series_us <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH")

raw_us <- get_fred_data(series_us, start = "1985-01-01", end = "2007-01-01")

# Read EBP from Excel
ebp <- read_ebp_excel(
  path = "/Users/sophiascarcella/Desktop/Data.xlsx",
  sheet = 2,
  date_col = "observation_date", # change if needed
  ebp_col = "ebp"                # change if needed
)

# Merge onto the monthly panel
raw_us2 <- raw_us %>%
  mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
  left_join(ebp, by = "date")

tr <- transform_us_monetary_var(raw_us2)
us <- tr$data

print(tr$ledger)
print(adf_report(us))

# Baseline oil-first ordering with expectations before policy rate
#order_us_oilfirst_exp <- c("oil", "g_indpro", "unrate", "pi_cpi", "exp_infl", "fedfunds", "ebp")

order_us_oilfirst_exp <- c("oil", "exp_infl", "g_indpro", "unrate", "pi_cpi", "ebp", "fedfunds")


fit_oilfirst_exp <- fit_var_cholesky(us, order = order_us_oilfirst_exp, p_fixed = 6, type = "const" )

cat("Lag method:", fit_oilfirst_exp$lag_method, "| Selected p = ", fit_oilfirst_exp$p, "\n")



irf_ff_oilfirst_exp <- get_var_irf_tidy(
  fit_oilfirst_exp$model,
  shock = "fedfunds",
  horizon = 36,
  ortho = TRUE,
  boot = TRUE
)

p1 <- plot_irf_facets(
  irf_ff_oilfirst_exp,
  title = "IRFs to a monetary policy shock (Cholesky) — with Oil + Inflation Expectations",
  subtitle = paste0(
    "Ordering: ", paste(order_us_oilfirst_exp, collapse = " → "),
    " | ", fit_oilfirst_exp$lag_method
  )
)

print(p1)

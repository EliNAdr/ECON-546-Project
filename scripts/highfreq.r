# ============================================================
# FULL SCRIPT: Cholesky VAR + Proxy SVAR (External Instrument)
# Uses your Excel column named "instrument" as the SVAR-IV shock
# ============================================================

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

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# =========================
# 1) Helpers
# =========================
dlog  <- function(x) c(NA, diff(log(x)))
diff1 <- function(x) c(NA, diff(x))

get_fred_data <- function(series, start, end) {
  stopifnot(is.character(series), length(series) >= 1)
  raw <- map_dfr(series, ~ fredr(series_id = .x))
  raw %>%
    select(date, series_id, value) %>%
    filter(date >= as.Date(start), date <= as.Date(end)) %>%
    pivot_wider(names_from = series_id, values_from = value) %>%
    arrange(date)
}

read_excel_monthly <- function(path, sheet = 1, date_col, value_col, value_name) {
  xraw <- readxl::read_excel(path, sheet = sheet)
  names(xraw) <- tolower(names(xraw))
  date_col  <- tolower(date_col)
  value_col <- tolower(value_col)

  if (!(date_col %in% names(xraw))) stop("Excel missing date column: ", date_col)
  if (!(value_col %in% names(xraw))) stop("Excel missing value column: ", value_col)

  xraw %>%
    transmute(
      date = .data[[date_col]],
      !!value_name := as.numeric(.data[[value_col]])
    ) %>%
    mutate(
      date = case_when(
        inherits(date, "Date") ~ date,
        is.numeric(date) ~ as.Date(date, origin = "1899-12-30"),
        TRUE ~ as.Date(date)
      ),
      date = as.Date(format(date, "%Y-%m-01"))
    ) %>%
    arrange(date)
}

adf_report <- function(df, cols = names(df)[names(df) != "date"]) {
  map_dfr(cols, function(v) {
    res <- tseries::adf.test(df[[v]])
    tibble(variable = v, adf_stat = unname(res$statistic), p_value = res$p.value)
  }) %>% arrange(p_value)
}

# =========================
# 2) Transform dataset
# =========================
transform_us_monetary_var <- function(df) {
  needed <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH", "ebp", "instrument")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing series in df: ", paste(miss, collapse = ", "))

  out <- df %>%
    transmute(
      date     = date,
      oil      = 100 * dlog(WTISPLC),
      exp_infl = MICH,                  # level, percent
      g_indpro = 100 * dlog(INDPRO),
      unrate   = UNRATE,                # level, percent
      pi_cpi   = 100 * dlog(CPILFESL),  # core CPI inflation, m/m %
      ebp      = ebp,                   # level
      fedfunds = FEDFUNDS,              # level, percent
      instrument = instrument           # external instrument series
    ) %>%
    tidyr::drop_na()

  ledger <- tibble::tribble(
    ~variable, ~source, ~transform, ~units,
    "oil",      "WTISPLC",  "100*Δlog(WTISPLC)",  "% m/m",
    "exp_infl", "MICH",     "level",              "% (level)",
    "g_indpro", "INDPRO",   "100*Δlog(INDPRO)",   "% m/m",
    "unrate",   "UNRATE",   "level",              "% (level)",
    "pi_cpi",   "CPILFESL", "100*Δlog(CPILFESL)", "% m/m",
    "ebp",      "Excel",    "level",              "level",
    "fedfunds", "FEDFUNDS", "level",              "% (level)",
    "instrument","Excel",   "level",              "instrument units"
  )

  list(data = out, ledger = ledger)
}

# =========================
# 3) Fit reduced-form VAR (same as your approach)
# =========================
fit_var <- function(df, order, p_fixed = 6, type = "const") {
  Y <- df %>% select(all_of(order))
  mod <- vars::VAR(Y, p = as.integer(p_fixed), type = type)
  list(model = mod, p = as.integer(p_fixed), ordering = order)
}

# =========================
# 4) Proxy SVAR (SVAR-IV) IRFs
#    Identify shock using instrument z_t:
#      b ∝ Cov(u_t, z_t)
#    Normalize so fedfunds impact = +1 on impact.
# =========================
var_ma_coefs <- function(mod, n_ahead) {
  A <- vars::Acoef(mod)          # list length p, each KxK
  K <- ncol(mod$y)
  p <- mod$p

  # Companion matrix F (Kp x Kp)
  Fmat <- matrix(0, nrow = K * p, ncol = K * p)
  # Top block: [A1 A2 ... Ap]
  Fmat[1:K, 1:(K * p)] <- do.call(cbind, A)
  # Lower block: shift identity
  if (p > 1) {
    Fmat[(K + 1):(K * p), 1:(K * (p - 1))] <- diag(K * (p - 1))
  }

  J <- cbind(diag(K), matrix(0, nrow = K, ncol = K * (p - 1)))  # selects top K states

  Psi <- array(0, dim = c(K, K, n_ahead + 1))
  Psi[, , 1] <- diag(K)

  Fpow <- diag(K * p)
  for (h in 1:n_ahead) {
    Fpow <- Fpow %*% Fmat
    Psi[, , h + 1] <- J %*% Fpow %*% t(J)
  }
  Psi
}

proxy_svar_irf <- function(mod, df_used, order, z_col = "instrument",
                           shock_var = "fedfunds", n_ahead = 36, scale_to = 1) {

  # Residuals from reduced-form VAR: T x K
  U <- as.matrix(residuals(mod))
  colnames(U) <- order

  # Align z with residual sample: VAR drops first p rows
  p <- mod$p
  z_full <- df_used[[z_col]]
  z <- z_full[(p + 1):length(z_full)]
  if (length(z) != nrow(U)) stop("Instrument length mismatch with VAR residuals (check NA drops / filtering).")

  # Center
  Uc <- scale(U, center = TRUE, scale = FALSE)
  zc <- as.numeric(scale(z, center = TRUE, scale = FALSE))

  # Impact vector b ∝ Cov(U, z) = E[U z]
  b <- as.numeric(t(Uc) %*% zc) / (nrow(Uc) - 1)
  names(b) <- order

  # Normalize so shock_var has impact = +scale_to at horizon 0
  if (!(shock_var %in% names(b))) stop("shock_var not in VAR ordering: ", shock_var)
  if (abs(b[shock_var]) < 1e-12) stop("Instrument is (almost) uncorrelated with the policy residual; relevance may be weak.")

  # force positive impact on policy variable
  if (b[shock_var] < 0) b <- -b

  b_norm <- b * (scale_to / b[shock_var])

  # MA coefficients
  Psi <- var_ma_coefs(mod, n_ahead = n_ahead)   # K x K x (H+1)

  # IRFs: response(h) = Psi_h %*% b_norm
  K <- length(order)
  H <- n_ahead
  irf_mat <- matrix(NA_real_, nrow = H + 1, ncol = K)
  colnames(irf_mat) <- order

  for (h in 0:H) {
    irf_mat[h + 1, ] <- as.numeric(Psi[, , h + 1] %*% b_norm)
  }

  irf_df <- tibble(
    horizon = 0:H
  ) %>%
    bind_cols(as_tibble(irf_mat)) %>%
    pivot_longer(-horizon, names_to = "response", values_to = "coefficient") %>%
    mutate(impulse = paste0("SVAR-IV(", z_col, ")"))

  list(irf = irf_df, b = b_norm)
}

# Optional: very simple relevance check (first-stage F-stat)
instrument_relevance <- function(mod, df_used, z_col = "instrument", policy_var = "fedfunds") {
  U <- as.matrix(residuals(mod))
  p <- mod$p
  z <- df_used[[z_col]][(p + 1):nrow(df_used)]
  # policy residual is the column corresponding to policy_var
  u_pol <- U[, policy_var]

  fit <- lm(u_pol ~ z)
  fs  <- summary(fit)$fstatistic
  list(first_stage = fit, F = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]))
}

# =========================
# 5) Plotting
# =========================
plot_irf_facets_simple <- function(irf_df, title = NULL, subtitle = NULL) {
  ggplot(irf_df, aes(x = horizon, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ response, ncol = 1, scales = "free_y") +
    labs(title = title, subtitle = subtitle, x = "Horizon", y = "Response") +
    theme_minimal()
}

# =========================
# 6) RUN
# =========================

# --- FRED series
series_us <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH")
raw_us <- get_fred_data(series_us, start = "1985-01-01", end = "2007-01-01") %>%
  mutate(date = as.Date(format(date, "%Y-%m-01")))

# --- Read EBP + instrument from Excel (adjust sheet/col names as needed)
# If both are on the same sheet, just call twice with different value_col.
path_xlsx <- "/Users/sophiascarcella/Desktop/Data.xlsx"

ebp <- read_excel_monthly(
  path = path_xlsx,
  sheet = 2,
  date_col  = "observation_date",
  value_col = "ebp",
  value_name = "ebp"
)

instr <- read_excel_monthly(
  path = path_xlsx,
  sheet = 2,
  date_col  = "observation_date",
  value_col = "instrument",
  value_name = "instrument"
)

# --- Merge
raw_us2 <- raw_us %>%
  left_join(ebp, by = "date") %>%
  left_join(instr, by = "date")

# --- Transform
tr <- transform_us_monetary_var(raw_us2)
us <- tr$data
print(tr$ledger)
print(adf_report(us))

# --- Choose ordering (your preferred)
order_us <- c("oil", "exp_infl", "g_indpro", "unrate", "pi_cpi", "ebp", "fedfunds")

# --- IMPORTANT: restrict to overlap where instrument exists (adjust dates if using GK/Caldara series)
us_iv <- us %>%
  filter(date >= as.Date("1990-01-01"), date <= as.Date("2007-01-01")) %>%
  drop_na(instrument)

# --- Fit reduced-form VAR on overlap sample
fit <- fit_var(us_iv, order = order_us, p_fixed = 6, type = "const")
mod <- fit$model

# --- Relevance check
rel <- instrument_relevance(mod, us_iv, z_col = "instrument", policy_var = "fedfunds")
cat("First-stage F-stat (u_fedfunds ~ instrument):", rel$F, " on df(", rel$df1, ",", rel$df2, ")\n")

# --- Proxy SVAR IRFs (normalize to 1 unit increase in fedfunds on impact)
iv_out <- proxy_svar_irf(
  mod = mod,
  df_used = us_iv,
  order = order_us,
  z_col = "instrument",
  shock_var = "fedfunds",
  n_ahead = 36,
  scale_to = 1
)

# --- Plot
p_iv <- plot_irf_facets_simple(
  iv_out$irf,
  title = "IRFs to a monetary policy shock (SVAR-IV) — External Instrument",
  subtitle = paste0(
    "Ordering in VAR: ", paste(order_us, collapse = " → "),
    " | p=", mod$p,
    " | Shock normalized: fedfunds impact = 1",
    " | First-stage F=", round(rel$F, 2)
  )
)
print(p_iv)

# --- (Optional) Print the impact vector b (impact responses at horizon 0)
cat("\nImpact vector (h=0) normalized so fedfunds = 1:\n")
print(round(iv_out$b, 4))

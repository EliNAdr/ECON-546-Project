# ============================================================
# FULL SCRIPT: Reduced-form VAR + Proxy SVAR-IV IRFs
# OUTPUT FORMAT MATCHES "their code":
#   out$irf (df with horizon, shock, response, coefficient, lower, upper)
#   out$irf_chart (ggplot, optional)
#
# NOTES / FIXES INCLUDED:
# - Uses the "new format" get_var_irf style (facet ncol=2, bold strips)
# - Fixes the CPI transformation label mismatch:
#     pi_cpi = 100 * dlog12(CPILFESL)  # YoY
#   and ledger matches that.
# - proxy_svar_irf() returns tidy df in the same column format
# - plot option uses the same style as your new get_var_irf (ncol=2 + bold strips)
# - ggsave example uses a proper filename extension
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

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# =========================
# 1) Helpers
# =========================
dlog   <- function(x) c(NA, diff(log(x)))
diff1  <- function(x) c(NA, diff(x))
dlog12 <- function(x) c(rep(NA, 12), diff(log(x), lag = 12))

get_fred_data <- function(series, start, end) {
  stopifnot(is.character(series), length(series) >= 1)
  raw <- purrr::map_dfr(series, ~ fredr(series_id = .x))
  raw %>%
    dplyr::select(date, series_id, value) %>%
    dplyr::filter(date >= as.Date(start), date <= as.Date(end)) %>%
    tidyr::pivot_wider(names_from = series_id, values_from = value) %>%
    dplyr::arrange(date)
}

read_excel_monthly <- function(path, sheet = 1, date_col, value_col, value_name) {
  xraw <- readxl::read_excel(path, sheet = sheet)
  names(xraw) <- tolower(names(xraw))
  date_col  <- tolower(date_col)
  value_col <- tolower(value_col)

  if (!(date_col %in% names(xraw))) stop("Excel missing date column: ", date_col)
  if (!(value_col %in% names(xraw))) stop("Excel missing value column: ", value_col)

  xraw %>%
    dplyr::transmute(
      date = .data[[date_col]],
      !!value_name := as.numeric(.data[[value_col]])
    ) %>%
    dplyr::mutate(
      date = dplyr::case_when(
        inherits(date, "Date") ~ date,
        is.numeric(date) ~ as.Date(date, origin = "1899-12-30"),
        TRUE ~ as.Date(date)
      ),
      date = as.Date(format(date, "%Y-%m-01"))
    ) %>%
    dplyr::arrange(date)
}

adf_report <- function(df, cols = names(df)[names(df) != "date"]) {
  purrr::map_dfr(cols, function(v) {
    res <- tseries::adf.test(df[[v]])
    tibble::tibble(variable = v, adf_stat = unname(res$statistic), p_value = res$p.value)
  }) %>% dplyr::arrange(p_value)
}

# =========================
# 2) Transform dataset
# =========================
transform_us_monetary_var <- function(df) {
  needed <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH", "ebp", "instrument")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing series in df: ", paste(miss, collapse = ", "))

  out <- df %>%
    dplyr::transmute(
      date       = date,
      oil        = 100 * dlog(WTISPLC),
      exp_infl   = MICH,                    # level, percent
      g_indpro   = 100 * dlog(INDPRO),
      unrate     = UNRATE,                  # level, percent
      pi_cpi     = 100 * dlog12(CPILFESL),   # YoY inflation (%)
      ebp        = ebp,                     # level
      fedfunds   = FEDFUNDS,                # level, percent
      instrument = instrument               # external instrument series (level)
    ) %>%
    tidyr::drop_na()

  ledger <- tibble::tribble(
    ~variable,    ~source,     ~transform,                    ~units,
    "oil",        "WTISPLC",   "100*Δlog(WTISPLC)",           "% m/m",
    "exp_infl",   "MICH",      "level",                       "% (level)",
    "g_indpro",   "INDPRO",    "100*Δlog(INDPRO)",            "% m/m",
    "unrate",     "UNRATE",    "level",                       "% (level)",
    "pi_cpi",     "CPILFESL",  "100*Δlog(CPILFESL) lag=12",    "% y/y",
    "ebp",        "Excel",     "level",                       "level",
    "fedfunds",   "FEDFUNDS",  "level",                       "% (level)",
    "instrument", "Excel",     "level",                       "instrument units"
  )

  list(data = out, ledger = ledger)
}

# =========================
# 3) Fit reduced-form VAR
# =========================
fit_var <- function(df, order, p_fixed = 6, type = "const") {
  if (!is.numeric(p_fixed) || length(p_fixed) != 1 || is.na(p_fixed) || p_fixed < 1) {
    stop("p_fixed must be a single integer >= 1")
  }
  Y <- df %>% dplyr::select(dplyr::all_of(order))
  mod <- vars::VAR(Y, p = as.integer(p_fixed), type = type)
  list(model = mod, p = as.integer(p_fixed), ordering = order)
}

# =========================
# 4) (Reduced-form) VAR IRF wrapper (NEW FORMAT)
# =========================
get_var_irf <- function(mod, shock, resp = NULL, ortho = FALSE, horizon, plot = FALSE) {
  #' Wrapper function to get impulse responses from a VAR model
  #'
  #' Returns:
  #' - list(irf = <df>, irf_chart = <ggplot optional>)
  #' DF columns:
  #' horizon, shock, response, coefficient, lower, upper

  if (!inherits(mod, "varest")) stop("model provided must be of class 'varest'")
  if (missing(horizon) || length(horizon) != 1 || horizon < 1) {
    stop("horizon must be a single integer >= 1")
  }

  irf_out <- vars::irf(
    mod,
    impulse  = shock,
    response = resp,
    ortho    = ortho,
    n.ahead  = horizon
  )

  irf_to_df <- function(irf_obj) {
    purrr::map_df(names(irf_obj$irf), function(shk) {
      irfs  <- irf_obj$irf[[shk]]
      lower <- irf_obj$Lower[[shk]]
      upper <- irf_obj$Upper[[shk]]

      purrr::map_df(seq_len(ncol(irfs)), function(j) {
        tibble::tibble(
          horizon     = 0:(nrow(irfs) - 1),
          shock       = shk,
          response    = colnames(irfs)[j],
          coefficient = as.numeric(irfs[, j]),
          lower       = as.numeric(lower[, j]),
          upper       = as.numeric(upper[, j])
        )
      })
    })
  }

  out <- list(irf = irf_to_df(irf_out))

  if (isTRUE(plot)) {
    chart <- ggplot2::ggplot(out$irf, ggplot2::aes(x = horizon)) +
      ggplot2::geom_hline(yintercept = 0.0, linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = coefficient), linewidth = 1.2, color = "blue") +
      ggplot2::geom_line(ggplot2::aes(y = lower), linewidth = 1.0, linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = upper), linewidth = 1.0, linetype = "dashed") +
      ggplot2::facet_wrap(~ response, ncol = 2, scales = "free_y") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = "bold"))

    out$irf_chart <- chart
  }

  out
}

# =========================
# 5) VAR MA coefficients (Psi_h)
# =========================
var_ma_coefs <- function(mod, n_ahead) {
  A <- vars::Acoef(mod)          # list length p, each KxK
  K <- ncol(mod$y)
  p <- mod$p

  Fmat <- matrix(0, nrow = K * p, ncol = K * p)
  Fmat[1:K, 1:(K * p)] <- do.call(cbind, A)
  if (p > 1) {
    Fmat[(K + 1):(K * p), 1:(K * (p - 1))] <- diag(K * (p - 1))
  }

  J <- cbind(diag(K), matrix(0, nrow = K, ncol = K * (p - 1)))

  Psi <- array(0, dim = c(K, K, n_ahead + 1))
  Psi[, , 1] <- diag(K)

  Fpow <- diag(K * p)
  for (h in 1:n_ahead) {
    Fpow <- Fpow %*% Fmat
    Psi[, , h + 1] <- J %*% Fpow %*% t(J)
  }
  Psi
}

# =========================
# 6) Proxy SVAR-IV: point IRF
# =========================
proxy_svar_irf_point <- function(mod, df_used, order, z_col = "instrument",
                                 shock_var = "fedfunds", n_ahead = 36, scale_to = 1) {

  U <- as.matrix(residuals(mod))
  colnames(U) <- order

  p <- mod$p
  z_full <- df_used[[z_col]]
  z <- z_full[(p + 1):length(z_full)]
  if (length(z) != nrow(U)) stop("Instrument length mismatch with VAR residuals (check NA drops / filtering).")

  Uc <- scale(U, center = TRUE, scale = FALSE)
  zc <- as.numeric(scale(z, center = TRUE, scale = FALSE))

  b <- as.numeric(t(Uc) %*% zc) / (nrow(Uc) - 1)
  names(b) <- order

  if (!(shock_var %in% names(b))) stop("shock_var not in VAR ordering: ", shock_var)
  if (abs(b[shock_var]) < 1e-12) stop("Instrument is (almost) uncorrelated with policy residual; relevance may be weak.")

  if (b[shock_var] < 0) b <- -b
  b_norm <- b * (scale_to / b[shock_var])

  Psi <- var_ma_coefs(mod, n_ahead = n_ahead)

  K <- length(order)
  H <- n_ahead
  irf_mat <- matrix(NA_real_, nrow = H + 1, ncol = K)
  colnames(irf_mat) <- order

  for (h in 0:H) {
    irf_mat[h + 1, ] <- as.numeric(Psi[, , h + 1] %*% b_norm)
  }

  list(irf_mat = irf_mat, b = b_norm)
}

# =========================
# 7) Residual bootstrap for SVAR-IV IRFs (percentile bands)
# =========================
simulate_var_from_residuals <- function(mod, U_boot, Y_init) {
  A <- vars::Acoef(mod)       # list A1..Ap, each KxK
  cst <- vars::Bcoef(mod)     # coefficients matrix (includes const)
  K <- ncol(mod$y)
  p <- mod$p

  intercept <- rep(0, K)
  if ("const" %in% rownames(cst)) {
    intercept <- as.numeric(cst["const", ])
  }

  Tn <- nrow(U_boot)
  Y <- matrix(NA_real_, nrow = Tn + p, ncol = K)
  colnames(Y) <- colnames(mod$y)

  Y[1:p, ] <- Y_init

  for (t in (p + 1):(Tn + p)) {
    yt <- intercept
    for (j in 1:p) {
      yt <- yt + A[[j]] %*% Y[t - j, ]
    }
    yt <- yt + U_boot[t - p, ]
    Y[t, ] <- as.numeric(yt)
  }

  Y[(p + 1):(Tn + p), , drop = FALSE]
}

proxy_svar_irf <- function(mod, df_used, order, z_col = "instrument",
                           shock_var = "fedfunds",
                           resp = NULL,
                           n_ahead = 36,
                           scale_to = 1,
                           boot = TRUE,
                           nboot = 300,
                           ci = 0.90,
                           plot = FALSE) {

  # ---- point estimate
  pt <- proxy_svar_irf_point(
    mod = mod, df_used = df_used, order = order,
    z_col = z_col, shock_var = shock_var,
    n_ahead = n_ahead, scale_to = scale_to
  )

  irf_mat <- pt$irf_mat
  H <- n_ahead

  # Which responses to keep
  if (is.null(resp)) {
    resp_keep <- order
  } else {
    if (!all(resp %in% order)) stop("resp contains names not in VAR ordering: ", paste(setdiff(resp, order), collapse = ", "))
    resp_keep <- resp
  }
  keep_idx <- match(resp_keep, order)

  # Base df (tidy)
  irf_df <- tibble::tibble(horizon = 0:H) %>%
    dplyr::bind_cols(tibble::as_tibble(irf_mat[, keep_idx, drop = FALSE])) %>%
    tidyr::pivot_longer(-horizon, names_to = "response", values_to = "coefficient") %>%
    dplyr::mutate(
      shock = paste0("SVAR-IV(", z_col, ")"),
      lower = NA_real_,
      upper = NA_real_
    ) %>%
    dplyr::select(horizon, shock, response, coefficient, lower, upper)

  # ---- bootstrap bands
  if (isTRUE(boot)) {
    U <- as.matrix(residuals(mod))
    colnames(U) <- order

    p <- mod$p
    z_full <- df_used[[z_col]]
    z <- z_full[(p + 1):length(z_full)]
    if (length(z) != nrow(U)) stop("Instrument length mismatch with VAR residuals.")

    Y_init <- as.matrix(mod$y[1:p, , drop = FALSE])

    K <- length(order)
    boots <- array(NA_real_, dim = c(H + 1, K, nboot))

    set.seed(123)
    for (b in 1:nboot) {
      idx <- sample.int(nrow(U), size = nrow(U), replace = TRUE)
      U_b <- U[idx, , drop = FALSE]

      Y_sim <- simulate_var_from_residuals(mod, U_boot = U_b, Y_init = Y_init)
      Y_sim <- as.data.frame(Y_sim)
      names(Y_sim) <- order

      df_sim <- df_used
      df_sim <- df_sim[(p + 1):nrow(df_sim), , drop = FALSE]
      df_sim <- df_sim %>% dplyr::mutate(!!!setNames(as.list(Y_sim), order))

      mod_b <- vars::VAR(df_sim %>% dplyr::select(dplyr::all_of(order)), p = p, type = "const")

      pt_b <- proxy_svar_irf_point(
        mod = mod_b, df_used = df_sim, order = order,
        z_col = z_col, shock_var = shock_var,
        n_ahead = H, scale_to = scale_to
      )

      boots[, , b] <- pt_b$irf_mat
    }

    alpha <- (1 - ci) / 2
    lo_q <- alpha
    hi_q <- 1 - alpha

    lo <- apply(boots[, keep_idx, , drop = FALSE], c(1, 2), quantile, probs = lo_q, na.rm = TRUE)
    hi <- apply(boots[, keep_idx, , drop = FALSE], c(1, 2), quantile, probs = hi_q, na.rm = TRUE)

    # Long bands df keyed by horizon/response
    bands_df <- tibble::tibble(horizon = 0:H) %>%
      dplyr::bind_cols(tibble::as_tibble(lo)) %>%
      dplyr::rename_with(~ paste0(.x, "_lo"), -horizon) %>%
      dplyr::left_join(
        tibble::tibble(horizon = 0:H) %>%
          dplyr::bind_cols(tibble::as_tibble(hi)) %>%
          dplyr::rename_with(~ paste0(.x, "_hi"), -horizon),
        by = "horizon"
      ) %>%
      tidyr::pivot_longer(-horizon, names_to = "name", values_to = "value") %>%
      dplyr::mutate(
        response = sub("_(lo|hi)$", "", name),
        band = sub("^.*_(lo|hi)$", "\\1", name)
      ) %>%
      dplyr::select(horizon, response, band, value) %>%
      tidyr::pivot_wider(names_from = band, values_from = value) %>%
      dplyr::rename(lower = lo, upper = hi)

    irf_df <- irf_df %>%
      dplyr::left_join(bands_df, by = c("horizon", "response"), suffix = c("", ".tmp")) %>%
      dplyr::mutate(
        lower = dplyr::coalesce(lower.tmp, lower),
        upper = dplyr::coalesce(upper.tmp, upper)
      ) %>%
      dplyr::select(horizon, shock, response, coefficient, lower, upper)
  }

  out <- list(irf = irf_df)

  if (isTRUE(plot)) {
    chart <- ggplot2::ggplot(out$irf, ggplot2::aes(x = horizon)) +
      ggplot2::geom_hline(yintercept = 0.0, linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = coefficient), linewidth = 1.2, color = "blue") +
      ggplot2::geom_line(ggplot2::aes(y = lower), linewidth = 1.0, linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = upper), linewidth = 1.0, linetype = "dashed") +
      ggplot2::facet_wrap(~ response, ncol = 2, scales = "free_y") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = "bold"))

    out$irf_chart <- chart
  }

  out
}

# =========================
# 8) Optional: relevance check (first-stage F-stat)
# =========================
instrument_relevance <- function(mod, df_used, z_col = "instrument", policy_var = "fedfunds") {
  U <- as.matrix(residuals(mod))
  p <- mod$p
  z <- df_used[[z_col]][(p + 1):nrow(df_used)]
  u_pol <- U[, policy_var]

  fit <- lm(u_pol ~ z)
  fs  <- summary(fit)$fstatistic
  list(first_stage = fit, F = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]))
}

# ============================================================
# 9) RUN
# ============================================================

# --- FRED series
series_us <- c("INDPRO", "UNRATE", "CPILFESL", "FEDFUNDS", "WTISPLC", "MICH")
raw_us <- get_fred_data(series_us, start = "1985-01-01", end = "2007-01-01") %>%
  dplyr::mutate(date = as.Date(format(date, "%Y-%m-01")))

# --- Read EBP + instrument from Excel (adjust as needed)
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
  dplyr::left_join(ebp, by = "date") %>%
  dplyr::left_join(instr, by = "date")

# --- Transform
tr <- transform_us_monetary_var(raw_us2)
us <- tr$data
print(tr$ledger)
print(adf_report(us))

# --- Ordering
order_us <- c("oil", "exp_infl", "g_indpro", "unrate", "pi_cpi", "ebp", "fedfunds")

# --- Restrict to overlap where instrument exists (edit dates as needed)
us_iv <- us %>%
  dplyr::filter(date >= as.Date("1990-01-01"), date <= as.Date("2007-01-01")) %>%
  tidyr::drop_na(instrument)

# --- Fit reduced-form VAR
fit <- fit_var(us_iv, order = order_us, p_fixed = 6, type = "const")
mod <- fit$model

cat("Lag method: fixed p=", fit$p, " | p = ", fit$p, "\n", sep = "")
cat("Ordering: ", paste(fit$ordering, collapse = " → "), "\n", sep = "")

# --- Relevance check
rel <- instrument_relevance(mod, us_iv, z_col = "instrument", policy_var = "fedfunds")
cat("First-stage F-stat (u_fedfunds ~ instrument):", rel$F, " on df(", rel$df1, ",", rel$df2, ")\n")

# --- Proxy SVAR-IV IRFs (OUTPUT FORMAT MATCHES get_var_irf)
iv_out <- proxy_svar_irf(
  mod       = mod,
  df_used   = us_iv,
  order     = order_us,
  z_col     = "instrument",
  shock_var = "fedfunds",
  resp      = c("ebp", "exp_infl", "fedfunds", "g_indpro", "oil", "pi_cpi", "unrate"),
  n_ahead   = 36,
  scale_to  = 1,
  boot      = TRUE,
  nboot     = 300,
  ci        = 0.90,
  plot      = TRUE
)

# --- Pretty labels (optional)
label_map <- c(
  oil        = "Oil Price",
  exp_infl   = "Inflation Expectations",
  g_indpro   = "Industrial Production Growth",
  unrate     = "Unemployment Rate",
  pi_cpi     = "CPI Inflation (YoY)",
  ebp        = "Excess Bond Premium",
  fedfunds   = "Policy Rate"
)

iv_out$irf <- iv_out$irf %>%
  dplyr::mutate(
    response = as.character(response),
    response = dplyr::recode(response, !!!label_map)
  )

# Rebuild chart after relabel (keeps same "new format" style)
iv_out$irf_chart <- ggplot2::ggplot(iv_out$irf, ggplot2::aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0.0, linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coefficient), linewidth = 1.2, color = "blue") +
  ggplot2::geom_line(ggplot2::aes(y = lower), linewidth = 1.0, linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = upper), linewidth = 1.0, linetype = "dashed") +
  ggplot2::facet_wrap(~ response, ncol = 2, scales = "free_y") +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = "bold")) +
  ggplot2::labs(x = "Horizon", y = "Response")

print(iv_out$irf_chart)

# --- Save plot (make sure figures/ exists, and include a file extension!)
if (!dir.exists("figures")) dir.create("figures")


ggsave("svar_iv.png", iv_out$irf_chart, width = 12, height = 8, dpi = 300)
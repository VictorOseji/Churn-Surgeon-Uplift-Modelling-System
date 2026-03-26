# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — utils.R
#  Logging, math primitives, cost-benefit framework, helper functions
# ══════════════════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(cli); library(glue); library(dplyr); library(tibble)
  library(purrr); library(tidyr); library(stringr); library(Matrix)
})

# ── Logging ───────────────────────────────────────────────────────────────────
cs_log <- function(level = c("INFO","WARN","ERROR","DEBUG"), msg, agent = "CS") {
  level <- match.arg(level)
  ts    <- format(Sys.time(), "%H:%M:%S")
  col   <- switch(level,
    INFO  = cli::col_green,  WARN  = cli::col_yellow,
    ERROR = cli::col_red,    DEBUG = cli::col_cyan)
  cli::cat_line(glue("[{ts}] [{col(level)}] [{agent}] {msg}"))
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
clamp  <- function(x, lo = 0, hi = 1) pmax(lo, pmin(hi, x))
f_gbp  <- function(x, d = 0) paste0("£", formatC(round(x, d), format = "d", big.mark = ","))
f_pct  <- function(x, d = 1) paste0(round(x * 100, d), "%")
slog   <- function(x, eps = 1e-9) log(pmax(x, eps))

# ── Sigmoid / logistic ────────────────────────────────────────────────────────
sigmoid  <- function(x) 1 / (1 + exp(-clamp(x, -30, 30)))
logit    <- function(p) log(clamp(p, 1e-9, 1-1e-9) / (1 - clamp(p, 1e-9, 1-1e-9)))

# ── Log-sum-exp ───────────────────────────────────────────────────────────────
lse <- function(a, b) { m <- pmax(a, b); m + log(exp(a-m) + exp(b-m)) }

# ── Variance / covariance helpers ─────────────────────────────────────────────
wtd_mean <- function(x, w) sum(w * x, na.rm=TRUE) / sum(w, na.rm=TRUE)
wtd_var  <- function(x, w) {
  mu <- wtd_mean(x, w)
  sum(w * (x - mu)^2, na.rm=TRUE) / (sum(w, na.rm=TRUE) - 1)
}

# ── Rolling / window helpers ──────────────────────────────────────────────────
rolling_mean <- function(x, k = 10) {
  vapply(seq_along(x), function(i) mean(x[max(1,i-k+1):i], na.rm=TRUE), numeric(1))
}

# ── Normalise a vector to [0,1] ───────────────────────────────────────────────
norm01 <- function(x) {
  rng <- range(x, na.rm=TRUE); if (diff(rng) == 0) return(rep(.5, length(x)))
  (x - rng[1]) / diff(rng)
}

# ── Cost-benefit primitives ───────────────────────────────────────────────────

# Net value of targeting a customer:
#   NV_i = τ_i × CLV_i − discount_cost_i  (if τ_i > 0 and NV_i > 0: worthwhile)
# where τ_i = ITE = P(churn | control) - P(churn | treated)
net_value <- function(tau, clv, cost) {
  tau * clv - cost
}

# Campaign ROI
campaign_roi <- function(total_saved_value, total_cost) {
  (total_saved_value - total_cost) / max(total_cost, 1)
}

# Targeting efficiency vs universal targeting
targeting_lift <- function(targeted_roi, universal_roi) {
  targeted_roi / max(universal_roi, 0.001) - 1
}

# ── Four treatment response groups ────────────────────────────────────────────
# Based on (potential outcomes Y(0), Y(1)) ≡ (churn_control, churn_treated)
# Persuadables:  Y(0)=1, Y(1)=0  → treatment prevents churn  τ > 0 ✓ TARGET
# Sure Things:   Y(0)=0, Y(1)=0  → would stay anyway          τ = 0 ✗ WASTE
# Lost Causes:   Y(0)=1, Y(1)=1  → leave regardless            τ = 0 ✗ FUTILE
# Sleeping Dogs: Y(0)=0, Y(1)=1  → treatment makes it worse   τ < 0 ✗ HARM

classify_response_group <- function(y0, y1) {
  case_when(
    y0 == 1 & y1 == 0 ~ "Persuadable",
    y0 == 0 & y1 == 0 ~ "Sure Thing",
    y0 == 1 & y1 == 1 ~ "Lost Cause",
    y0 == 0 & y1 == 1 ~ "Sleeping Dog",
    TRUE              ~ "Unknown"
  )
}

# ── ATE / ATT / ATC estimators ────────────────────────────────────────────────
ate <- function(tau) mean(tau, na.rm=TRUE)
att <- function(tau, treated) mean(tau[treated == 1], na.rm=TRUE)
atc <- function(tau, treated) mean(tau[treated == 0], na.rm=TRUE)

# ── Hypothesis test for treatment effect ─────────────────────────────────────
test_ate <- function(y, w, method = "ttest") {
  y1 <- y[w == 1]; y0 <- y[w == 0]
  t.test(y1, y0, alternative = "less")  # H1: treated churn less
}

# ── Variance stabilising transform for propensity scores ─────────────────────
overlap_trim <- function(ps, lo = 0.05, hi = 0.95) {
  ps >= lo & ps <= hi
}

# ── Bootstrap CI helper ───────────────────────────────────────────────────────
bootstrap_ci <- function(x, fn = mean, B = 500, alpha = 0.05, seed = 42) {
  set.seed(seed)
  bs <- replicate(B, fn(sample(x, length(x), replace=TRUE)))
  quantile(bs, c(alpha/2, 1-alpha/2))
}

# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — evaluation.R
#  Uplift model evaluation suite:
#
#  1. QINI CURVE (Radcliffe & Surry 2011)
#     Plots incremental conversions vs customers targeted
#     Q(t) = [n_T(t)/N_T - n_C(t)/N_C] × N_T
#
#  2. AUUC (Area Under Uplift Curve)
#     Summary statistic for overall uplift model quality
#     Random baseline AUUC = 0; perfect model = maximum possible
#
#  3. CUMULATIVE GAIN CHART
#     Plots cumulative saved customers vs targeting threshold
#
#  4. RATE (Rank-Weighted Average Treatment Effect) — Yadlowsky et al. 2021
#     A proper scoring rule for CATE ordering quality
#
#  5. UPLIFT DECILE ANALYSIS
#     ATE by predicted-τ decile — should be monotonically increasing
#
#  6. CALIBRATION
#     Predicted τ vs observed ATE within decile bins
# ══════════════════════════════════════════════════════════════════════════════
library(tibble); library(dplyr); library(purrr)

# ── 1. Qini Curve ─────────────────────────────────────────────────────────────
# Returns tibble with columns: pct_targeted, qini, qini_random, qini_perfect
compute_qini <- function(tau_hat, Y, W, n_points=100) {
  n   <- length(tau_hat); N_T <- sum(W==1); N_C <- sum(W==0)
  ord <- order(tau_hat, decreasing=TRUE)

  # Sort by predicted tau
  Y_s <- Y[ord]; W_s <- W[ord]

  thresholds <- round(seq(1, n, length.out=n_points))

  tibble(threshold=thresholds) |>
    mutate(
      pct_targeted = threshold / n,
      q = map_dbl(threshold, function(t) {
        top    <- seq_len(t)
        n_T_t  <- sum(W_s[top] == 1)
        n_C_t  <- sum(W_s[top] == 0)
        y_T_t  <- if (n_T_t>0) sum(Y_s[top][W_s[top]==1]) else 0
        y_C_t  <- if (n_C_t>0) sum(Y_s[top][W_s[top]==0]) else 0
        # Qini value: incremental outcome in treated vs proportional control
        y_T_t - y_C_t * (n_T_t / max(N_C,1))
      }),
      qini        = q,
      qini_random = (pct_targeted * (sum(Y[W==1]) - sum(Y[W==0]) * N_T/N_C))
    ) |>
    select(pct_targeted, qini, qini_random)
}

# ── 2. AUUC ───────────────────────────────────────────────────────────────────
compute_auuc <- function(tau_hat, Y, W, n_points=100) {
  qini <- compute_qini(tau_hat, Y, W, n_points)
  # Trapezoid rule
  auuc   <- sum(diff(qini$pct_targeted) * (head(qini$qini,-1)+tail(qini$qini,-1))/2)
  auuc_r <- sum(diff(qini$pct_targeted) * (head(qini$qini_random,-1)+tail(qini$qini_random,-1))/2)
  list(auuc=auuc, auuc_random=auuc_r, qini_lift=auuc-auuc_r)
}

# ── 3. Cumulative Gain ────────────────────────────────────────────────────────
compute_cumulative_gain <- function(tau_hat, Y, W, n_points=100) {
  n     <- length(tau_hat)
  ord   <- order(tau_hat, decreasing=TRUE)
  Y_s   <- Y[ord]; W_s <- W[ord]
  N_T   <- sum(W==1); N_C <- sum(W==0)
  thresholds <- round(seq(1, n, length.out=n_points))

  tibble(threshold=thresholds) |>
    mutate(
      pct_targeted = threshold / n,
      gain = map_dbl(threshold, function(t) {
        top  <- seq_len(t)
        y_T  <- if (sum(W_s[top]==1)>0) mean(Y_s[top][W_s[top]==1]) else 0
        y_C  <- if (sum(W_s[top]==0)>0) mean(Y_s[top][W_s[top]==0]) else 0
        y_T - y_C
      }),
      gain_random = map_dbl(threshold, function(t) {
        mean(Y[W==1]) - mean(Y[W==0])
      })
    )
}

# ── 4. RATE (Rank-Weighted ATE) ───────────────────────────────────────────────
# RATE(f) = (1/n) Σ_i w_i(rank(τ̂_i)) × (2W_i-1) × Y_i / e_i(1-e_i)
# where w_i is a rank weight (Yadlowsky et al. 2021)
compute_rate <- function(tau_hat, Y, W, ps, type="AUTOC") {
  n    <- length(tau_hat)
  rank_tau <- rank(tau_hat, ties.method="average") / n  # normalised rank in [0,1]

  # Doubly-robust score
  dr_score <- (2*W-1)*Y / (W*ps + (1-W)*(1-ps) + 1e-9)

  if (type == "AUTOC") {
    # AUTOC: area under TOC curve, weight = I(rank > t) for each t
    sorted_rank <- sort(rank_tau, decreasing=TRUE)
    aucs <- cumsum(dr_score[order(tau_hat, decreasing=TRUE)]) / seq_len(n)
    mean(aucs)
  } else {
    # QINI-style RATE
    weights <- rank_tau  # higher predicted tau → higher weight
    sum(weights * dr_score) / sum(weights + 1e-9)
  }
}

# ── 5. Uplift Decile Analysis ─────────────────────────────────────────────────
uplift_by_decile <- function(tau_hat, Y, W, n_deciles=10) {
  df <- tibble(tau_hat=tau_hat, Y=Y, W=W) |>
    mutate(decile=ntile(tau_hat, n_deciles))

  df |> group_by(decile) |>
    summarise(
      n              = n(),
      n_treated      = sum(W==1),
      n_control      = sum(W==0),
      mean_tau_hat   = mean(tau_hat, na.rm=TRUE),
      ate_observed   = if (sum(W==1)>0 && sum(W==0)>0)
                         mean(Y[W==1]) - mean(Y[W==0]) else NA_real_,
      y_treated      = if (sum(W==1)>0) mean(Y[W==1]) else NA_real_,
      y_control      = if (sum(W==0)>0) mean(Y[W==0]) else NA_real_,
      se_ate         = if (sum(W==1)>1 && sum(W==0)>1) {
        se1 <- sd(Y[W==1])/sqrt(sum(W==1)); se0 <- sd(Y[W==0])/sqrt(sum(W==0))
        sqrt(se1^2+se0^2)
      } else NA_real_,
      .groups="drop"
    ) |>
    mutate(
      ci_lo = ate_observed - 1.96*se_ate,
      ci_hi = ate_observed + 1.96*se_ate,
      monotone_ok = if_else(decile == 1, TRUE,
                     ate_observed >= lag(ate_observed, default=ate_observed[1]) - .02)
    )
}

# ── 6. Calibration ────────────────────────────────────────────────────────────
uplift_calibration <- function(tau_hat, true_tau, n_bins=10) {
  tibble(tau_hat=tau_hat, true_tau=true_tau) |>
    mutate(bin=ntile(tau_hat, n_bins)) |>
    group_by(bin) |>
    summarise(
      mean_predicted = mean(tau_hat),
      mean_true      = mean(true_tau),
      n              = n(),
      .groups        = "drop"
    ) |>
    mutate(calibration_error = mean_predicted - mean_true)
}

# ── 7. Model comparison table ─────────────────────────────────────────────────
compare_models <- function(model_preds_list, Y, W, ps,
                            true_tau=NULL, n_points=100) {
  map_dfr(names(model_preds_list), function(nm) {
    tau <- model_preds_list[[nm]]
    au  <- compute_auuc(tau, Y, W, n_points)
    rt  <- tryCatch(compute_rate(tau, Y, W, ps), error=function(e) NA_real_)
    dec <- uplift_by_decile(tau, Y, W)
    mono<- mean(dec$monotone_ok, na.rm=TRUE)
    corr_true <- if (!is.null(true_tau)) cor(tau, true_tau, use="pairwise.complete.obs") else NA_real_

    tibble(
      model        = nm,
      auuc         = au$auuc,
      qini_lift    = au$qini_lift,
      rate         = rt,
      monotonicity = mono,
      ate_d10      = dec$ate_observed[dec$decile==10],
      ate_d1       = dec$ate_observed[dec$decile==1],
      lift_ratio   = dec$ate_observed[dec$decile==10] / (dec$ate_observed[dec$decile==1]+.001),
      corr_true_tau= corr_true
    )
  }) |> arrange(desc(qini_lift))
}

# ── 8. Policy uplift value ────────────────────────────────────────────────────
# Given a budget and CLV values, compute the optimal targeting policy value
policy_value <- function(tau_hat, clv, cost, budget=NULL) {
  # Net value of targeting each customer
  nv <- net_value(tau_hat, clv, cost)

  # Sort by net value descending
  ord <- order(nv, decreasing=TRUE)
  nv_sorted   <- nv[ord]
  cost_sorted <- cost[ord]
  cum_cost    <- cumsum(cost_sorted)
  cum_nv      <- cumsum(nv_sorted)
  pct_targeted<- seq_len(length(tau_hat)) / length(tau_hat)

  if (!is.null(budget)) {
    eligible    <- cum_cost <= budget
    n_target    <- max(1, sum(eligible))
    total_nv    <- sum(nv_sorted[seq_len(n_target)])
    total_cost  <- sum(cost_sorted[seq_len(n_target)])
    roi         <- campaign_roi(total_nv, total_cost)
    return(list(n_targeted=n_target, total_nv=total_nv,
                total_cost=total_cost, roi=roi, policy_ids=ord[seq_len(n_target)]))
  }

  # Full curve
  tibble(pct_targeted=pct_targeted, cum_nv=cum_nv, cum_cost=cum_cost,
         roi=cum_nv/(cum_cost+.01))
}

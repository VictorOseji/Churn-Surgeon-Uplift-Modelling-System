# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — action_engine.R
#  Automated intervention system:
#  • Classifies customers into four response groups
#  • Selects Persuadables only (positive ITE + positive NV)
#  • Budget-constrained ranking by ROI
#  • Generates campaign briefs per segment
# ══════════════════════════════════════════════════════════════════════════════
library(R6); library(tibble); library(dplyr); library(purrr); library(glue)

# ── Offer catalogue ────────────────────────────────────────────────────────────
OFFER_CATALOGUE <- tibble(
  offer_id    = c("OFF001","OFF002","OFF003","OFF004","OFF005","OFF006"),
  offer_name  = c("10% Loyalty Discount","2 Months Free","Feature Upgrade",
                   "Personal Success Manager","Winback 20% Off","Early Adopter Bundle"),
  channel     = c("Email","Email + App","In-App","Phone","Email + SMS","App"),
  base_cost   = c(8.50, 45.00, 0.50, 35.00, 22.00, 15.00),   # per customer
  monthly_val_saved_est = c(24, 48, 12, 60, 36, 28),           # expected MRR saved
  min_clv     = c(100, 200, 50, 400, 150, 100),
  max_discount_frac = c(.10, .167, 0, .25, .20, .12),
  tau_min     = c(.05, .10, .03, .15, .12, .06),              # min uplift to trigger
  priority    = c(2, 1, 3, 1, 2, 3)
)

# ── Classify customers into 4 uplift response groups ─────────────────────────
classify_response <- function(tau_hat, base_churn_prob, threshold_positive=0.05,
                               threshold_negative=-0.03) {
  case_when(
    tau_hat >= threshold_positive & base_churn_prob > 0.20 ~ "Persuadable",
    tau_hat >= threshold_positive & base_churn_prob <= 0.20~ "Sure Thing",
    tau_hat <= threshold_negative                           ~ "Sleeping Dog",
    base_churn_prob > 0.50                                 ~ "Lost Cause",
    TRUE                                                   ~ "Marginal"
  )
}

# ── Recommend best offer for a customer ──────────────────────────────────────
recommend_offer <- function(tau_hat, clv, segment, base_churn_prob) {
  eligible <- OFFER_CATALOGUE |>
    filter(tau_min <= tau_hat, min_clv <= clv) |>
    mutate(
      net_value     = tau_hat * clv - base_cost,
      roi           = net_value / (base_cost + .01),
      churn_match   = case_when(
        base_churn_prob > .50 ~ priority <= 2,
        base_churn_prob > .25 ~ priority <= 3,
        TRUE ~ priority == 3)
    ) |>
    arrange(desc(roi)) |>
    slice_head(n=1)

  if (!nrow(eligible)) return(NULL)
  eligible
}

# ── Batch action engine ───────────────────────────────────────────────────────
run_action_engine <- function(forecast_df, budget_gbp=NULL,
                               tau_threshold=0.05, clv_col="clv_estimate",
                               cost_col="offer_cost") {
  d <- forecast_df |>
    mutate(
      nv           = net_value(tau_ensemble, .data[[clv_col]], .data[[cost_col]]),
      response_grp = classify_response(tau_ensemble, base_churn_prob),
      eligible     = response_grp == "Persuadable" & tau_ensemble >= tau_threshold & nv > 0,
      roi          = nv / ((.data[[cost_col]]) + .01)
    ) |>
    arrange(desc(roi))

  # Add recommended offer
  d <- d |>
    mutate(
      offer_id   = map_chr(seq_len(nrow(d)), function(i) {
        if (!d$eligible[i]) return("NONE")
        off <- recommend_offer(d$tau_ensemble[i], d[[clv_col]][i],
                               d$segment[i] %||% "unknown",
                               d$base_churn_prob[i])
        if (is.null(off)) "NONE" else off$offer_id
      }),
      offer_name = map_chr(offer_id, function(id) {
        if (id == "NONE") return("No action")
        OFFER_CATALOGUE$offer_name[OFFER_CATALOGUE$offer_id == id][1] %||% "Unknown"
      })
    )

  # Budget constraint
  if (!is.null(budget_gbp)) {
    d <- d |>
      mutate(
        cum_cost     = cumsum(if_else(eligible, .data[[cost_col]], 0)),
        within_budget= cum_cost <= budget_gbp & eligible
      )
  }

  d
}

# ── Campaign summary ──────────────────────────────────────────────────────────
campaign_summary <- function(action_df, clv_col="clv_estimate", cost_col="offer_cost") {
  targeted <- filter(action_df, eligible)

  by_group <- action_df |>
    group_by(response_grp) |>
    summarise(
      n          = n(),
      mean_tau   = mean(tau_ensemble, na.rm=TRUE),
      mean_clv   = mean(.data[[clv_col]], na.rm=TRUE),
      n_targeted = sum(eligible, na.rm=TRUE),
      .groups    = "drop"
    )

  list(
    n_total          = nrow(action_df),
    n_targeted       = nrow(targeted),
    n_persuadables   = sum(action_df$response_grp=="Persuadable"),
    n_sure_things    = sum(action_df$response_grp=="Sure Thing"),
    n_lost_causes    = sum(action_df$response_grp=="Lost Cause"),
    n_sleeping_dogs  = sum(action_df$response_grp=="Sleeping Dog"),
    total_cost       = sum(targeted[[cost_col]], na.rm=TRUE),
    total_nv         = sum(targeted$nv, na.rm=TRUE),
    mean_tau_targeted= mean(targeted$tau_ensemble, na.rm=TRUE),
    mean_tau_all     = mean(action_df$tau_ensemble, na.rm=TRUE),
    roi              = campaign_roi(sum(targeted$nv,na.rm=TRUE),
                                    sum(targeted[[cost_col]],na.rm=TRUE)),
    by_group         = by_group
  )
}

# ── Threshold sensitivity: find optimal tau threshold ─────────────────────────
optimise_threshold <- function(action_df, tau_grid=seq(.01,.35,.01),
                                clv_col="clv_estimate", cost_col="offer_cost") {
  map_dfr(tau_grid, function(th) {
    targeted <- filter(action_df, tau_ensemble >= th, response_grp=="Persuadable")
    if (!nrow(targeted)) return(tibble(threshold=th,n=0,nv=0,roi=0))
    nv   <- sum(targeted$nv, na.rm=TRUE)
    cost <- sum(targeted[[cost_col]], na.rm=TRUE)
    tibble(threshold=th, n=nrow(targeted), nv=nv, cost=cost,
           roi=campaign_roi(nv, cost))
  })
}

# ── ActionEngine R6 ───────────────────────────────────────────────────────────
ActionEngine <- R6::R6Class("ActionEngine",
  private = list(df_=NULL, summary_=NULL, sensitivity_=NULL),
  public  = list(

    initialize = function() cs_log("INFO","ActionEngine ready.","AE"),

    run = function(forecast_df, budget_gbp=NULL, tau_threshold=0.05) {
      cs_log("INFO",
        glue("Running action engine: {nrow(forecast_df)} customers, ",
             "τ threshold={tau_threshold}, budget=",
             "{if(is.null(budget_gbp))'unlimited' else f_gbp(budget_gbp)}"),
        "AE")
      private$df_          <- run_action_engine(forecast_df, budget_gbp,
                                                 tau_threshold)
      private$summary_     <- campaign_summary(private$df_)
      private$sensitivity_ <- optimise_threshold(private$df_)

      cs_log("INFO",
        glue("Campaign: {private$summary_$n_targeted}/{private$summary_$n_total} targeted | ",
             "Persuadables: {private$summary_$n_persuadables} | ",
             "ROI: {round(private$summary_$roi,2)}×"),
        "AE")
      invisible(self)
    },

    print_brief = function() {
      s <- private$summary_
      cat(glue("
{'═'%x%60}
  CAMPAIGN BRIEF — CHURN SURGEON
{'═'%x%60}
  Customers analysed : {f_n(s$n_total)}
  Persuadables found : {f_n(s$n_persuadables)} ({f_pct(s$n_persuadables/s$n_total)})
  Recommended target : {f_n(s$n_targeted)}
  Total offer cost   : {f_gbp(s$total_cost)}
  Net value saved    : {f_gbp(s$total_nv)}
  Campaign ROI       : {round(s$roi,2)}×
  Mean τ (targeted)  : {round(s$mean_tau_targeted,4)}
  Mean τ (all)       : {round(s$mean_tau_all,4)}

  SURE THINGS (do NOT send offer): {f_n(s$n_sure_things)}
  LOST CAUSES (futile):            {f_n(s$n_lost_causes)}
  SLEEPING DOGS (would backfire):  {f_n(s$n_sleeping_dogs)}
{'═'%x%60}
\n"))
    },

    optimal_threshold = function() {
      private$sensitivity_ |>
        filter(roi==max(roi, na.rm=TRUE)) |>
        slice_head(n=1)
    },

    decisions    = function() private$df_,
    summary      = function() private$summary_,
    sensitivity  = function() private$sensitivity_,
    target_list  = function() filter(private$df_, eligible)
  )
)

f_n <- function(x) formatC(round(x), format="d", big.mark=",")

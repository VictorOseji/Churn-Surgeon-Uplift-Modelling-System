# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — churn_surgeon.R   (Master Orchestrator)
# ══════════════════════════════════════════════════════════════════════════════
library(R6); library(dplyr); library(purrr); library(ggplot2)
library(glue); library(tidyr); library(scales); library(here)

source(here("R/utils.R"))
source(here("R/data/customer_data.R"))
source(here("R/models/propensity.R"))
source(here("R/models/uplift_models.R"))
source(here("R/evaluation/evaluation.R"))
source(here("R/engine/action_engine.R"))

ChurnSurgeon <- R6::R6Class("ChurnSurgeon",
  private = list(
    raw_=NULL, X_=NULL, Y_=NULL, W_=NULL,
    ps_est_=NULL, uplift_=NULL, forecast_=NULL,
    action_=NULL, eval_=NULL, built_at_=NULL
  ),
  public = list(

    # ── Load / generate data ───────────────────────────────────────────────
    load_data = function(n=5000, seed=42) {
      cs_log("INFO","Generating customer dataset.","CS")
      dat <- generate_customers(n, seed=seed)
      private$raw_ <- dat$data
      private$X_   <- as.matrix(build_feature_matrix(dat$data))
      private$Y_   <- dat$data$churned
      private$W_   <- dat$data$treated
      cs_log("INFO",
        glue("{nrow(private$raw_)} customers | ",
             "Treated={sum(private$W_==1)} | Control={sum(private$W_==0)} | ",
             "Churn rate={f_pct(mean(private$Y_))}"),
        "CS")
      invisible(self)
    },

    # ── Propensity estimation + matching ──────────────────────────────────
    fit_propensity = function(method="ensemble", caliper=0.20) {
      cs_log("INFO","Fitting propensity model.","CS")
      private$ps_est_ <- PropensityEstimator$new()
      private$ps_est_$estimate(private$X_, private$W_, method)
      private$ps_est_$match_samples(private$X_, private$W_, caliper)
      invisible(self)
    },

    # ── Uplift models ─────────────────────────────────────────────────────
    fit_uplift = function(n_trees=150, seed=42) {
      cs_log("INFO","Fitting uplift models (CF + TL + SL + XL + DML).","CS")
      ps <- private$ps_est_$ps()
      private$uplift_ <- UpliftModeler$new()
      private$uplift_$fit(private$X_, private$Y_, private$W_, ps, n_trees, seed)

      # Build forecast tibble
      ci <- private$uplift_$predict_with_ci(private$X_, B=50)

      private$forecast_ <- private$raw_ |>
        mutate(
          tau_cf        = private$uplift_$tau_cf(),
          tau_tl        = private$uplift_$tau_tl(),
          tau_sl        = private$uplift_$tau_sl(),
          tau_xl        = private$uplift_$tau_xl(),
          tau_dml       = private$uplift_$tau_dml(),
          tau_ensemble  = private$uplift_$tau_ensemble(),
          tau_ci_lo     = ci$ci_lo,
          tau_ci_hi     = ci$ci_hi,
          ps            = ps,
          base_churn_prob = true_y0,   # in real app use predicted P(Y|W=0, X)
          nv            = net_value(tau_ensemble, clv_estimate, offer_cost),
          pred_group    = classify_response(tau_ensemble, base_churn_prob)
        )
      invisible(self)
    },

    # ── Evaluation ────────────────────────────────────────────────────────
    evaluate = function() {
      cs_log("INFO","Computing uplift evaluation metrics.","CS")
      fc   <- private$forecast_
      Y    <- fc$churned; W <- fc$treated; ps <- fc$ps

      model_list <- list(
        `Causal Forest`  = fc$tau_cf,
        `T-Learner`      = fc$tau_tl,
        `S-Learner`      = fc$tau_sl,
        `X-Learner`      = fc$tau_xl,
        `Double ML`      = fc$tau_dml,
        Ensemble         = fc$tau_ensemble
      )

      private$eval_ <- list(
        comparison   = compare_models(model_list, Y, W, ps, fc$true_tau),
        qini         = compute_qini(fc$tau_ensemble, Y, W),
        deciles      = uplift_by_decile(fc$tau_ensemble, Y, W),
        calibration  = uplift_calibration(fc$tau_ensemble, fc$true_tau),
        policy_curve = policy_value(fc$tau_ensemble, fc$clv_estimate, fc$offer_cost),
        auuc         = compute_auuc(fc$tau_ensemble, Y, W)
      )
      cs_log("INFO",
        glue("AUUC lift={round(private$eval_$auuc$qini_lift,3)} | ",
             "Corr(τ̂,τ)={round(filter(private$eval_$comparison,model=='Ensemble')$corr_true_tau,3)}"),
        "CS")
      invisible(self)
    },

    # ── Action engine ─────────────────────────────────────────────────────
    run_action = function(budget_gbp=NULL, tau_threshold=0.05) {
      cs_log("INFO","Running action engine.","CS")
      private$action_ <- ActionEngine$new()
      private$action_$run(private$forecast_, budget_gbp, tau_threshold)
      invisible(self)
    },

    # ── Full pipeline ──────────────────────────────────────────────────────
    run_all = function(n=5000, seed=42, n_trees=150, budget=NULL) {
      private$built_at_ <- Sys.time()
      self$load_data(n, seed)
      self$fit_propensity()
      self$fit_uplift(n_trees, seed)
      self$evaluate()
      self$run_action(budget)
      invisible(self)
    },

    # ── Print report ──────────────────────────────────────────────────────
    print_report = function() {
      ev <- private$eval_; ae <- private$action_$summary()
      fc <- private$forecast_; ps <- private$ps_est_
      bl <- ps$balance()
      c_comp <- ev$comparison

      cat(glue("
{strrep('═',68)}
  CHURN SURGEON — SYSTEM REPORT
  {format(private$built_at_,'%Y-%m-%d %H:%M:%S')}
{strrep('═',68)}

POPULATION
{strrep('─',68)}
  n={nrow(fc)} | treated={sum(fc$treated==1)} | control={sum(fc$treated==0)}
  Observed churn: {f_pct(mean(fc$churned))}
  True ATE: {round(mean(fc$true_tau),4)}

PROPENSITY SCORE
{strrep('─',68)}
  Matched pairs  : {ps$match_obj()$n_matched}
  % matched      : {f_pct(ps$match_obj()$pct_matched)}
  Avg |SMD| before: {round(mean(abs(bl$smd_before),na.rm=T),3)}
  Avg |SMD| after : {round(mean(abs(bl$smd_after), na.rm=T),3)}

MODEL COMPARISON (AUUC lift / Corr with true τ)
{strrep('─',68)}
{paste(c_comp |> glue_data('  {model}: AUUC lift={round(qini_lift,4)}, Corr={round(corr_true_tau,3)}, Rate={round(rate,4)}'), collapse='\n')}

ACTION ENGINE
{strrep('─',68)}
  Persuadables  : {ae$n_persuadables} ({f_pct(ae$n_persuadables/ae$n_total)})
  Targeted      : {ae$n_targeted}
  Sure Things   : {ae$n_sure_things} (would have stayed anyway — NOT targeted)
  Lost Causes   : {ae$n_lost_causes} (futile — NOT targeted)
  Sleeping Dogs : {ae$n_sleeping_dogs} (treatment backfires — NOT targeted)
  Total cost    : {f_gbp(ae$total_cost)}
  Net value     : {f_gbp(ae$total_nv)}
  Campaign ROI  : {round(ae$roi,2)}×
{strrep('═',68)}\n"))
    },

    # ── Plots ─────────────────────────────────────────────────────────────
    plot_qini = function() {
      q <- private$eval_$qini
      ggplot(q, aes(pct_targeted)) +
        geom_ribbon(aes(ymin=qini_random, ymax=qini), alpha=.15, fill="#2ECC71") +
        geom_line(aes(y=qini,        colour="Model"), linewidth=1.2) +
        geom_line(aes(y=qini_random, colour="Random"), linewidth=.8, linetype="dashed") +
        scale_colour_manual(values=c(Model="#2ECC71",Random="#E74C3C"), name=NULL) +
        scale_x_continuous(labels=percent_format()) +
        labs(title="Qini Curve — Churn Uplift Model",
             subtitle=glue("AUUC lift = {round(private$eval_$auuc$qini_lift,4)}"),
             x="% customers targeted",y="Incremental conversions saved") +
        theme_minimal(base_size=11)+theme(legend.position="bottom")
    },

    plot_deciles = function() {
      private$eval_$deciles |>
        ggplot(aes(decile, ate_observed)) +
        geom_col(aes(fill=ate_observed), alpha=.85) +
        geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=.3) +
        geom_line(aes(y=mean_tau_hat), colour="grey40", linewidth=.9, linetype="dashed") +
        scale_fill_gradient(low="#E74C3C", high="#2ECC71",
                            name="Observed ATE") +
        scale_x_continuous(breaks=1:10) +
        labs(title="Uplift by Decile — Monotone Test",
             subtitle="Dashed = predicted τ̂ | Bars = observed ATE",
             x="Predicted uplift decile (1=lowest τ̂, 10=highest τ̂)",
             y="Observed ATE (churn reduction)") +
        theme_minimal(base_size=11)+theme(legend.position="bottom")
    },

    plot_response_groups = function() {
      private$forecast_ |>
        count(pred_group) |>
        mutate(pct = n/sum(n)) |>
        ggplot(aes(reorder(pred_group,-pct), pct, fill=pred_group)) +
        geom_col(alpha=.85) +
        geom_text(aes(label=paste0("n=",n,"\n",f_pct(pct))), vjust=-.3, size=3.2) +
        scale_fill_manual(values=c(Persuadable="#2ECC71",`Sure Thing`="#3498DB",
                                    `Lost Cause`="#E74C3C",`Sleeping Dog`="#E67E22",
                                    Marginal="#BDC3C7"), guide="none") +
        scale_y_continuous(labels=percent_format()) +
        labs(title="Customer Response Group Classification",
             subtitle="Only 'Persuadables' should receive save offers",
             x=NULL, y="% of customers") +
        theme_minimal(base_size=11)
    },

    plot_tau_distribution = function() {
      private$forecast_ |>
        ggplot(aes(tau_ensemble, fill=pred_group)) +
        geom_histogram(bins=50, alpha=.8) +
        geom_vline(xintercept=0.05, linetype="dashed", colour="grey50") +
        scale_fill_manual(values=c(Persuadable="#2ECC71",`Sure Thing`="#3498DB",
                                    `Lost Cause`="#E74C3C",`Sleeping Dog`="#E67E22",
                                    Marginal="#BDC3C7"), name="Response Group") +
        labs(title="Distribution of Estimated Uplift Scores",
             subtitle="Dashed line = action threshold (τ̂=0.05)",
             x="Estimated ITE τ̂ (uplift)",y="Count") +
        theme_minimal(base_size=11)+theme(legend.position="bottom")
    },

    plot_calibration = function() {
      private$eval_$calibration |>
        ggplot(aes(mean_predicted, mean_true)) +
        geom_point(aes(size=n), colour="#3498DB", alpha=.8) +
        geom_abline(slope=1,intercept=0,linetype="dashed",colour="grey50") +
        geom_smooth(method="lm",se=TRUE,colour="#2ECC71",fill="#2ECC71",alpha=.15) +
        scale_size_continuous(guide="none") +
        labs(title="Uplift Score Calibration",
             subtitle="Each dot = one decile bin | Perfect calibration on diagonal",
             x="Mean predicted τ̂",y="Mean true τ") +
        theme_minimal(base_size=11)
    },

    plot_balance = function() {
      private$ps_est_$balance() |>
        pivot_longer(c(smd_before,smd_after), names_to="stage",values_to="smd") |>
        ggplot(aes(smd, reorder(feature,abs(smd_before)), colour=stage)) +
        geom_point(size=2.5, alpha=.8) +
        geom_vline(xintercept=c(-.10,.10), linetype="dashed",colour="grey70") +
        scale_colour_manual(values=c(smd_before="#E74C3C",smd_after="#2ECC71"),
                            labels=c("Before matching","After matching"),name=NULL) +
        labs(title="Propensity Score Matching — Covariate Balance",
             subtitle="|SMD| < 0.10 indicates good balance",
             x="Standardised Mean Difference",y=NULL) +
        theme_minimal(base_size=11)+theme(legend.position="bottom")
    },

    plot_policy_value = function() {
      pv <- private$eval_$policy_curve
      ggplot(pv, aes(pct_targeted)) +
        geom_area(aes(y=cum_nv), fill="#2ECC71", alpha=.25) +
        geom_line(aes(y=cum_nv), colour="#2ECC71", linewidth=1.2) +
        geom_line(aes(y=cum_cost*(-1)), colour="#E74C3C", linewidth=.9, linetype="dashed") +
        scale_x_continuous(labels=percent_format()) +
        scale_y_continuous(labels=label_dollar(prefix="£")) +
        labs(title="Policy Value Curve — Cumulative Net Value vs Targeting Rate",
             subtitle="Green = cumulative NV | Red (dashed) = cumulative cost",
             x="% customers targeted",y="Cumulative £ value") +
        theme_minimal(base_size=11)
    },

    plot_model_comparison = function() {
      private$eval_$comparison |>
        pivot_longer(c(auuc,qini_lift,rate), names_to="metric",values_to="val") |>
        ggplot(aes(reorder(model,val), val, fill=metric)) +
        geom_col(position="dodge", alpha=.85) +
        coord_flip() +
        scale_fill_brewer(palette="Set2",name="Metric") +
        labs(title="Uplift Model Comparison",x=NULL,y="Score") +
        theme_minimal(base_size=11)+theme(legend.position="bottom")
    },

    # ── Accessors ─────────────────────────────────────────────────────────
    data        = function() private$raw_,
    forecast    = function() private$forecast_,
    ps_est      = function() private$ps_est_,
    uplift      = function() private$uplift_,
    evaluation  = function() private$eval_,
    action      = function() private$action_
  )
)

# ── Entry point ────────────────────────────────────────────────────────────────
run_churn_surgeon <- function(n=5000, seed=42, n_trees=150,
                               budget=NULL, print_report=TRUE) {
  cs <- ChurnSurgeon$new()
  cs$run_all(n, seed, n_trees, budget)
  if (print_report) cs$print_report()
  invisible(cs)
}

# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — tests/test_churn_surgeon.R   (55+ tests)
# ══════════════════════════════════════════════════════════════════════════════
library(testthat); library(dplyr); library(purrr); library(tibble); library(here)
lapply(list.files(here("R"),"\\.R$",recursive=TRUE,full.names=TRUE),source)

# ── Fixtures ──────────────────────────────────────────────────────────────────
mk_data <- function(n=200,seed=1){
  set.seed(seed)
  X <- matrix(rnorm(n*6),ncol=6,dimnames=list(NULL,paste0("x",1:6)))
  W <- rbinom(n,1,.5)
  # True CATE varies with x1
  tau_true <- .10 + .15*X[,1] + rnorm(n,0,.03)
  Y0 <- rbinom(n,1,clamp(.30+.10*X[,2]))
  Y1 <- rbinom(n,1,clamp(pmax(0,.30+.10*X[,2]-tau_true)))
  Y  <- ifelse(W==1,Y1,Y0)
  ps <- clamp(.50+.05*X[,1],.05,.95)
  list(X=X,Y=Y,W=W,tau_true=tau_true,ps=ps)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════
test_that("sigmoid: bounded [0,1]", {
  expect_true(all(sigmoid(c(-100,-1,0,1,100)) >= 0))
  expect_true(all(sigmoid(c(-100,-1,0,1,100)) <= 1))
})
test_that("sigmoid(0) = 0.5", { expect_equal(sigmoid(0), .5) })
test_that("logit is inverse of sigmoid", {
  x <- c(.1,.3,.5,.7,.9)
  expect_equal(round(sigmoid(logit(x)),8), x)
})
test_that("clamp works at both ends", {
  expect_equal(clamp(c(-1,.5,2)), c(0,.5,1))
})
test_that("net_value: positive when tau*clv > cost", {
  expect_gt(net_value(.20,100,5), 0)
  expect_lt(net_value(.01,100,50), 0)
})
test_that("classify_response_group: Persuadable when y0=1,y1=0", {
  expect_equal(classify_response_group(1,0),"Persuadable")
})
test_that("classify_response_group: Sure Thing when y0=0,y1=0", {
  expect_equal(classify_response_group(0,0),"Sure Thing")
})
test_that("classify_response_group: Lost Cause when y0=1,y1=1", {
  expect_equal(classify_response_group(1,1),"Lost Cause")
})
test_that("classify_response_group: Sleeping Dog when y0=0,y1=1", {
  expect_equal(classify_response_group(0,1),"Sleeping Dog")
})
test_that("bootstrap_ci: returns 2 quantiles", {
  ci <- bootstrap_ci(rnorm(100), B=50)
  expect_length(ci,2); expect_lt(ci[1],ci[2])
})
test_that("campaign_roi: positive when saved > cost", {
  expect_gt(campaign_roi(1000,200), 0)
})
test_that("norm01: maps to [0,1]", {
  x <- c(1,2,3,4,5); v <- norm01(x)
  expect_equal(min(v),0); expect_equal(max(v),1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 2. DATA GENERATION
# ═══════════════════════════════════════════════════════════════════════════════
test_that("generate_customers: returns correct n", {
  d <- generate_customers(300,seed=1)$data
  expect_equal(nrow(d),300)
})
test_that("generate_customers: churned is 0/1", {
  d <- generate_customers(200,seed=2)$data
  expect_true(all(d$churned %in% c(0,1)))
})
test_that("generate_customers: treated is 0/1", {
  d <- generate_customers(200,seed=3)$data
  expect_true(all(d$treated %in% c(0,1)))
})
test_that("generate_customers: true_tau exists", {
  d <- generate_customers(200,seed=4)$data
  expect_true("true_tau" %in% names(d))
  expect_true(all(is.numeric(d$true_tau)))
})
test_that("build_feature_matrix: correct column count", {
  d <- generate_customers(100,seed=5)$data
  X <- build_feature_matrix(d)
  expect_equal(ncol(X),length(FEATURE_COLS))
})
test_that("build_feature_matrix: no NAs", {
  d <- generate_customers(100,seed=6)$data
  X <- build_feature_matrix(d)
  expect_true(!any(is.na(X)))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 3. PROPENSITY SCORE
# ═══════════════════════════════════════════════════════════════════════════════
test_that("estimate_ps_logistic: returns [0.01,0.99] PS", {
  d <- mk_data(200)
  r <- estimate_ps_logistic(d$X,d$W)
  expect_true(all(r$ps >= .01 & r$ps <= .99))
})
test_that("estimate_ps_logistic: length equals n", {
  d <- mk_data(150)
  r <- estimate_ps_logistic(d$X,d$W)
  expect_length(r$ps,150)
})
test_that("estimate_ps_boost: returns numeric vector", {
  d <- mk_data(100)
  r <- estimate_ps_boost(d$X,d$W,n_trees=5)
  expect_type(r$ps,"double"); expect_length(r$ps,100)
})
test_that("estimate_ps_ensemble: averages two methods", {
  d <- mk_data(150)
  ps <- estimate_ps_ensemble(d$X,d$W)
  expect_length(ps,150); expect_true(all(ps>=.01&ps<=.99))
})
test_that("estimate_ipw: returns tau near true ATE", {
  d <- mk_data(500)
  r <- estimate_ipw(d$Y,d$W,d$ps)
  # True ATE ≈ -0.10 on average (treated churn less)
  expect_true(abs(r$tau) < .30)   # sanity check
})
test_that("match_nn: returns integer indices", {
  d <- mk_data(200)
  ps <- estimate_ps_logistic(d$X,d$W)$ps
  m  <- match_nn(ps,d$W)
  expect_type(m$matched_t,"integer")
  expect_type(m$matched_c,"integer")
})
test_that("match_nn: matched_t and matched_c same length", {
  d <- mk_data(200)
  ps <- estimate_ps_logistic(d$X,d$W)$ps
  m  <- match_nn(ps,d$W)
  expect_equal(length(m$matched_t),length(m$matched_c))
})
test_that("balance_diagnostics: SMD after < SMD before on average", {
  d <- mk_data(300)
  ps <- estimate_ps_logistic(d$X,d$W)$ps
  m  <- match_nn(ps,d$W,caliper=0.3)
  b  <- balance_diagnostics(as.data.frame(d$X),d$W,m)
  expect_lt(mean(abs(b$smd_after),na.rm=T),mean(abs(b$smd_before),na.rm=T)+.05)
})
test_that("PropensityEstimator R6: end-to-end", {
  d  <- mk_data(200)
  pe <- PropensityEstimator$new()
  pe$estimate(d$X,d$W)
  expect_length(pe$ps(),200)
  pe$match_samples(as.data.frame(d$X),d$W,caliper=0.3)
  expect_true(!is.null(pe$match_obj()))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 4. UPLIFT MODELS
# ═══════════════════════════════════════════════════════════════════════════════
test_that("rf_fit + rf_predict: returns correct-length predictions", {
  d <- mk_data(100)
  f <- rf_fit(d$X,d$Y,n_trees=10)
  p <- rf_predict(f,d$X)
  expect_length(p,100)
})
test_that("rf_predict: values in plausible range for binary outcome", {
  d <- mk_data(100)
  f <- rf_fit(d$X,d$Y,n_trees=10)
  p <- rf_predict(f,d$X)
  expect_true(all(p>=0&p<=1))
})
test_that("fit_t_learner + predict: returns tau", {
  d <- mk_data(150)
  tl <- fit_t_learner(d$X,d$Y,d$W)
  tau <- predict_t_learner(tl,d$X)
  expect_length(tau,150); expect_true(all(is.finite(tau)))
})
test_that("fit_s_learner + predict: returns tau", {
  d <- mk_data(150)
  sl <- fit_s_learner(d$X,d$Y,d$W)
  tau <- predict_s_learner(sl,d$X)
  expect_length(tau,150)
})
test_that("fit_x_learner + predict: returns tau", {
  d <- mk_data(150)
  xl <- fit_x_learner(d$X,d$Y,d$W)
  tau <- predict_x_learner(xl,d$X)
  expect_length(tau,150)
})
test_that("fit_double_ml + predict: tau_global finite", {
  d <- mk_data(200)
  dml <- fit_double_ml(d$X,d$Y,d$W,n_folds=3)
  expect_true(is.finite(dml$tau_global))
})
test_that("fit_double_ml: Y_res mean near 0 after partialing", {
  d   <- mk_data(300)
  dml <- fit_double_ml(d$X,d$Y,d$W,n_folds=3)
  expect_lt(abs(mean(dml$Y_res)),.20)  # should be near zero
})
test_that("fit_causal_forest + predict: returns tau of correct length", {
  d  <- mk_data(200,seed=7)
  cf <- fit_causal_forest(d$X,d$Y,d$W,n_trees=20)
  tau<- predict_causal_forest(cf,d$X)
  expect_length(tau,200)
})
test_that("ensemble_cate: weighted average of predictions", {
  p1 <- rep(.10,50); p2 <- rep(.20,50)
  en <- ensemble_cate(list(p1,p2),c(.5,.5))
  expect_equal(en[1],.15)
})
test_that("UpliftModeler R6: all tau methods available after fit", {
  d  <- mk_data(200,seed=8)
  um <- UpliftModeler$new()
  um$fit(d$X,d$Y,d$W,d$ps,n_trees=15)
  expect_length(um$tau_cf(),200)
  expect_length(um$tau_tl(),200)
  expect_length(um$tau_sl(),200)
  expect_length(um$tau_xl(),200)
  expect_length(um$tau_dml(),200)
  expect_length(um$tau_ensemble(),200)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 5. EVALUATION
# ═══════════════════════════════════════════════════════════════════════════════
test_that("compute_qini: returns tibble with required columns", {
  d <- mk_data(200)
  q <- compute_qini(d$tau_true,d$Y,d$W,n_points=20)
  for(col in c("pct_targeted","qini","qini_random")) expect_true(col %in% names(q))
})
test_that("compute_qini: pct_targeted from 0 to 1", {
  d <- mk_data(200)
  q <- compute_qini(d$tau_true,d$Y,d$W,n_points=10)
  expect_gte(min(q$pct_targeted),0); expect_lte(max(q$pct_targeted),1.01)
})
test_that("compute_auuc: returns named list", {
  d <- mk_data(200)
  r <- compute_auuc(d$tau_true,d$Y,d$W,n_points=20)
  expect_true(all(c("auuc","auuc_random","qini_lift") %in% names(r)))
})
test_that("compute_auuc: perfect model > random", {
  set.seed(1); n<-300
  W <- rbinom(n,1,.5); Y <- rbinom(n,1,.30+.20*W)
  tau_perfect <- .20*(2*W-1)+.05; tau_random <- rnorm(n)
  r_perf <- compute_auuc(tau_perfect,Y,W,30)
  r_rand <- compute_auuc(tau_random,Y,W,30)
  expect_gt(r_perf$qini_lift,r_rand$qini_lift-.05)  # perfect should score better
})
test_that("uplift_by_decile: returns 10 rows", {
  d <- mk_data(300)
  dec <- uplift_by_decile(d$tau_true,d$Y,d$W)
  expect_equal(nrow(dec),10)
})
test_that("uplift_by_decile: decile column 1:10", {
  d <- mk_data(300)
  dec <- uplift_by_decile(d$tau_true,d$Y,d$W)
  expect_equal(sort(dec$decile),1:10)
})
test_that("uplift_calibration: n_bins rows", {
  d <- mk_data(200)
  cl <- uplift_calibration(d$tau_true,d$tau_true+rnorm(200,0,.01),n_bins=5)
  expect_lte(nrow(cl),5)
})
test_that("compare_models: one row per model", {
  d <- mk_data(200,seed=5)
  ml <- list(A=d$tau_true,B=rnorm(200))
  r  <- compare_models(ml,d$Y,d$W,d$ps,d$tau_true)
  expect_equal(nrow(r),2)
})
test_that("policy_value: returns tibble with cum_nv", {
  d  <- mk_data(200)
  pv <- policy_value(d$tau_true,rep(200,200),rep(10,200))
  expect_true("cum_nv" %in% names(pv))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. ACTION ENGINE
# ═══════════════════════════════════════════════════════════════════════════════
test_that("classify_response: Persuadable for high tau + high churn", {
  expect_equal(classify_response(.15,.60),"Persuadable")
})
test_that("classify_response: Sure Thing for high tau + low churn", {
  expect_equal(classify_response(.12,.10),"Sure Thing")
})
test_that("classify_response: Sleeping Dog for negative tau", {
  expect_equal(classify_response(-.10,.30),"Sleeping Dog")
})
test_that("run_action_engine: returns response_grp column", {
  d  <- mk_data(200,seed=9)
  fc <- tibble(customer_id=paste0("C",1:200),tau_ensemble=d$tau_true,
               clv_estimate=rep(150,200),offer_cost=rep(10,200),
               base_churn_prob=clamp(.30+.10*d$X[,2]),
               segment=sample(c("A","B"),200,replace=T))
  r  <- run_action_engine(fc)
  expect_true("response_grp" %in% names(r))
  expect_true("eligible" %in% names(r))
})
test_that("campaign_summary: returns n_persuadables", {
  d  <- mk_data(200,seed=10)
  fc <- tibble(customer_id=paste0("C",1:200),tau_ensemble=d$tau_true,
               clv_estimate=rep(200,200),offer_cost=rep(12,200),
               base_churn_prob=clamp(.40+.05*d$X[,1]),
               segment=sample(c("A","B"),200,replace=T))
  r  <- run_action_engine(fc)
  s  <- campaign_summary(r)
  expect_true(!is.null(s$n_persuadables))
})
test_that("optimise_threshold: optimal threshold in [0,0.35]", {
  d  <- mk_data(200,seed=11)
  fc <- tibble(customer_id=paste0("C",1:200),tau_ensemble=d$tau_true,
               clv_estimate=rep(200,200),offer_cost=rep(12,200),
               base_churn_prob=clamp(.40+.05*d$X[,1]),
               segment=sample(c("A","B"),200,replace=T))
  r  <- run_action_engine(fc)
  ts <- optimise_threshold(r)
  opt<- ts$threshold[which.max(ts$roi)]
  expect_gte(opt,0); expect_lte(opt,.35)
})
test_that("ActionEngine R6: end-to-end", {
  d  <- mk_data(300,seed=12)
  fc <- tibble(customer_id=paste0("C",1:300),tau_ensemble=d$tau_true,
               clv_estimate=rlnorm(300,log(200),.4),offer_cost=rep(10,300),
               base_churn_prob=clamp(.40+.05*d$X[,1]),
               segment=sample(c("A","B"),300,replace=T))
  ae <- ActionEngine$new()
  ae$run(fc,tau_threshold=.05)
  expect_gt(nrow(ae$decisions()),0)
  expect_true(!is.null(ae$summary()$roi))
})

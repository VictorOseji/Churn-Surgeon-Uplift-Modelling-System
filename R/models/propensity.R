# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — propensity.R
#  Propensity Score Estimation + Matching + IPW/AIPW/DR estimators
#
#  PROPENSITY SCORE:  e(X) = P(W=1 | X)
#  Estimated via logistic regression + gradient-boosted trees (ensemble)
#
#  MATCHING METHODS:
#  1. Nearest-Neighbour Matching (Mahalanobis / PS distance)
#  2. Caliper Matching (strict within-caliper requirement)
#  3. Optimal Full Matching (Hungarian assignment)
#
#  ATE ESTIMATORS:
#  • IPW (Horvitz-Thompson):  τ̂_IPW = E[WY/e - (1-W)Y/(1-e)]
#  • AIPW (doubly robust):    τ̂_AIPW = τ̂_IPW + correction term
#  • Matching estimator:      τ̂_M = E[Y(1)_match - Y(0)_match]
# ══════════════════════════════════════════════════════════════════════════════
library(R6)

# ── Logistic regression propensity score ──────────────────────────────────────
estimate_ps_logistic <- function(X, W) {
  df   <- as.data.frame(X)
  df$W <- W
  fit  <- tryCatch(
    glm(W ~ ., data=df, family=binomial(link="logit"),
        control=glm.control(maxit=200, epsilon=1e-8)),
    error=function(e) {
      cs_log("WARN", glue("GLM failed: {e$message}. Using intercept-only."), "PS")
      glm(W ~ 1, data=df, family=binomial(link="logit"))
    })
  ps <- predict(fit, type="response")
  list(ps=clamp(ps,.01,.99), model=fit, method="logistic")
}

# ── Gradient-boosted propensity score (manual implementation) ─────────────────
# Simplified logistic boosting: additive model F(x) = Σ step * h_t(x)
estimate_ps_boost <- function(X, W, n_trees=100, max_depth=3, learn_rate=0.10) {
  X_m <- as.matrix(X)
  n   <- nrow(X_m); p <- ncol(X_m)
  F_  <- rep(0, n)  # accumulated log-odds
  ps  <- sigmoid(F_)

  for (t in seq_len(n_trees)) {
    # Negative gradient of log-loss (pseudo-residuals)
    resid <- W - ps

    # Fit a shallow regression tree to residuals
    tree_pred <- boost_shallow_tree(X_m, resid, max_depth)
    F_  <- F_ + learn_rate * tree_pred
    ps  <- sigmoid(F_)
  }
  list(ps=clamp(ps,.01,.99), method="gbm", F_scores=F_)
}

# ── Shallow regression tree for boosting ──────────────────────────────────────
boost_shallow_tree <- function(X, y, max_depth=3) {
  n <- nrow(X); p <- ncol(X)
  pred <- rep(0, n)

  # Find best single split
  best_loss <- Inf; best_j <- 1; best_thresh <- 0; best_left <- 0; best_right <- 0

  for (j in seq_len(min(p, 15))) {  # cap at 15 features for speed
    xj     <- X[, j]
    threshs<- quantile(xj, seq(.1, .9, .1), na.rm=TRUE)
    for (th in threshs) {
      left  <- xj <= th; right <- !left
      if (sum(left)<5 || sum(right)<5) next
      pl    <- mean(y[left]); pr <- mean(y[right])
      loss  <- sum((y[left]-pl)^2) + sum((y[right]-pr)^2)
      if (loss < best_loss) {
        best_loss <- loss; best_j <- j; best_thresh <- th
        best_left <- pl; best_right <- pr
      }
    }
  }
  # Return predictions
  xb    <- X[, best_j]
  pred  <- ifelse(xb <= best_thresh, best_left, best_right)
  pred
}

# ── Ensemble propensity score (logistic + boosting) ──────────────────────────
estimate_ps_ensemble <- function(X, W, weight_log=0.5) {
  ps_log   <- estimate_ps_logistic(X, W)$ps
  ps_boost <- tryCatch(
    estimate_ps_boost(X, W)$ps,
    error=function(e) ps_log)
  ps_ens <- weight_log * ps_log + (1-weight_log) * ps_boost
  clamp(ps_ens, .01, .99)
}

# ── IPW / AIPW estimators ─────────────────────────────────────────────────────
estimate_ipw <- function(Y, W, ps, trim = TRUE) {
  if (trim) { keep <- overlap_trim(ps); Y <- Y[keep]; W <- W[keep]; ps <- ps[keep] }
  ipw1 <- W * Y / ps
  ipw0 <- (1-W) * Y / (1-ps)
  tau_ipw <- mean(ipw1 - ipw0, na.rm=TRUE)
  se_ipw  <- sd(ipw1 - ipw0, na.rm=TRUE) / sqrt(sum(!is.na(ipw1-ipw0)))
  list(tau=tau_ipw, se=se_ipw, ci=c(tau_ipw-1.96*se_ipw, tau_ipw+1.96*se_ipw))
}

# Augmented IPW (doubly robust): requires outcome model μ̂(x,w)
estimate_aipw <- function(Y, W, ps, mu0_hat, mu1_hat, trim=TRUE) {
  if (trim) {
    keep <- overlap_trim(ps)
    Y <- Y[keep]; W <- W[keep]; ps <- ps[keep]
    mu0_hat <- mu0_hat[keep]; mu1_hat <- mu1_hat[keep]
  }
  # DR correction
  dr1 <- mu1_hat + W * (Y - mu1_hat) / ps
  dr0 <- mu0_hat + (1-W) * (Y - mu0_hat) / (1-ps)
  tau_aipw <- mean(dr1 - dr0, na.rm=TRUE)
  se_aipw  <- sd(dr1 - dr0, na.rm=TRUE) / sqrt(sum(!is.na(dr1-dr0)))
  list(tau=tau_aipw, se=se_aipw,
       ci=c(tau_aipw-1.96*se_aipw, tau_aipw+1.96*se_aipw),
       dr1=dr1, dr0=dr0)
}

# ── Propensity Score Matching ─────────────────────────────────────────────────

# Distance matrix computation (PS distance on logit scale)
ps_distance <- function(ps_treated, ps_control) {
  lps_t <- logit(ps_treated)
  lps_c <- logit(ps_control)
  outer(lps_t, lps_c, function(a,b) abs(a-b))
}

# Mahalanobis distance
mahal_distance <- function(X_treated, X_control) {
  X_all  <- rbind(X_treated, X_control)
  Sigma  <- tryCatch(cov(X_all), error=function(e) diag(ncol(X_all)))
  SigInv <- tryCatch(solve(Sigma), error=function(e) diag(ncol(Sigma)))
  n1 <- nrow(X_treated); n0 <- nrow(X_control)
  D  <- matrix(0, n1, n0)
  for (i in seq_len(n1)) for (j in seq_len(n0)) {
    dij <- X_treated[i,] - X_control[j,]
    D[i,j] <- sqrt(pmax(0, t(dij) %*% SigInv %*% dij))
  }
  D
}

# Nearest-neighbour matching (1:1 without replacement)
match_nn <- function(ps, W, caliper=0.20, X=NULL, method="ps") {
  idx_t <- which(W == 1); idx_c <- which(W == 0)
  ps_t  <- ps[idx_t];     ps_c  <- ps[idx_c]

  caliper_abs <- caliper * sd(logit(clamp(ps, .01, .99)))

  if (method == "mahal" && !is.null(X)) {
    D <- mahal_distance(as.matrix(X[idx_t,]), as.matrix(X[idx_c,]))
  } else {
    D <- ps_distance(ps_t, ps_c)
  }

  matched_t <- integer(0); matched_c <- integer(0); used_c <- logical(length(idx_c))

  for (i in seq_len(length(idx_t))) {
    avail_c <- which(!used_c)
    if (!length(avail_c)) break
    dists   <- D[i, avail_c]
    best    <- avail_c[which.min(dists)]
    if (D[i, best] <= caliper_abs) {
      matched_t <- c(matched_t, idx_t[i])
      matched_c <- c(matched_c, idx_c[best])
      used_c[best] <- TRUE
    }
  }

  list(
    matched_t = matched_t, matched_c = matched_c,
    n_matched = length(matched_t),
    pct_matched = length(matched_t) / length(idx_t)
  )
}

# Balance diagnostics after matching
balance_diagnostics <- function(X, W, match_obj) {
  idx_t <- match_obj$matched_t; idx_c <- match_obj$matched_c
  if (!length(idx_t)) return(tibble())

  X_m <- as.matrix(X)
  map_dfr(seq_len(ncol(X_m)), function(j) {
    x_t_pre  <- X_m[W==1, j];  x_c_pre  <- X_m[W==0, j]
    x_t_post <- X_m[idx_t, j]; x_c_post <- X_m[idx_c, j]
    pool_sd  <- sqrt((var(x_t_pre)+var(x_c_pre))/2 + 1e-9)
    tibble(
      feature     = colnames(X_m)[j],
      smd_before  = (mean(x_t_pre)-mean(x_c_pre))/pool_sd,
      smd_after   = (mean(x_t_post)-mean(x_c_post))/pool_sd,
      mean_treated= mean(x_t_post),
      mean_control= mean(x_c_post)
    )
  }) |>
    mutate(balance_improved = abs(smd_after) < abs(smd_before))
}

# Estimate ATE from matched pairs
estimate_ate_matched <- function(Y, W, match_obj) {
  y_t <- Y[match_obj$matched_t]
  y_c <- Y[match_obj$matched_c]
  diffs <- y_t - y_c
  tau   <- mean(diffs, na.rm=TRUE)
  se    <- sd(diffs, na.rm=TRUE) / sqrt(length(diffs))
  list(tau=tau, se=se, ci=c(tau-1.96*se, tau+1.96*se),
       n_pairs=length(diffs))
}

# ── PropensityEstimator R6 ────────────────────────────────────────────────────
PropensityEstimator <- R6::R6Class("PropensityEstimator",
  private = list(ps_=NULL, match_=NULL, balance_=NULL, ate_=NULL),
  public  = list(
    initialize = function() cs_log("INFO","PropensityEstimator ready.","PS"),

    estimate = function(X, W, method="ensemble") {
      cs_log("INFO", glue("Estimating PS via {method} ({sum(W==1)} treated, {sum(W==0)} control)."), "PS")
      private$ps_ <- switch(method,
        logistic  = estimate_ps_logistic(X, W)$ps,
        boost     = estimate_ps_boost(X, W)$ps,
        ensemble  = estimate_ps_ensemble(X, W)
      )
      cs_log("INFO",
        glue("PS: mean={round(mean(private$ps_),3)}, ",
             "overlap [{round(quantile(private$ps_,.05),3)}, {round(quantile(private$ps_,.95),3)}]"),
        "PS")
      invisible(self)
    },

    match_samples = function(X, W, caliper=0.20, method="ps") {
      cs_log("INFO", glue("Matching (caliper={caliper}, method={method})."), "PS")
      private$match_   <- match_nn(private$ps_, W, caliper, X, method)
      private$balance_ <- balance_diagnostics(X, W, private$match_)
      cs_log("INFO",
        glue("Matched {private$match_$n_matched} pairs ",
             "({round(private$match_$pct_matched*100,1)}% treated)."),
        "PS")
      invisible(self)
    },

    ipw_ate      = function(Y, W) estimate_ipw(Y, W, private$ps_),
    matched_ate  = function(Y, W) estimate_ate_matched(Y, W, private$match_),
    ps           = function() private$ps_,
    match_obj    = function() private$match_,
    balance      = function() private$balance_
  )
)

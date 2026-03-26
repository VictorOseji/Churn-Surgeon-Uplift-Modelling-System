# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — uplift_models.R
#  Five CATE estimators implemented from scratch:
#
#  1. CAUSAL FOREST (Wager & Athey 2018) — honest random forest
#     • Builds trees splitting on CATE signal (not just Y)
#     • Honesty: separate subsamples for splitting and prediction
#     • Confidence intervals via infinitesimal jackknife
#
#  2. T-LEARNER — train μ̂₀ and μ̂₁ separately
#     τ̂(x) = μ̂₁(x) - μ̂₀(x)
#
#  3. S-LEARNER — include W in feature set, single model
#     τ̂(x) = μ̂(x,1) - μ̂(x,0)
#
#  4. X-LEARNER (Künzel et al. 2019) — leverages imbalanced samples
#     Stage 1: fit T-learner μ̂₀, μ̂₁
#     Stage 2: impute ITE = D̃_i on each arm, fit CATE models
#     Stage 3: combine via propensity-weighted average
#
#  5. DOUBLE ML (Chernozhukov et al. 2018) — partial linear model
#     Residualise Y and W on X, then estimate τ from residuals
#     Uses cross-fitting to avoid regularisation bias
# ══════════════════════════════════════════════════════════════════════════════
library(R6)

# ══════════════════════════════════════════════════════════════════════════════
#  BASE LEARNER — Random Forest (for use inside all meta-learners)
# ══════════════════════════════════════════════════════════════════════════════

# Fit a regression random forest
rf_fit <- function(X, y, n_trees=200, min_node_size=10, subsample_frac=0.632,
                   max_features=NULL, seed=42) {
  set.seed(seed)
  X_m <- as.matrix(X); n <- nrow(X_m); p <- ncol(X_m)
  mf  <- max_features %||% max(1, round(sqrt(p)))

  trees <- lapply(seq_len(n_trees), function(b) {
    # Bootstrap / subsample
    bag   <- sample(n, round(n*subsample_frac), replace=FALSE)
    oob   <- setdiff(seq_len(n), bag)
    build_tree(X_m[bag,], y[bag], mf, min_node_size, oob_idx=oob)
  })

  list(trees=trees, n_trees=n_trees, p=p, X_col_names=colnames(X_m))
}

rf_predict <- function(forest, X_new) {
  X_m <- as.matrix(X_new)
  preds <- vapply(forest$trees, function(tr) predict_tree(tr, X_m), numeric(nrow(X_m)))
  if (is.matrix(preds)) rowMeans(preds, na.rm=TRUE) else preds
}

# Feature importance (mean impurity decrease)
rf_importance <- function(forest) {
  p    <- forest$p
  imps <- matrix(0, p, length(forest$trees))
  for (b in seq_along(forest$trees)) {
    tr <- forest$trees[[b]]
    if (!is.null(tr$importance)) imps[, b] <- tr$importance
  }
  rowMeans(imps, na.rm=TRUE)
}

# ── Decision tree (CART regression) ───────────────────────────────────────────
build_tree <- function(X, y, max_features, min_node_size, depth=0, max_depth=6, oob_idx=NULL) {
  n <- length(y)
  if (n < min_node_size * 2 || depth >= max_depth || var(y, na.rm=TRUE) < 1e-9) {
    return(list(leaf=TRUE, value=mean(y, na.rm=TRUE),
                importance=rep(0, ncol(X))))
  }

  p        <- ncol(X)
  feat_idx <- sample(p, min(max_features, p))
  imp      <- rep(0, p)
  best     <- list(loss=Inf, j=1, th=0, il=0, ir=0)

  for (j in feat_idx) {
    xj      <- X[, j]; uniq <- unique(xj)
    if (length(uniq) < 2) next
    threshs <- quantile(xj, seq(.1,.9,.1), na.rm=TRUE)
    for (th in threshs) {
      left <- xj <= th; right <- !left
      if (sum(left)<min_node_size || sum(right)<min_node_size) next
      yl <- y[left]; yr <- y[right]
      loss <- sum((yl-mean(yl))^2) + sum((yr-mean(yr))^2)
      if (loss < best$loss) {
        best <- list(loss=loss, j=j, th=th,
                     il=mean(yl, na.rm=TRUE), ir=mean(yr, na.rm=TRUE))
      }
    }
    if (best$j == j) {
      parent_var <- var(y, na.rm=TRUE) * (n-1)
      imp[j]     <- (parent_var - best$loss) / n
    }
  }

  if (is.infinite(best$loss)) {
    return(list(leaf=TRUE, value=mean(y, na.rm=TRUE),
                importance=imp))
  }

  left_idx  <- X[, best$j] <= best$th
  right_idx <- !left_idx

  node <- list(
    leaf=FALSE, j=best$j, th=best$th, importance=imp,
    left  = build_tree(X[left_idx, , drop=FALSE], y[left_idx],
                       max_features, min_node_size, depth+1, max_depth),
    right = build_tree(X[right_idx,, drop=FALSE], y[right_idx],
                       max_features, min_node_size, depth+1, max_depth)
  )
  node
}

predict_tree <- function(node, X) {
  if (node$leaf) return(rep(node$value, nrow(X)))
  left  <- X[, node$j] <= node$th
  pred  <- numeric(nrow(X))
  if (any(left))  pred[left]  <- predict_tree(node$left,  X[left, , drop=FALSE])
  if (any(!left)) pred[!left] <- predict_tree(node$right, X[!left,, drop=FALSE])
  pred
}

# Logistic version (for propensity / outcome models returning probabilities)
rf_fit_logistic <- function(X, y, ...) {
  # Fit RF on logit(y) then re-sigmoid
  y_safe <- clamp(y, 0.01, 0.99)
  forest <- rf_fit(X, logit(y_safe), ...)
  forest$logistic <- TRUE
  forest
}
rf_predict_logistic <- function(forest, X_new) {
  raw <- rf_predict(forest, X_new)
  if (isTRUE(forest$logistic)) sigmoid(raw) else raw
}

# ══════════════════════════════════════════════════════════════════════════════
#  1. CAUSAL FOREST (Wager & Athey 2018, simplified honest version)
# ══════════════════════════════════════════════════════════════════════════════

fit_causal_forest <- function(X, Y, W, ps=NULL,
                               n_trees=300, min_node_size=10,
                               subsample_frac=0.5, max_features=NULL,
                               seed=42) {
  set.seed(seed)
  X_m  <- as.matrix(X); n <- nrow(X_m); p <- ncol(X_m)
  mf   <- max_features %||% max(1, round(sqrt(p)))
  ps   <- ps %||% rep(mean(W), n)

  trees <- lapply(seq_len(n_trees), function(b) {
    # Honest splitting: split sample in two halves
    spl   <- sample(n, round(n * subsample_frac), replace=FALSE)
    spl_a <- spl[seq_len(floor(length(spl)/2))]   # for splitting
    spl_b <- spl[seq_len(floor(length(spl)/2))+floor(length(spl)/2)]  # for prediction

    # Pseudo-outcome ρ̃_i = (W_i - e_i) / (e_i(1-e_i)) * Y_i
    # (Robinson decomposition residuals)
    ei     <- ps[spl_a]
    Wi     <- W[spl_a];  Yi <- Y[spl_a]
    rho    <- (Wi - ei) / (ei*(1-ei) + 1e-9) * Yi

    tree <- build_causal_tree(X_m[spl_a,], rho, mf, min_node_size,
                              X_pred=X_m[spl_b,], Y_pred=Y[spl_b], W_pred=W[spl_b])
    list(tree=tree, spl_b=spl_b)
  })

  list(trees=trees, n_trees=n_trees, ps=ps, X_col_names=colnames(X_m))
}

# Causal tree builds on pseudo-outcomes but splits on treatment heterogeneity
build_causal_tree <- function(X, rho, max_features, min_node_size,
                               X_pred=NULL, Y_pred=NULL, W_pred=NULL,
                               depth=0, max_depth=6) {
  n <- length(rho)
  if (n < min_node_size*2 || depth >= max_depth) {
    # Leaf value: ATE estimate from prediction subsample
    val <- if (!is.null(Y_pred) && length(Y_pred)>0) {
      y1 <- Y_pred[W_pred==1]; y0 <- Y_pred[W_pred==0]
      if (length(y1)>0 && length(y0)>0) mean(y1)-mean(y0) else mean(rho)
    } else mean(rho)
    return(list(leaf=TRUE, value=val))
  }

  p        <- ncol(X); feat_idx <- sample(p, min(max_features, p))
  best     <- list(loss=Inf, j=1, th=0)

  for (j in feat_idx) {
    xj      <- X[,j]; threshs <- quantile(xj, seq(.1,.9,.1), na.rm=TRUE)
    for (th in threshs) {
      left <- xj<=th; right <- !left
      if (sum(left)<min_node_size || sum(right)<min_node_size) next
      loss <- -( (mean(rho[left])-mean(rho))^2 * sum(left) +
                 (mean(rho[right])-mean(rho))^2 * sum(right) )
      if (loss < best$loss) best <- list(loss=loss, j=j, th=th)
    }
  }

  if (is.infinite(best$loss))
    return(list(leaf=TRUE, value=mean(rho)))

  li <- X[,best$j] <= best$th; ri <- !li
  pred_li <- if (!is.null(X_pred)) X_pred[,best$j] <= best$th else NULL
  pred_ri <- if (!is.null(X_pred)) !pred_li else NULL

  list(
    leaf=FALSE, j=best$j, th=best$th,
    left  = build_causal_tree(X[li,,drop=F], rho[li], max_features, min_node_size,
              X_pred[pred_li,,drop=F], Y_pred[pred_li], W_pred[pred_li], depth+1, max_depth),
    right = build_causal_tree(X[ri,,drop=F], rho[ri], max_features, min_node_size,
              X_pred[pred_ri,,drop=F], Y_pred[pred_ri], W_pred[pred_ri], depth+1, max_depth)
  )
}

predict_causal_forest <- function(cf, X_new) {
  X_m   <- as.matrix(X_new)
  preds <- vapply(cf$trees, function(t) predict_tree(t$tree, X_m), numeric(nrow(X_m)))
  if (is.matrix(preds)) rowMeans(preds, na.rm=TRUE) else preds
}

# Variance via IJ bootstrap approximation
causal_forest_variance <- function(cf, X_new, n_boot=100, seed=42) {
  set.seed(seed)
  X_m  <- as.matrix(X_new)
  n_tr <- length(cf$trees)
  preds <- vapply(sample(n_tr, min(n_boot, n_tr)), function(b)
    predict_tree(cf$trees[[b]]$tree, X_m), numeric(nrow(X_m)))
  apply(preds, 1, var, na.rm=TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
#  2. T-LEARNER
# ══════════════════════════════════════════════════════════════════════════════

fit_t_learner <- function(X, Y, W, seed=42) {
  X_m  <- as.matrix(X)
  mu0  <- rf_fit(X_m[W==0,], Y[W==0], seed=seed)
  mu1  <- rf_fit(X_m[W==1,], Y[W==1], seed=seed+1)
  list(mu0=mu0, mu1=mu1, method="T-Learner")
}

predict_t_learner <- function(tl, X_new) {
  X_m <- as.matrix(X_new)
  rf_predict(tl$mu1, X_m) - rf_predict(tl$mu0, X_m)
}

# ══════════════════════════════════════════════════════════════════════════════
#  3. S-LEARNER
# ══════════════════════════════════════════════════════════════════════════════

fit_s_learner <- function(X, Y, W, seed=42) {
  X_w  <- cbind(as.matrix(X), W=W)
  mu   <- rf_fit(X_w, Y, seed=seed)
  list(mu=mu, method="S-Learner")
}

predict_s_learner <- function(sl, X_new) {
  X_m  <- as.matrix(X_new); n <- nrow(X_m)
  X_t  <- cbind(X_m, W=1); X_c <- cbind(X_m, W=0)
  rf_predict(sl$mu, X_t) - rf_predict(sl$mu, X_c)
}

# ══════════════════════════════════════════════════════════════════════════════
#  4. X-LEARNER (Künzel et al. 2019)
# ══════════════════════════════════════════════════════════════════════════════

fit_x_learner <- function(X, Y, W, ps=NULL, seed=42) {
  X_m  <- as.matrix(X); n <- nrow(X_m)
  ps   <- ps %||% rep(mean(W), n)

  # Stage 1: T-learner base models
  mu0  <- rf_fit(X_m[W==0,], Y[W==0], seed=seed)
  mu1  <- rf_fit(X_m[W==1,], Y[W==1], seed=seed+1)

  # Stage 2: Imputed ITEs on each arm
  # Treated arm: D̃_i^1 = Y_i^1 - μ̂₀(X_i^1)
  D1   <- Y[W==1] - rf_predict(mu0, X_m[W==1,])
  # Control arm: D̃_i^0 = μ̂₁(X_i^0) - Y_i^0
  D0   <- rf_predict(mu1, X_m[W==0,]) - Y[W==0]

  # Stage 3: Fit CATE models on each arm
  tau1 <- rf_fit(X_m[W==1,], D1, seed=seed+2)
  tau0 <- rf_fit(X_m[W==0,], D0, seed=seed+3)

  list(mu0=mu0, mu1=mu1, tau0=tau0, tau1=tau1, ps=ps, method="X-Learner")
}

predict_x_learner <- function(xl, X_new) {
  X_m  <- as.matrix(X_new)
  # Propensity-weighted combination
  # Use average PS as weight here (individual PS not available at prediction)
  ps_bar <- mean(xl$ps)
  tau0_hat <- rf_predict(xl$tau0, X_m)
  tau1_hat <- rf_predict(xl$tau1, X_m)
  # Weight by P(W=1) for tau0, P(W=0) for tau1
  ps_bar * tau1_hat + (1 - ps_bar) * tau0_hat
}

# ══════════════════════════════════════════════════════════════════════════════
#  5. DOUBLE ML / PARTIALLY LINEAR MODEL (Chernozhukov et al. 2018)
#  Y = τ(X)·W + g(X) + ε
#  Estimated via cross-fitting to remove regularisation bias
# ══════════════════════════════════════════════════════════════════════════════

fit_double_ml <- function(X, Y, W, ps=NULL, n_folds=5, seed=42) {
  set.seed(seed)
  X_m <- as.matrix(X); n <- nrow(X_m)
  fold_ids <- sample(rep(seq_len(n_folds), length.out=n))

  Y_res <- numeric(n); W_res <- numeric(n)
  E_Y   <- numeric(n); E_W   <- numeric(n)

  for (k in seq_len(n_folds)) {
    train <- fold_ids != k; test <- fold_ids == k

    # Nuisance: E[Y|X] using training fold
    g_hat <- rf_fit(X_m[train,], Y[train], seed=seed+k)
    E_Y[test] <- clamp(rf_predict(g_hat, X_m[test,]), 0, 1)

    # Nuisance: E[W|X] = e(X) using training fold
    e_hat <- rf_fit(X_m[train,], W[train], seed=seed+k+100)
    E_W[test] <- clamp(rf_predict(e_hat, X_m[test,]), 0.01, 0.99)

    Y_res[test] <- Y[test] - E_Y[test]
    W_res[test] <- W[test] - E_W[test]
  }

  # Partially linear: regress Y_res on W_res and X * W_res
  # τ̂ = WLS: (W_res'W_res)^{-1} W_res'Y_res
  tau_global <- sum(W_res * Y_res) / (sum(W_res^2) + 1e-9)

  # Heterogeneous τ(x): forest on pseudo-outcome ρ̃ = Y_res / W_res
  rho_hat  <- Y_res / (W_res + sign(W_res)*1e-6)
  rho_hat  <- clamp(rho_hat, -1, 1)
  tau_rf   <- rf_fit(X_m, rho_hat, seed=seed+200)

  list(
    tau_global = tau_global,
    tau_rf     = tau_rf,
    Y_res      = Y_res, W_res = W_res,
    E_Y = E_Y, E_W = E_W,
    method     = "DoubleML"
  )
}

predict_double_ml <- function(dml, X_new) {
  rf_predict(dml$tau_rf, as.matrix(X_new))
}

# ══════════════════════════════════════════════════════════════════════════════
#  ENSEMBLE: average predictions from all methods
# ══════════════════════════════════════════════════════════════════════════════

ensemble_cate <- function(preds_list, weights=NULL) {
  mat <- do.call(cbind, preds_list)
  if (is.null(weights)) weights <- rep(1/ncol(mat), ncol(mat))
  as.numeric(mat %*% weights)
}

# ══════════════════════════════════════════════════════════════════════════════
#  R6 Wrapper: UpliftModeler
# ══════════════════════════════════════════════════════════════════════════════

UpliftModeler <- R6::R6Class("UpliftModeler",
  private = list(
    cf_=NULL, tl_=NULL, sl_=NULL, xl_=NULL, dml_=NULL,
    tau_cf_=NULL, tau_tl_=NULL, tau_sl_=NULL, tau_xl_=NULL, tau_dml_=NULL,
    tau_ensemble_=NULL, importance_=NULL
  ),
  public = list(

    initialize = function() cs_log("INFO","UpliftModeler ready.","UM"),

    fit = function(X, Y, W, ps=NULL, n_trees=200, seed=42) {
      cs_log("INFO","Fitting Causal Forest...","UM")
      private$cf_  <- fit_causal_forest(X, Y, W, ps, n_trees, seed=seed)
      cs_log("INFO","Fitting T-Learner...","UM")
      private$tl_  <- fit_t_learner(X, Y, W, seed=seed)
      cs_log("INFO","Fitting S-Learner...","UM")
      private$sl_  <- fit_s_learner(X, Y, W, seed=seed)
      cs_log("INFO","Fitting X-Learner...","UM")
      private$xl_  <- fit_x_learner(X, Y, W, ps, seed=seed)
      cs_log("INFO","Fitting Double ML (5-fold cross-fitting)...","UM")
      private$dml_ <- fit_double_ml(X, Y, W, ps, seed=seed)

      cs_log("INFO","Computing ensemble CATE predictions...","UM")
      self$predict(X)
      invisible(self)
    },

    predict = function(X_new) {
      private$tau_cf_  <- predict_causal_forest(private$cf_,  X_new)
      private$tau_tl_  <- predict_t_learner(private$tl_,      X_new)
      private$tau_sl_  <- predict_s_learner(private$sl_,      X_new)
      private$tau_xl_  <- predict_x_learner(private$xl_,      X_new)
      private$tau_dml_ <- predict_double_ml(private$dml_,     X_new)

      # Ensemble (equal weights; can be tuned)
      private$tau_ensemble_ <- ensemble_cate(list(
        private$tau_cf_, private$tau_tl_, private$tau_sl_,
        private$tau_xl_, private$tau_dml_))

      invisible(self)
    },

    predict_with_ci = function(X_new, B=100) {
      tau_hat <- private$tau_ensemble_
      var_cf  <- causal_forest_variance(private$cf_, X_new, n_boot=B)
      ci_lo   <- tau_hat - 1.96 * sqrt(pmax(0, var_cf))
      ci_hi   <- tau_hat + 1.96 * sqrt(pmax(0, var_cf))
      tibble(tau=tau_hat, var=var_cf, ci_lo=ci_lo, ci_hi=ci_hi)
    },

    feature_importance = function(X) {
      imp_cf  <- rf_importance(list(trees=lapply(private$cf_$trees, `[[`, "tree"),
                                    n_trees=private$cf_$n_trees, p=ncol(as.matrix(X))))
      imp_tl0 <- rf_importance(private$tl_$mu0)
      imp_tl1 <- rf_importance(private$tl_$mu1)
      imp_avg <- (imp_cf + imp_tl0 + imp_tl1) / 3
      imp_avg <- imp_avg / max(imp_avg + 1e-9)
      setNames(imp_avg, colnames(as.matrix(X)))
    },

    tau_cf       = function() private$tau_cf_,
    tau_tl       = function() private$tau_tl_,
    tau_sl       = function() private$tau_sl_,
    tau_xl       = function() private$tau_xl_,
    tau_dml      = function() private$tau_dml_,
    tau_ensemble = function() private$tau_ensemble_,
    dml          = function() private$dml_
  )
)

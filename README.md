# Churn Surgeon — Uplift Modelling System

> *"Traditional churn models tell you who is going to leave. This system tells you who is worth saving."*

---

## The Four Response Groups

```
                   Would churn WITHOUT offer?
                        YES           NO
                   ┌──────────────┬──────────────┐
Would churn   YES  │  Lost Cause  │ Sleeping Dog │
WITH offer?        │  (futile)    │ (backfires!) │
                   ├──────────────┼──────────────┤
              NO   │ Persuadable  │  Sure Thing  │
                   │  ✓ TARGET   │  (wasted £)  │
                   └──────────────┴──────────────┘
```

Only **Persuadables** should receive save offers. Sending to Sure Things wastes money; sending to Sleeping Dogs causes harm.

---

## Architecture

```
CUSTOMER DATA (5,000 customers, 5 latent segments, embedded true τ_i)
        ↓
PROPENSITY SCORE ESTIMATION
  e(X) = P(W=1|X)
  Methods: Logistic + Gradient Boosting (ensemble)
  Matching: Nearest-neighbour with Mahalanobis/PS caliper
  Balance diagnostics: |SMD| before/after matching
        ↓
CATE ESTIMATION (5 estimators + ensemble)
  1. Causal Forest (Wager & Athey 2018) — honest RF, Robinson decomposition
  2. T-Learner — separate μ̂₀, μ̂₁ → τ̂ = μ̂₁(x) - μ̂₀(x)
  3. S-Learner — joint model on [X,W] → τ̂ = μ̂(x,1) - μ̂(x,0)
  4. X-Learner (Künzel et al. 2019) — imputed ITEs + PS weighting
  5. Double ML (Chernozhukov et al. 2018) — cross-fitted partial linear
  Ensemble: equal-weighted average
        ↓
EVALUATION
  Qini curves | AUUC | Decile monotonicity | RATE | Calibration
        ↓
ACTION ENGINE
  Classify → rank Persuadables → ROI-optimal targeting
  Budget-constrained campaign brief generation
        ↓
DASHBOARD (6 tabs) + 55+ TESTS + TARGETS PIPELINE
```

---

## Five CATE Estimators

### 1. Causal Forest (Wager & Athey 2018)
- **Honesty**: separate subsamples for tree splitting and for leaf value estimation
- **Splitting criterion**: maximises variance of treatment effects across children (not just Y variance)
- **Pseudo-outcome**: Robinson residual `ρ̃ = (W-e(X))/(e(X)(1-e(X))) × Y`
- **CI**: infinitesimal jackknife variance approximation

### 2. T-Learner
```
τ̂(x) = μ̂₁(x) - μ̂₀(x)
```
Separate random forests for treated/control. Simple but suffers when treatment groups have different distributions.

### 3. S-Learner
```
τ̂(x) = μ̂(x, W=1) - μ̂(x, W=0)
```
Single model with W as a feature. Can underestimate if W is "shrunk" by the regularisation.

### 4. X-Learner (Künzel et al. 2019)
Stage 1: T-learner  
Stage 2: D̃₁ = Y₁ - μ̂₀(X₁), D̃₀ = μ̂₁(X₀) - Y₀  
Stage 3: τ̂ = e(x)·τ̂₁(x) + (1-e(x))·τ̂₀(x)  
Best when sample sizes are very imbalanced across arms.

### 5. Double ML (Chernozhukov et al. 2018)
```
Y - E[Y|X] = τ(x) × (W - e(X)) + ε
```
Cross-fitting with k=5 folds removes regularisation bias. τ global estimated via WLS; heterogeneous τ(x) via forest on pseudo-outcome ρ̃ = Ỹ/W̃.

---

## Evaluation Metrics

| Metric | Description |
|---|---|
| **Qini Curve** | Incremental outcome saves vs % targeted |
| **AUUC** | Area Under Uplift Curve (vs random baseline) |
| **Decile test** | ATE by τ̂ decile — should increase monotonically |
| **RATE** | Rank-weighted ATE (Yadlowsky et al. 2021) |
| **Calibration** | Predicted τ̂ vs observed ATE in bins |
| **Corr(τ̂,τ)** | Pearson correlation with true ITE |

---

## Quick Start

```r
pak::pak(c("R6","tibble","dplyr","tidyr","purrr","ggplot2","glue","stringr",
           "scales","cli","here","Matrix","shiny","bslib","plotly","DT","testthat"))

source("R/agents/churn_surgeon.R")
cs <- run_churn_surgeon(n=5000, seed=42)

# Key outputs
cs$forecast()           # per-customer tau estimates
cs$action()$decisions() # ranked action queue
cs$evaluation()$comparison  # model comparison table

# Plots
cs$plot_qini()
cs$plot_deciles()
cs$plot_response_groups()
cs$plot_tau_distribution()
cs$plot_calibration()
cs$plot_balance()
cs$plot_policy_value()
cs$plot_model_comparison()

# Dashboard
shiny::runApp("dashboard/app.R")

# Tests (55+)
testthat::test_file("tests/test_churn_surgeon.R")

# targets pipeline
targets::tar_make()
```

---

## Project Structure

```
churn/
├── R/
│   ├── utils.R                     ← sigmoid/logit, net_value, four groups, bootstrap CI
│   ├── data/customer_data.R        ← 5-segment DGP with embedded true τ_i
│   ├── models/
│   │   ├── propensity.R            ← PS (logistic + boost + ensemble), matching, IPW/AIPW
│   │   └── uplift_models.R         ← CF, T/S/X-learner, Double ML, ensemble; UpliftModeler R6
│   ├── evaluation/evaluation.R     ← Qini, AUUC, deciles, RATE, calibration, policy_value
│   └── engine/action_engine.R      ← Response group classifier, ActionEngine R6, ROI optimiser
├── R/agents/churn_surgeon.R        ← ChurnSurgeon R6, full pipeline, 8 plots
├── dashboard/app.R                 ← 6-tab Shiny (surgical theatre aesthetic)
├── tests/test_churn_surgeon.R      ← 55+ testthat tests
├── _targets.R
└── README.md
```

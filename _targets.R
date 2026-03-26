# ── _targets.R ───────────────────────────────────────────────────────────────
library(targets); library(tarchetypes)
tar_option_set(packages=c("R6","tibble","dplyr","tidyr","purrr","ggplot2",
  "glue","stringr","scales","cli","here","Matrix"),format="rds",error="continue")
lapply(list.files("R","\\.R$",recursive=TRUE,full.names=TRUE),source)
list(
  tar_target(raw_data,    generate_customers(5000,seed=42)$data),
  tar_target(X_mat,       as.matrix(build_feature_matrix(raw_data))),
  tar_target(Y_vec,       raw_data$churned),
  tar_target(W_vec,       raw_data$treated),
  tar_target(ps_model,    {pe<-PropensityEstimator$new();pe$estimate(X_mat,W_vec);pe$match_samples(as.data.frame(X_mat),W_vec);pe}),
  tar_target(ps_vec,      ps_model$ps()),
  tar_target(uplift_models,{um<-UpliftModeler$new();um$fit(X_mat,Y_vec,W_vec,ps_vec,n_trees=200);um}),
  tar_target(forecast_df, raw_data|>mutate(
    tau_cf=uplift_models$tau_cf(),tau_tl=uplift_models$tau_tl(),
    tau_sl=uplift_models$tau_sl(),tau_xl=uplift_models$tau_xl(),
    tau_dml=uplift_models$tau_dml(),tau_ensemble=uplift_models$tau_ensemble(),
    ps=ps_vec,base_churn_prob=true_y0,
    nv=net_value(tau_ensemble,clv_estimate,offer_cost),
    pred_group=classify_response(tau_ensemble,base_churn_prob))),
  tar_target(eval_results, list(
    comparison=compare_models(list(CF=forecast_df$tau_cf,TL=forecast_df$tau_tl,
      SL=forecast_df$tau_sl,XL=forecast_df$tau_xl,DML=forecast_df$tau_dml,
      Ensemble=forecast_df$tau_ensemble),Y_vec,W_vec,ps_vec,raw_data$true_tau),
    qini=compute_qini(forecast_df$tau_ensemble,Y_vec,W_vec),
    deciles=uplift_by_decile(forecast_df$tau_ensemble,Y_vec,W_vec),
    auuc=compute_auuc(forecast_df$tau_ensemble,Y_vec,W_vec))),
  tar_target(action_results,{ae<-ActionEngine$new();ae$run(forecast_df);ae}),
  tar_quarto(churn_report,"docs/churn_report.qmd",quiet=FALSE)
)

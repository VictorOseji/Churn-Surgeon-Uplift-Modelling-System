# ══════════════════════════════════════════════════════════════════════════════
#  CHURN SURGEON — dashboard/app.R
#  6 Tabs: OR Theatre · Uplift Anatomy · Campaign · Propensity ·
#          Model Comparison · Individual Patient
#  Aesthetic: deep clinical / surgical theatre — sterile whites on near-black
# ══════════════════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr)
  library(purrr); library(ggplot2); library(plotly); library(DT)
  library(glue); library(scales); library(here)
})
lapply(list.files(here("R"), "\\.R$", recursive=TRUE, full.names=TRUE), source)

pt <- function(p, ...) {
  p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)", 
    plot_bgcolor = "#FFFFFF",
    font = list(family = "IBM Plex Mono", color = "#1E293B", size = 10),
    xaxis = list(
      gridcolor = "#E1E5EB", 
      zerolinecolor = "#CBD2DC", 
      linecolor = "#E1E5EB", 
      tickfont = list(size = 9, color = "#64748B")
    ),
    yaxis = list(
      gridcolor = "#E1E5EB", 
      zerolinecolor = "#CBD2DC", 
      linecolor = "#E1E5EB", 
      tickfont = list(size = 9, color = "#64748B")
    ),
    legend = list(
      bgcolor = "rgba(0,0,0,0)", 
      font = list(size = 9, color = "#1E293B")
    ),
    margin = list(l = 52, r = 18, t = 20, b = 48), ...
  ) |>
    config(displayModeBar = FALSE)
}

RG_COLS <- c(Persuadable="#00C060",`Sure Thing`="#1A78C8",
             `Lost Cause`="#E03030",`Sleeping Dog`="#D08020",Marginal="#60888C")

cs <- local({
  message("[CS] Building Churn Surgeon...")
  x <- ChurnSurgeon$new()
  x$run_all(n=3000, seed=42, n_trees=100)
  x
})
fc   <- cs$forecast()
ev   <- cs$evaluation()
ae   <- cs$action()
psm  <- cs$ps_est()
cust_ids <- fc$customer_id

tile <- function(lbl,val,sub,cl){
  div(class=paste("kpi-tile",cl),
      div(class="kl",lbl),
      div(class="kv",val),
      div(class="ks",sub)
      )
}


## ======= USER INPUT INTERFACE ==========================================
ui <- tagList(
  tags$head(includeCSS(here::here("dashboard","www","styling.css"))),
  page_navbar(
    title = tagList(
      tags$span("CHURN SURGEON", style="font-family:'Barlow Condensed',sans-serif;font-size:1.3rem;color:#00E8E8;letter-spacing:.12em;"),
      tags$span("UPLIFT MODELLING SYSTEM",class="brand-sub")
    ),
    window_title="Churn Surgeon",
    theme = bs_theme(fg = "#060A0C", bg = "#B8D0D8", primary = "#00C8C8", bootswatch = NULL),
    fillable=FALSE,

    # ── 1. OR THEATRE ─────────────────────────────────────────────────────────
    nav_panel("OR Theatre",
      div(class="tab-pane",
        div(class="kpi-strip",uiOutput("or_kpis")),
        div(class="g7-5 mb12",
          div(class="cs-card",
            div(class="cs-head","Qini Curve — Incremental Uplift vs Targeting Rate"),
            div(class="cs-body",plotlyOutput("or_qini",height="300px"))
            ),
          div(style="display:flex;flex-direction:column;gap:12px;",
            div(class="cs-card c-green",
              div(class="cs-head hg","Response Group Distribution"),
              div(class="cs-body",plotlyOutput("or_rg_donut",height="190px"))
              ),
            div(class="cs-card c-red",
              div(class="cs-head hr","At-Risk Summary"),
              div(class="cs-body",uiOutput("or_risk_panel"))
              )
          )
        ),
        div(class="g3",
          div(class="cs-card",
            div(class="cs-head","Uplift Score Distribution"),
            div(class="cs-body",plotlyOutput("or_tau_dist",height="190px"))
            ),
          div(class="cs-card c-amber",
            div(class="cs-head ha","Decile Monotonicity Test"),
            div(class="cs-body",plotlyOutput("or_deciles",height="190px"))
            ),
          div(class="cs-card c-blue",
            div(class="cs-head hb","Policy Value Curve"),
            div(class="cs-body",plotlyOutput("or_policy",height="190px"))
            )
        )
      )
    ),

    # ── 2. UPLIFT ANATOMY ─────────────────────────────────────────────────────
    nav_panel("Uplift Anatomy",
      div(class="tab-pane",
        div(class="g2 mb12",
          div(class="cs-card",
            div(class="cs-head","τ̂ vs True τ — Model Calibration"),
            div(class="cs-body",plotlyOutput("ua_calib",height="290px"))),
          div(class="cs-card c-green",
            div(class="cs-head hg","Uplift by True Response Group"),
            div(class="cs-body",plotlyOutput("ua_true_grp",height="290px")))
        ),
        div(class="g2 mb12",
          div(class="cs-card c-amber",
            div(class="cs-head ha","Cumulative Gain Chart"),
            div(class="cs-body",plotlyOutput("ua_cg",height="240px"))),
          div(class="cs-card",
            div(class="cs-head","Feature Importance — Causal Effect"),
            div(class="cs-body",plotlyOutput("ua_importance",height="240px")))
        )
      )
    ),

    # ── 3. CAMPAIGN ROOM ──────────────────────────────────────────────────────
    nav_panel("Campaign",
      div(class="tab-pane",
        div(class="kpi-strip",uiOutput("cmp_kpis")),
        div(class="g7-5 mb12",
          div(class="cs-card",
            div(class="cs-head","Priority Action Queue — Ranked Persuadables"),
            div(class="cs-body",DTOutput("cmp_queue"))),
          div(style="display:flex;flex-direction:column;gap:12px;",
            div(class="cs-card c-green",
              div(class="cs-head hg","Threshold Sensitivity — ROI vs τ cutoff"),
              div(class="cs-body",plotlyOutput("cmp_threshold",height="240px"))),
            div(class="cs-card c-amber",
              div(class="cs-head ha","Budget Controls"),
              div(class="cs-body",
                sliderInput("cmp_budget","Campaign Budget (£):",
                             min=5000,max=200000,value=50000,step=5000,width="100%"),
                sliderInput("cmp_tau_thr","Min τ̂ threshold:",
                             min=.01,max=.30,value=.05,step=.01,width="100%"),
                actionButton("cmp_run","▶ Recompute",class="btn-primary"),
                hr(style="border-color:var(--bdr);margin:10px 0"),
                uiOutput("cmp_live_summary")))
          )
        )
      )
    ),

    # ── 4. PROPENSITY LAB ─────────────────────────────────────────────────────
    nav_panel("Propensity",
      div(class="tab-pane",
        div(class="kpi-strip",uiOutput("ps_kpis")),
        div(class="g2 mb12",
          div(class="cs-card",
            div(class="cs-head","Propensity Score Distribution — Treated vs Control"),
            div(class="cs-body",plotlyOutput("ps_dist",height="280px"))),
          div(class="cs-card c-green",
            div(class="cs-head hg","Covariate Balance — Before vs After Matching"),
            div(class="cs-body",plotlyOutput("ps_balance",height="280px")))
        ),
        div(class="g2",
          div(class="cs-card c-amber",
            div(class="cs-head ha","IPW / AIPW ATE Estimates"),
            div(class="cs-body",plotlyOutput("ps_ate",height="230px"))),
          div(class="cs-card",
            div(class="cs-head","Matching Quality — SMD Improvement"),
            div(class="cs-body",plotlyOutput("ps_smd_improve",height="230px")))
        )
      )
    ),

    # ── 5. MODEL COMPARISON ───────────────────────────────────────────────────
    nav_panel("Models",
      div(class="tab-pane",
        div(class="kpi-strip",uiOutput("mc_kpis")),
        div(class="g2 mb12",
          div(class="cs-card",
            div(class="cs-head","AUUC by Model"),
            div(class="cs-body",plotlyOutput("mc_auuc",height="270px"))),
          div(class="cs-card c-blue",
            div(class="cs-head hb","Qini Curves — All Models"),
            div(class="cs-body",plotlyOutput("mc_qini",height="270px")))
        ),
        div(class="cs-card mb12",
          div(class="cs-head","Full Model Comparison Table"),
          div(class="cs-body",DTOutput("mc_table")))
      )
    ),

    # ── 6. PATIENT RECORD ─────────────────────────────────────────────────────
    nav_panel("Patient",
      div(class="tab-pane",
        div(class="g5-7",
          div(style="display:flex;flex-direction:column;gap:12px;",
            div(class="cs-card",
              div(class="cs-head","Patient Lookup"),
              div(class="cs-body",
                selectInput("pat_id","Customer ID:",cust_ids,width="100%"),
                uiOutput("pat_profile"))),
            div(class="cs-card c-green",
              div(class="cs-head hg","Uplift Score & CI"),
              div(class="cs-body",plotlyOutput("pat_gauge",height="195px"))),
            div(class="cs-card c-amber",
              div(class="cs-head ha","Recommended Action"),
              div(class="cs-body",uiOutput("pat_action")))
          ),
          div(style="display:flex;flex-direction:column;gap:12px;",
            div(class="cs-card",
              div(class="cs-head","Model Predictions — All Estimators"),
              div(class="cs-body",plotlyOutput("pat_models",height="270px"))),
            div(class="cs-card c-blue",
              div(class="cs-head hb","Churn Probability — With vs Without Offer"),
              div(class="cs-body",plotlyOutput("pat_counterfact",height="240px"))),
            div(class="cs-card",
              div(class="cs-head","Similar Persuadable Customers"),
              div(class="cs-body",DTOutput("pat_similar")))
          )
        )
      )
    )
  )
)

## ===== SERVER LOGIC INTERFACE ====================================
server <- function(input, output, session) {

  # ─── 1. OR THEATRE ──────────────────────────────────────────────────────────
  output$or_kpis <- renderUI({
    s <- ae$summary()
    div(class="kpi-strip",
      tile("Persuadables", f_n(s$n_persuadables), f_pct(s$n_persuadables/s$n_total),"kc"),
      tile("Sure Things",  f_n(s$n_sure_things),  "do NOT send offer","kb"),
      tile("Lost Causes",  f_n(s$n_lost_causes),  "futile","kr"),
      tile("Sleeping Dogs",f_n(s$n_sleeping_dogs),"treatment backfires","ka"),
      tile("Campaign ROI", paste0(round(s$roi,2),"×"),"net value / cost","kg"))
  })

  output$or_qini <- renderPlotly({
    q <- ev$qini
    plot_ly(q,x=~pct_targeted)|>
      add_ribbons(ymin=~qini_random,ymax=~qini,fillcolor="rgba(0,192,96,.12)",
        line=list(color="transparent"),name="Uplift area")|>
      add_lines(y=~qini,name="Model",line=list(color="#00E070",width=2.5))|>
      add_lines(y=~qini_random,name="Random",line=list(color="#E03030",width=1,dash="dash"))|>
    pt()|>layout(xaxis=list(title="% customers targeted",tickformat=".0%"),
                 yaxis=list(title="Incremental saves"),
                 legend=list(orientation="h",y=-.25))
  })

  output$or_rg_donut <- renderPlotly({
    d <- count(fc,pred_group)
    cols <- map_chr(d$pred_group,~RG_COLS[.x]%||%"#888")
    plot_ly(d,labels=~pred_group,values=~n,type="pie",hole=.5,
      marker=list(colors=cols,line=list(color="rgba(0,0,0,0)",width=0)),
      textfont=list(size=9,family="IBM Plex Mono"))|>
    pt()|>layout(legend=list(orientation="v"),margin=list(t=5,b=5))
  })

  output$or_risk_panel <- renderUI({
    s <- ae$summary(); persuad <- filter(ae$decisions(),response_grp=="Persuadable")
    div(style="font-family:'IBM Plex Mono',mono;font-size:.76rem;",
      div(style="color:#00E070;font-size:1.1rem;margin-bottom:6px;",
          f_n(s$n_persuadables)," persuadables found"),
      div(glue("Mean τ̂ (Persuadable): {round(mean(persuad$tau_ensemble,na.rm=T),4)}")),
      div(glue("Expected saves/1000 targeted: {round(mean(persuad$tau_ensemble,na.rm=T)*1000,0)}")),
      div(style="color:#60888C;margin-top:4px;",
        glue("AUUC lift: {round(ev$auuc$qini_lift,4)}"))
    )
  })

  output$or_tau_dist <- renderPlotly({
    
    color <- RG_COLS[unique(fc$pred_group)]
    
    plot_ly(fc, x=~tau_ensemble, color=~pred_group, colors=color, type="histogram",
            nbins=50, opacity=.75)|>
      add_lines(x = c(.05,.05),y = c(0,300),
                line=list(color="#00C8C8",dash="dash",width=1.5),
                showlegend=FALSE) |>
      pt() |>
      layout(barmode="overlay",
             xaxis = list(title="Estimated ITE τ̂")  ,
             yaxis = list(title="Count"),
             legend = list(orientation="h",y=-.3))
  })

  output$or_deciles <- renderPlotly({
    d <- ev$deciles
    plot_ly(d,x=~decile,y=~ate_observed,type="bar",
      marker=list(color=~ate_observed,colorscale=list(c(0,"#E03030"),c(.5,"#D08020"),c(1,"#00C060")),
                  line=list(color="rgba(0,0,0,0)")))|>
      add_lines(x=d$decile,y=d$mean_tau_hat,name="τ̂",line=list(color="#60888C",dash="dash",width=1.5))|>
      add_markers(data=filter(d,!monotone_ok),x=~decile,y=~ate_observed,
        marker=list(color="#E03030",size=10,symbol="x"),name="Non-monotone")|>
    pt()|>layout(xaxis=list(title="Uplift decile"),yaxis=list(title="Observed ATE"),showlegend=FALSE)
  })

  output$or_policy <- renderPlotly({
    pv <- ev$policy_curve
    plot_ly(pv,x=~pct_targeted)|>
      add_lines(y=~cum_nv/1e3,name="Cum NV (£k)",line=list(color="#00E070",width=2))|>
      add_lines(y=~cum_cost/1e3*(-1),name="Cum cost",line=list(color="#E03030",width=1,dash="dot"))|>
    pt()|>layout(xaxis=list(title="% targeted",tickformat=".0%"),
                 yaxis=list(title="£k"),legend=list(orientation="h",y=-.3))
  })

  # ─── 2. UPLIFT ANATOMY ──────────────────────────────────────────────────────
  output$ua_calib <- renderPlotly({
    cl <- ev$calibration
    plot_ly(cl,x=~mean_predicted,y=~mean_true,size=~n,type="scatter",mode="markers",
      marker=list(sizemode="diameter",sizeref=1,color="#00C8C8",opacity=.8))|>
      add_lines(x=c(min(cl$mean_predicted),max(cl$mean_predicted)),
        y=c(min(cl$mean_predicted),max(cl$mean_predicted)),
        line=list(color="#60888C",dash="dash",width=1.5),showlegend=FALSE)|>
    pt()|>layout(xaxis=list(title="Mean predicted τ̂"),yaxis=list(title="Mean true τ"))
  })

  output$ua_true_grp <- renderPlotly({
    d <- fc|>group_by(true_group)|>
      summarise(mean_pred=mean(tau_ensemble,na.rm=T),mean_true=mean(true_tau,na.rm=T),.groups="drop")
    plot_ly(d,x=~reorder(true_group,-mean_true))|>
      add_bars(y=~mean_true,name="True τ",marker=list(color="rgba(0,192,96,.6)"))|>
      add_bars(y=~mean_pred,name="Predicted τ̂",marker=list(color="rgba(0,200,200,.8)"))|>
    pt()|>layout(barmode="group",xaxis=list(title=""),yaxis=list(title="Mean treatment effect"),
                 legend=list(orientation="h",y=-.25))
  })

  output$ua_cg <- renderPlotly({
    cg <- compute_cumulative_gain(fc$tau_ensemble,fc$churned,fc$treated)
    plot_ly(cg,x=~pct_targeted)|>
      add_lines(y=~gain,name="Model",line=list(color="#00E070",width=2))|>
      add_lines(y=~gain_random,name="Random",line=list(color="#E03030",dash="dash",width=1))|>
    pt()|>layout(xaxis=list(title="% targeted",tickformat=".0%"),
                 yaxis=list(title="ATE in targeted group"),legend=list(orientation="h",y=-.3))
  })

  output$ua_importance <- renderPlotly({
    imp <- sort(cs$uplift()$feature_importance(build_feature_matrix(fc)),decreasing=TRUE)[1:15]
    d   <- tibble(feature=names(imp),importance=as.numeric(imp))
    plot_ly(d,x=~importance,y=~reorder(feature,importance),type="bar",orientation="h",
      marker=list(color=~importance,colorscale=list(c(0,"#172530"),c(1,"#00C8C8")),
                  line=list(color="rgba(0,0,0,0)")))|>
    pt()|>layout(xaxis=list(title="Relative importance"),yaxis=list(title=""))
  })

  # ─── 3. CAMPAIGN ────────────────────────────────────────────────────────────
  ae_reactive <- eventReactive(c(input$cmp_run,1),{
    de <- ActionEngine$new()
    de$run(cs$forecast(), input$cmp_budget, input$cmp_tau_thr)
    de
  },ignoreNULL=FALSE)

  output$cmp_kpis <- renderUI({
    ae2 <- ae_reactive()$summary()
    div(class="kpi-strip",
      tile("Targeted",    f_n(ae2$n_targeted),   "customers with offer","kc"),
      tile("Total Cost",  f_gbp(ae2$total_cost),  "campaign spend","ka"),
      tile("Net Value",   f_gbp(ae2$total_nv),    "retained CLV","kg"),
      tile("ROI",         paste0(round(ae2$roi,2),"×"),"net value / cost","kc"),
      tile("Mean τ̂",     round(ae2$mean_tau_targeted,4),"targeted group","kb"))
  })

  output$cmp_queue <- renderDT({
    ae_reactive()$target_list()|>
      mutate(tau_ensemble=round(tau_ensemble,4),
             clv_estimate=paste0("£",round(clv_estimate)),
             nv=paste0("£",round(nv)),
             offer_cost=paste0("£",round(offer_cost,2)))|>
      select(ID=customer_id,Segment=segment,`τ̂`=tau_ensemble,
             CLV=clv_estimate,NV=nv,Cost=offer_cost,
             Group=response_grp,Offer=offer_name)|>
      datatable(options=list(dom="ft",pageLength=12,scrollY="280px"),rownames=FALSE)|>
      formatStyle("Group",backgroundColor=styleEqual(
        c("Persuadable","Sure Thing","Lost Cause","Sleeping Dog","Marginal"),
        c("rgba(0,192,96,.18)","rgba(26,120,200,.18)","rgba(224,48,48,.18)","rgba(208,128,32,.18)","rgba(96,136,140,.18)")))
  },server=FALSE)

  output$cmp_threshold <- renderPlotly({
    ts <- ae$sensitivity()
    opt<- ae$optimal_threshold()$threshold[1]
    plot_ly(ts,x=~threshold)|>
      add_lines(y=~roi,name="ROI",line=list(color="#00E070",width=2.5))|>
      add_lines(x=c(opt,opt),y=c(0,max(ts$roi,na.rm=T)),
        line=list(color="#00C8C8",dash="dash",width=1.5),name=paste0("Optimal τ=",opt))|>
    pt()|>layout(xaxis=list(title="τ̂ threshold"),yaxis=list(title="Campaign ROI"),
                 legend=list(orientation="h",y=-.3))
  })

  output$cmp_live_summary <- renderUI({
    ae2 <- ae_reactive()$summary()
    div(style="font-family:'IBM Plex Mono',mono;font-size:.74rem;",
      div(glue("Targeted: {f_n(ae2$n_targeted)}")),
      div(glue("Cost: {f_gbp(ae2$total_cost)}")),
      div(style="color:#00E070;",glue("NV: {f_gbp(ae2$total_nv)}")),
      div(style="color:#00C8C8;",glue("ROI: {round(ae2$roi,2)}×"))
    )
  })

  # ─── 4. PROPENSITY ──────────────────────────────────────────────────────────
  output$ps_kpis <- renderUI({
    m  <- psm$match_obj(); b <- psm$balance()
    div(class="kpi-strip",
      tile("Matched Pairs", f_n(m$n_matched),     "PS matched","kc"),
      tile("% Treated Matched",f_pct(m$pct_matched),"coverage","kb"),
      tile("Avg |SMD| Before",round(mean(abs(b$smd_before),na.rm=T),3),"imbalance","kr"),
      tile("Avg |SMD| After", round(mean(abs(b$smd_after),na.rm=T),3),"post-match","kg"),
      tile("Balance Improved",paste0(sum(b$balance_improved,na.rm=T),"/",nrow(b)),"features","kc"))
  })

  output$ps_dist <- renderPlotly({
    d <- tibble(ps=psm$ps(),W=fc$treated)|>mutate(group=if_else(W==1,"Treated","Control"))
    plot_ly(d,x=~ps,color=~group,colors=c(Treated="#00C8C8",Control="#E03030"),
      type="histogram",nbinsx=40,opacity=.7)|>
    pt()|>layout(barmode="overlay",xaxis=list(title="Propensity score e(X)"),yaxis=list(title="Count"),
                 legend=list(orientation="h",y=-.25))
  })

  output$ps_balance <- renderPlotly({
    b <- psm$balance()|>arrange(desc(abs(smd_before)))|>head(20)
    plot_ly(b,y=~reorder(feature,abs(smd_before)))|>
      add_markers(x=~smd_before,name="Before",marker=list(color="#E03030",size=7))|>
      add_markers(x=~smd_after, name="After", marker=list(color="#00E070",size=7))|>
      add_lines(x=c(-.10,-.10),y=c(0,nrow(b)+1),line=list(color="#60888C",dash="dash",width=.8))|>
      add_lines(x=c( .10, .10),y=c(0,nrow(b)+1),line=list(color="#60888C",dash="dash",width=.8))|>
    pt()|>layout(xaxis=list(title="Standardised Mean Difference"),yaxis=list(title=""),
                 legend=list(orientation="h",y=-.25))
  })

  output$ps_ate <- renderPlotly({
    ipw_res <- psm$ipw_ate(fc$churned,fc$treated)
    mat_res <- psm$matched_ate(fc$churned,fc$treated)
    d<-tibble(
      estimator=c("IPW","Matched","True ATE"),
      tau=c(ipw_res$tau,mat_res$tau,mean(fc$true_tau,na.rm=T)),
      ci_lo=c(ipw_res$ci[1],mat_res$ci[1],NA_real_),
      ci_hi=c(ipw_res$ci[2],mat_res$ci[2],NA_real_)
    )
    plot_ly(d,x=~estimator,y=~tau,type="bar",
      error_y=list(type="data",symmetric=FALSE,
                   array=d$ci_hi-d$tau,arrayminus=d$tau-d$ci_lo,color="#60888C"),
      marker=list(color=c("#00C8C8","#00E070","#E03030"),line=list(color="rgba(0,0,0,0)")))|>
    pt()|>layout(xaxis=list(title=""),yaxis=list(title="ATE estimate"))
  })

  output$ps_smd_improve <- renderPlotly({
    b <- psm$balance()
    plot_ly(b,x=~smd_before,y=~smd_after,text=~feature,
      type="scatter",mode="markers",
      marker=list(color=~as.integer(balance_improved),
        colorscale=list(c(0,"#E03030"),c(1,"#00E070")),size=8,opacity=.8),
      hovertemplate="%{text}<br>Before: %{x:.3f}<br>After: %{y:.3f}<extra></extra>")|>
      add_lines(x=c(-1,1),y=c(-1,1),line=list(color="#60888C",dash="dash",width=.8))|>
    pt()|>layout(xaxis=list(title="|SMD| before"),yaxis=list(title="|SMD| after"),showlegend=FALSE)
  })

  # ─── 5. MODEL COMPARISON ────────────────────────────────────────────────────
  output$mc_kpis <- renderUI({
    comp <- ev$comparison; best<-comp$model[1]
    div(class="kpi-strip",
      tile("Best Model",   best,"highest AUUC lift","kc"),
      tile("Best AUUC",    round(comp$qini_lift[1],4),"ensemble recommended","kg"),
      tile("Best RATE",    round(max(comp$rate,na.rm=T),4),"rank-weighted ATE","kb"),
      tile("Best Corr(τ)", round(max(comp$corr_true_tau,na.rm=T),3),"vs true ITE","kc"),
      tile("Models Fit",   nrow(comp),"estimators compared","ka"))
  })

  output$mc_auuc <- renderPlotly({
    comp <- ev$comparison
    plot_ly(comp,x=~model,y=~qini_lift,type="bar",
      marker=list(color=~qini_lift,colorscale=list(c(0,"#172530"),c(1,"#00E070")),
                  line=list(color="rgba(0,0,0,0)")))|>
    pt()|>layout(xaxis=list(title=""),yaxis=list(title="AUUC Qini lift"))
  })

  output$mc_qini <- renderPlotly({
    model_list <- list(
      `Causal Forest`=fc$tau_cf,`T-Learner`=fc$tau_tl,`S-Learner`=fc$tau_sl,
      `X-Learner`=fc$tau_xl,`Double ML`=fc$tau_dml,Ensemble=fc$tau_ensemble)
    COLS<-c(`Causal Forest`="#00C8C8",`T-Learner`="#00E070",`S-Learner`="#1A78C8",
            `X-Learner`="#D08020",`Double ML`="#E03030",Ensemble="#F0F0F0")
    p <- plot_ly()
    for(nm in names(model_list)){
      q <- compute_qini(model_list[[nm]],fc$churned,fc$treated)
      p <- add_lines(p,data=q,x=~pct_targeted,y=~qini,name=nm,
                     line=list(color=COLS[nm],width=if(nm=="Ensemble")2.5 else 1.2))
    }
    p|>pt()|>layout(xaxis=list(title="% targeted",tickformat=".0%"),
                    yaxis=list(title="Qini"),legend=list(orientation="h",y=-.25))
  })

  output$mc_table <- renderDT({
    ev$comparison|>
      mutate(across(where(is.numeric),~round(.,4)))|>
      select(Model=model,`AUUC`=auuc,`Qini Lift`=qini_lift,RATE=rate,
             Monotonicity=monotonicity,`ATE D10`=ate_d10,`ATE D1`=ate_d1,
             `Lift Ratio`=lift_ratio,`Corr(τ̂,τ)`=corr_true_tau)|>
      datatable(options=list(dom="t",pageLength=10),rownames=FALSE)|>
      formatStyle("Model",backgroundColor=styleEqual("Ensemble","rgba(0,200,200,.10)"))
  },server=FALSE)

  # ─── 6. PATIENT ─────────────────────────────────────────────────────────────
  sel <- reactive({ filter(fc,customer_id==input$pat_id)[1,] })
  sel_de <- reactive({ filter(ae$decisions(),customer_id==input$pat_id)[1,] })

  output$pat_profile <- renderUI({
    r <- sel(); de <- sel_de()
    col <- RG_COLS[de$response_grp[1]]%||%"#888"
    div(style="font-family:'IBM Plex Mono',mono;font-size:.74rem;",
      div(style=paste0("color:",col,";font-size:1.1rem;font-family:'Barlow Condensed';margin-bottom:7px;"),
          paste0("■ ",de$response_grp[1])),
      div(glue("Segment: {r$segment[1]}  |  Plan: {r$plan[1]}")),
      div(glue("Tenure: {r$tenure_months[1]}mo | Spend: £{r$monthly_spend[1]}/mo")),
      div(glue("Logins 30d: {r$login_30d[1]} | NPS: {r$nps_score[1]}")),
      tags$hr(style="border-color:var(--bdr);margin:6px 0"),
      div(glue("τ̂ (ensemble): {round(r$tau_ensemble[1],4)}")),
      div(glue("True τ:       {round(r$true_tau[1],4)}")),
      div(glue("P(churn|ctrl):{round(r$true_y0[1],3)} | P(churn|trt):{round(r$true_y1[1],3)}")),
      div(style="color:#00E070;",glue("Estimated CLV: £{round(r$clv_estimate[1])}")))
  })

  output$pat_gauge <- renderPlotly({
    r <- sel(); tau <- r$tau_ensemble[1]%||%0
    col <- if(tau>.10)"#00E070" else if(tau>.02)"#D08020" else "#E03030"
    plot_ly(type="indicator",mode="gauge+number+delta",value=tau,
      number=list(font=list(size=30,color=col,family="Barlow Condensed"),valueformat=".4f"),
      delta=list(reference=r$true_tau[1]%||%tau,valueformat=".4f"),
      gauge=list(axis=list(range=c(-.15,.40),tickcolor="#172530"),
        bar=list(color=col),
        steps=list(list(range=c(-.15,0),color="rgba(224,48,48,.18)"),
                   list(range=c(0,.05),color="rgba(208,128,32,.18)"),
                   list(range=c(.05,.40),color="rgba(0,192,96,.18)")),
        threshold=list(line=list(color="#B8D0D8",width=2),value=r$true_tau[1]%||%tau)))|>
    pt()|>layout(margin=list(t=15,b=5,l=15,r=15))
  })

  output$pat_models <- renderPlotly({
    r <- sel()
    d <- tibble(
      model=c("Causal Forest","T-Learner","S-Learner","X-Learner","Double ML","Ensemble","True τ"),
      tau  =c(r$tau_cf,r$tau_tl,r$tau_sl,r$tau_xl,r$tau_dml,r$tau_ensemble,r$true_tau)
    )
    cols<-c(`Causal Forest`="#00C8C8",`T-Learner`="#00E070",`S-Learner`="#1A78C8",
            `X-Learner`="#D08020",`Double ML`="#E03030",Ensemble="#F0F0F0",`True τ`="#60888C")
    plot_ly(d,x=~model,y=~tau,type="bar",
      marker=list(color=map_chr(d$model,~cols[.x]%||%"#888"),
                  line=list(color="rgba(0,0,0,0)")))|>
      add_lines(x=c(1,7),y=c(0,0),line=list(color="#60888C",dash="dash",width=.8))|>
    pt()|>layout(xaxis=list(title=""),yaxis=list(title="τ̂ estimate"),showlegend=FALSE)
  })

  output$pat_action <- renderUI({
    de <- sel_de()
    if (!nrow(de)) return(div("Not found."))
    col <- if(de$eligible[1])"#00E070" else "#E03030"
    action_txt <- if(de$eligible[1]) de$offer_name[1] else "No intervention — not persuadable"
    div(style="font-family:'IBM Plex Mono',mono;font-size:.74rem;",
      div(style=paste0("color:",col,";font-size:1.0rem;font-family:'Barlow Condensed';margin-bottom:8px;"),
          action_txt),
      div(glue("Net value: £{round(de$nv[1],0)}")),
      div(glue("Priority ROI: {round(de$roi[1],2)}×")),
      div(style=if(de$eligible[1])"color:#00E070;" else "color:#E03030;",
        if(de$eligible[1]) "✓ Send offer" else "✗ Do not contact")
    )
  })

  output$pat_counterfact <- renderPlotly({
    r <- sel()
    d <- tibble(
      scenario=c("Without offer","With offer"),
      prob=c(r$true_y0[1]%||%.5, r$true_y1[1]%||%.4)
    )
    plot_ly(d,x=~scenario,y=~prob,type="bar",
      marker=list(color=c("#E03030","#00E070"),line=list(color="rgba(0,0,0,0)")),
      text=~paste0(round(prob*100,1),"%"),textposition="outside")|>
    pt()|>layout(xaxis=list(title=""),yaxis=list(title="P(churn)",range=c(0,1.15),tickformat=".0%"))
  })

  output$pat_similar <- renderDT({
    r <- sel()
    fc|>filter(pred_group=="Persuadable",customer_id!=r$customer_id[1])|>
      mutate(dist=abs(tau_ensemble-r$tau_ensemble[1]))|>
      arrange(dist)|>slice_head(n=8)|>
      mutate(tau_ensemble=round(tau_ensemble,4),clv_estimate=paste0("£",round(clv_estimate)))|>
      select(ID=customer_id,Segment=segment,`τ̂`=tau_ensemble,CLV=clv_estimate,Plan=plan)|>
      datatable(options=list(dom="t",pageLength=8),rownames=FALSE)
  },server=FALSE)
}

shinyApp(ui,server)

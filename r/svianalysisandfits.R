wrds <- connect()

source("~/dev/wrds/run_surface_analysis.R")

res1 <- analyze_single_expiry(
  wrds = wrds,
  ticker = "SPY",
  trade_date = "2024-01-05",
  expiry = "2024-01-19"
)


fit0 <- res1$fits[["2024-01-19"]]

plots <- plot_fit_bundle(
  fit_obj   = fit0,
  vendor_df = res1$vendor,
  tau_days  = 14,
  outdir    = "~/dev/wrds/output/SPY_20240105/plots",
  prefix    = "SPY_20240119"
)

plots$iv_fit_k
plots$iv_fit_strike
plots$vendor_vs_fit_strike

plots <- plot_fit_bundle_from_csv(
  fit_csv    = "~/dev/wrds/output/SPY_20240105/data/fit_df_20240119.csv",
  vendor_csv = "~/dev/wrds/output/SPY_20240105/data/vendor_surface.csv",
  tau_days   = 14,
  outdir     = "~/dev/wrds/output/SPY_20240105/plots",
  prefix     = "SPY_20240119"
)

res2 <- analyze_vol_surface(
  wrds = wrds,
  ticker = "SPY",
  trade_date = "2024-01-05",
  expiries = c("2024-01-19", "2024-02-16", "2024-03-15")
)

res3 <- analyze_expiry_range(
  wrds = wrds,
  ticker = "SPY",
  trade_date = "2024-01-05",
  expiry_start = "2024-01-19",
  expiry_end = "2024-06-21"
)

res4 <- analyze_all_expiries(
  wrds = wrds,
  ticker = "SPY",
  trade_date = "2024-01-05"
)



source("~/dev/wrds/svitermstructureplots.R")

ts_out <- plot_term_structure_bundle(
  diag_df = res4$diagnostics,
  outdir  = "~/dev/wrds/output/SPY_20240105/plots",
  prefix  = "SPY_20240105"
)

ts_out$plots$atm_iv
ts_out$plots$atm_skew
ts_out$plots$wing_slopes
ts_out$plots$curvature


source("~/dev/wrds/svisurfaceplots.R")

surf_out <- plot_surface_bundle(
  fits   = res4$fits,
  outdir = "~/dev/wrds/output/SPY_20240105/plots",
  prefix = "SPY_20240105"
)

surf_out$plots$iv_heatmap
surf_out$plots$totalvar_heatmap
surf_out$plots$iv_strike_heatmap

source("~/dev/wrds/svisurfaceplots.R")

surf_out <- plot_surface_bundle(
  fits   = res4$fits,
  outdir = "~/dev/wrds/output/SPY_20240105/plots",
  prefix = "SPY_20240105"
)

surf_out$plots$iv_heatmap
surf_out$plots$totalvar_heatmap
surf_out$plots$iv_strike_heatmap


source("~/dev/wrds/sviparamtermstructure.R")

param_out <- plot_param_bundle(
  fits   = res4$fits,
  outdir = "~/dev/wrds/output/SPY_20240105/plots",
  prefix = "SPY_20240105"
)

param_out$plots$a
param_out$plots$rho
param_out$plots$sigma

source("~/dev/wrds/svisummarytable.R")

summary_tbl <- build_surface_summary_table(
  diag_df = res4$diagnostics,
  param_df = param_out$params
)

summary_tbl

save_surface_summary_table(
  summary_tbl,
  "~/dev/wrds/output/SPY_20240105/data/SPY_20240105_surface_summary.csv"
)

ggplot(res4$diagnostics,
       aes(x = tau_days, y = rmse_total_var)) +
  geom_point() +
  geom_line() +
  labs(
    title = "SVI fit error vs maturity",
    x = "maturity (days)",
    y = "RMSE (total variance)"
  ) +
  theme_minimal()

#!/usr/bin/env Rscript

# ============================================================
# svisummarytable.R
#
# Purpose:
#   Build a clean summary table for fitted SVI smiles.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

build_surface_summary_table <- function(diag_df, param_df = NULL) {
  base <- diag_df %>%
    select(
      ticker, trade_date, exdate, tau_days,
      atm_iv, atm_slope, atm_curvature,
      left_wing_slope, right_wing_slope,
      rmse_total_var, mae_total_var, n_points
    ) %>%
    arrange(tau_days)
  
  if (!is.null(param_df)) {
    base <- base %>%
      left_join(
        param_df %>% select(exdate, a, b, rho, m, sigma),
        by = "exdate"
      )
  }
  
  base
}

save_surface_summary_table <- function(summary_df, out_csv) {
  readr::write_csv(summary_df, out_csv)
  invisible(out_csv)
}
#!/usr/bin/env Rscript

# ============================================================
# svitermstructureplots.R
#
# Purpose:
#   Plot SVI smile diagnostics across maturities.
#
# What this file does:
#   1. Builds a clean term-structure table from SVI diagnostics
#   2. Plots ATM volatility vs maturity
#   3. Plots ATM skew vs maturity
#   4. Plots left/right wing slopes vs maturity
#   5. Plots curvature vs maturity
#   6. Saves a bundle of term-structure plots
#
# Expected input:
#   diagnostics table produced by analyze_vol_surface(...)
#
# Typical usage:
#   source("~/dev/wrds/svitermstructureplots.R")
#   plots <- plot_term_structure_bundle(
#       diag_df = res4$diagnostics,
#       outdir  = "~/dev/wrds/output/SPY_20240105/plots",
#       prefix  = "SPY_20240105"
#   )
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

# ------------------------------------------------------------
# Small utilities
# ------------------------------------------------------------

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

.ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.safe_ggsave <- function(filename, plot, width = 8, height = 5, dpi = 150) {
  ggplot2::ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
}

# ------------------------------------------------------------
# Data preparation
#
# Builds a clean maturity-level table from the diagnostics output.
# tau_days and tau_years are included for plotting.
# ------------------------------------------------------------

prepare_term_structure_data <- function(diag_df) {
  req <- c(
    "ticker", "trade_date", "exdate", "tau_days",
    "atm_iv", "atm_slope", "atm_curvature",
    "left_wing_slope", "right_wing_slope",
    "rmse_total_var", "mae_total_var", "n_points"
  )
  
  missing_cols <- setdiff(req, names(diag_df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Diagnostics table is missing columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  diag_df %>%
    mutate(
      tau_years = tau_days / 365.0
    ) %>%
    arrange(tau_days)
}

# ------------------------------------------------------------
# ATM volatility term structure
#
# This plot shows how the ATM implied volatility evolves with
# maturity. It is one of the most standard volatility desk plots.
# ------------------------------------------------------------

plot_atm_iv_term_structure <- function(ts_df, title = NULL) {
  ggplot(ts_df, aes(x = tau_days, y = atm_iv)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "ATM volatility term structure",
      x = "maturity (days)",
      y = "ATM implied volatility"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# ATM skew term structure
#
# The ATM slope is the first derivative of the SVI total variance
# smile at k = 0. For equity indices it is typically negative.
# ------------------------------------------------------------

plot_atm_skew_term_structure <- function(ts_df, title = NULL) {
  ggplot(ts_df, aes(x = tau_days, y = atm_slope)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "ATM skew term structure",
      x = "maturity (days)",
      y = "ATM skew (dw/dk at k=0)"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Wing slopes term structure
#
# These asymptotic slopes summarize the left and right wings of
# the smile. For equities, the left wing is usually steeper.
# ------------------------------------------------------------

plot_wing_slopes_term_structure <- function(ts_df, title = NULL) {
  df <- bind_rows(
    ts_df %>%
      transmute(
        tau_days = tau_days,
        slope = left_wing_slope,
        wing = "left"
      ),
    ts_df %>%
      transmute(
        tau_days = tau_days,
        slope = right_wing_slope,
        wing = "right"
      )
  )
  
  ggplot(df, aes(x = tau_days, y = slope, linetype = wing, shape = wing)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "Wing slopes term structure",
      x = "maturity (days)",
      y = "wing slope"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Curvature term structure
#
# This shows how smile convexity evolves across maturities.
# ------------------------------------------------------------

plot_curvature_term_structure <- function(ts_df, title = NULL) {
  ggplot(ts_df, aes(x = tau_days, y = atm_curvature)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "ATM curvature term structure",
      x = "maturity (days)",
      y = "ATM curvature"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Fit quality diagnostics
#
# These plots show how calibration error changes across maturities.
# ------------------------------------------------------------

plot_rmse_term_structure <- function(ts_df, title = NULL) {
  ggplot(ts_df, aes(x = tau_days, y = rmse_total_var)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "SVI fit RMSE across maturities",
      x = "maturity (days)",
      y = "RMSE in total variance"
    ) +
    theme_minimal()
}

plot_points_term_structure <- function(ts_df, title = NULL) {
  ggplot(ts_df, aes(x = tau_days, y = n_points)) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% "Number of fitted points by maturity",
      x = "maturity (days)",
      y = "number of OTM points"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Bundle orchestration
#
# Builds all standard term-structure plots and optionally saves
# them to disk.
# ------------------------------------------------------------

plot_term_structure_bundle <- function(diag_df,
                                       outdir = NULL,
                                       prefix = "term_structure",
                                       save_plots = TRUE) {
  ts_df <- prepare_term_structure_data(diag_df)
  
  if (!is.null(outdir)) {
    .ensure_dir(outdir)
  }
  
  plots <- list(
    atm_iv        = plot_atm_iv_term_structure(ts_df),
    atm_skew      = plot_atm_skew_term_structure(ts_df),
    wing_slopes   = plot_wing_slopes_term_structure(ts_df),
    curvature     = plot_curvature_term_structure(ts_df),
    rmse          = plot_rmse_term_structure(ts_df),
    fitted_points = plot_points_term_structure(ts_df)
  )
  
  if (save_plots && !is.null(outdir)) {
    for (nm in names(plots)) {
      .safe_ggsave(
        filename = file.path(outdir, paste0(prefix, "_", nm, ".png")),
        plot = plots[[nm]]
      )
    }
    readr::write_csv(ts_df, file.path(outdir, paste0(prefix, "_table.csv")))
  }
  
  list(
    term_structure = ts_df,
    plots = plots
  )
}
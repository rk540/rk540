#!/usr/bin/env Rscript

# ============================================================
# sviplottools.R
#
# Purpose:
#   Visualization utilities for SVI smile fitting and diagnostics.
#
# What this file does:
#   1. Builds a smooth fitted SVI curve from calibrated parameters
#   2. Plots market implied vols against fitted SVI vols
#   3. Plots total variance fit diagnostics
#   4. Plots residuals
#   5. Overlays vendor surface vs market vs fitted SVI
#   6. Saves a bundle of plots for one expiry
#
# Main expected inputs:
#   - fit_obj : output of fit_svi_expiry(...)
#   - fit_df  : fit_obj$fit_df
#   - vendor_df : output of build_vendor_surface_dataset(...)
#
# Required columns in fit_df:
#   date, exdate, K, k, F, T, iv, total_var, iv_fit, total_var_fit
#
# Required functions available elsewhere:
#   svi_raw(...)
#
# Typical usage:
#   source("~/dev/wrds/sviplottools.R")
#   plots <- plot_fit_bundle(
#       fit_obj   = fit0,
#       vendor_df = spy_vsurf,
#       tau_days  = 14,
#       outdir    = "~/dev/wrds/output/SPY_20240105/plots",
#       prefix    = "SPY_20240119"
#   )
#
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

# ------------------------------------------------------------
# Small utilities
#
# These are lightweight helpers for:
#   - directory creation
#   - safe plot saving
#   - null fallback
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
# Core fitted-curve construction
#
# These functions generate a smooth SVI curve from the calibrated
# parameters rather than drawing lines through noisy market rows.
#
# This is important because plotting geom_line() directly on fit_df
# can create a noisy zig-zag if the rows are not ordered or if
# multiple quotes share nearby strikes.
# ------------------------------------------------------------

make_svi_curve <- function(fit_obj, n = 400) {
  p <- fit_obj$par
  df <- fit_obj$fit_df
  
  k_grid <- seq(
    min(df$k, na.rm = TRUE),
    max(df$k, na.rm = TRUE),
    length.out = n
  )
  
  T0 <- unique(df$T)[1]
  F0 <- unique(df$F)[1]
  
  total_var_fit <- svi_raw(
    k_grid,
    p["a"], p["b"], p["rho"], p["m"], p["sigma"]
  )
  
  iv_fit <- sqrt(pmax(total_var_fit / T0, 0))
  K_grid <- F0 * exp(k_grid)
  
  data.frame(
    k = k_grid,
    K = K_grid,
    total_var_fit = total_var_fit,
    iv_fit = iv_fit
  )
}

# ------------------------------------------------------------
# Basic fit plots: implied volatility
#
# These plots show market implied vols as points and the smooth
# SVI fit as a single line.
#
# Two coordinate systems are provided:
#   - log-moneyness space
#   - strike space
# ------------------------------------------------------------

plot_iv_fit_k <- function(fit_obj, title = NULL) {
  pts <- fit_obj$fit_df
  ln  <- make_svi_curve(fit_obj)
  
  ggplot() +
    geom_point(data = pts, aes(x = k, y = iv), alpha = 0.7) +
    geom_line(data = ln, aes(x = k, y = iv_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Implied vol vs SVI fit | expiry", unique(pts$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "implied volatility"
    ) +
    theme_minimal()
}

plot_iv_fit_strike <- function(fit_obj, title = NULL) {
  pts <- fit_obj$fit_df
  ln  <- make_svi_curve(fit_obj)
  
  ggplot() +
    geom_point(data = pts, aes(x = K, y = iv), alpha = 0.7) +
    geom_line(data = ln, aes(x = K, y = iv_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Implied vol vs SVI fit by strike | expiry", unique(pts$exdate)),
      x = "strike",
      y = "implied volatility"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Basic fit plots: total variance
#
# Since SVI is calibrated in total variance space, these are
# often the most important fit diagnostics.
# ------------------------------------------------------------

plot_totalvar_fit_k <- function(fit_obj, title = NULL) {
  pts <- fit_obj$fit_df
  ln  <- make_svi_curve(fit_obj)
  
  ggplot() +
    geom_point(data = pts, aes(x = k, y = total_var), alpha = 0.7) +
    geom_line(data = ln, aes(x = k, y = total_var_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Total variance vs SVI fit | expiry", unique(pts$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "total implied variance"
    ) +
    theme_minimal()
}

plot_totalvar_fit_strike <- function(fit_obj, title = NULL) {
  pts <- fit_obj$fit_df
  ln  <- make_svi_curve(fit_obj)
  
  ggplot() +
    geom_point(data = pts, aes(x = K, y = total_var), alpha = 0.7) +
    geom_line(data = ln, aes(x = K, y = total_var_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Total variance vs SVI fit by strike | expiry", unique(pts$exdate)),
      x = "strike",
      y = "total implied variance"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Residual diagnostics
#
# These plots show where the fitted smile is over- or under-
# estimating the market data.
#
# Residuals are useful for spotting:
#   - wing misfit
#   - local smile distortions
#   - noisy or illiquid quotes
# ------------------------------------------------------------

plot_iv_residuals_k <- function(fit_obj, title = NULL) {
  df <- fit_obj$fit_df %>%
    mutate(iv_resid = iv - iv_fit)
  
  ggplot(df, aes(x = k, y = iv_resid)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    labs(
      title = title %||% paste("IV residuals | expiry", unique(df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "iv - iv_fit"
    ) +
    theme_minimal()
}

plot_totalvar_residuals_k <- function(fit_obj, title = NULL) {
  df <- fit_obj$fit_df %>%
    mutate(tv_resid = total_var - total_var_fit)
  
  ggplot(df, aes(x = k, y = tv_resid)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    labs(
      title = title %||% paste("Total variance residuals | expiry", unique(df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "total_var - total_var_fit"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Vendor comparison plots
#
# These functions compare:
#   - market implied vols
#   - smooth SVI fitted curve
#   - vendor surface values
#
# Vendor input is expected to already be cleaned, with columns:
#   days, iv, impl_strike
# ------------------------------------------------------------

plot_vendor_vs_fit_strike <- function(fit_obj, vendor_df, tau_days, title = NULL) {
  pts <- fit_obj$fit_df
  ln  <- make_svi_curve(fit_obj)
  
  df_mkt <- pts %>%
    transmute(
      x = K,
      y = iv,
      source = "Market IV"
    )
  
  df_fit <- ln %>%
    transmute(
      x = K,
      y = iv_fit,
      source = "SVI fit"
    )
  
  df_vendor <- vendor_df %>%
    filter(days == tau_days) %>%
    transmute(
      x = impl_strike,
      y = iv,
      source = "Vendor"
    )
  
  df_all <- bind_rows(df_mkt, df_fit, df_vendor)
  
  ggplot(df_all, aes(x = x, y = y, linetype = source, shape = source)) +
    geom_point(data = subset(df_all, source == "Market IV"), alpha = 0.65) +
    geom_line(data = subset(df_all, source != "Market IV"), linewidth = 0.9) +
    labs(
      title = title %||% paste("Market vs SVI vs Vendor | tau_days =", tau_days),
      x = "strike",
      y = "implied volatility"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Fit-summary plot bundle
#
# This is the main orchestration function for plotting.
#
# It builds a set of standard plots for one fitted expiry and
# optionally saves them to disk.
#
# Returned object:
#   list of ggplot objects
# ------------------------------------------------------------

plot_fit_bundle <- function(fit_obj,
                            vendor_df = NULL,
                            tau_days = NULL,
                            outdir = NULL,
                            prefix = NULL,
                            save_plots = TRUE) {
  if (is.null(prefix)) {
    prefix <- paste0("fit_", gsub("-", "", as.character(unique(fit_obj$fit_df$exdate)[1])))
  }
  
  if (!is.null(outdir)) {
    .ensure_dir(outdir)
  }
  
  plots <- list(
    iv_fit_k             = plot_iv_fit_k(fit_obj),
    iv_fit_strike        = plot_iv_fit_strike(fit_obj),
    totalvar_fit_k       = plot_totalvar_fit_k(fit_obj),
    totalvar_fit_strike  = plot_totalvar_fit_strike(fit_obj),
    iv_residuals_k       = plot_iv_residuals_k(fit_obj),
    totalvar_residuals_k = plot_totalvar_residuals_k(fit_obj)
  )
  
  if (!is.null(vendor_df) && !is.null(tau_days)) {
    plots$vendor_vs_fit_strike <- plot_vendor_vs_fit_strike(
      fit_obj = fit_obj,
      vendor_df = vendor_df,
      tau_days = tau_days
    )
  }
  
  if (save_plots && !is.null(outdir)) {
    for (nm in names(plots)) {
      .safe_ggsave(
        filename = file.path(outdir, paste0(prefix, "_", nm, ".png")),
        plot = plots[[nm]]
      )
    }
  }
  
  plots
}

# ------------------------------------------------------------
# CSV-based workflow
#
# This allows you to regenerate plots later from saved CSV output
# without rerunning the calibration step.
#
# Expected CSV:
#   fit_df_YYYYMMDD.csv or similar
#
# This version plots observed values and fitted values directly
# from the saved fit_df table.
# ------------------------------------------------------------

plot_fit_bundle_from_csv <- function(fit_csv,
                                     vendor_csv = NULL,
                                     tau_days = NULL,
                                     outdir = NULL,
                                     prefix = NULL,
                                     save_plots = TRUE) {
  fit_df <- readr::read_csv(fit_csv, show_col_types = FALSE)
  
  required_cols <- c("date", "exdate", "K", "k", "T", "F", "iv", "total_var", "iv_fit", "total_var_fit")
  missing_cols <- setdiff(required_cols, names(fit_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("fit_csv is missing columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Rebuild a pseudo-fit object for compatibility
  # This requires fitted parameters only for smooth curve generation.
  # If parameters are not stored, we fall back to sorted fitted rows.
  pseudo_fit_obj <- list(
    par = NULL,
    fit_df = fit_df
  )
  
  # If no fitted parameters are available, use sorted rows directly.
  make_curve_from_rows <- function(df) {
    df %>%
      arrange(k) %>%
      select(k, K, total_var_fit, iv_fit)
  }
  
  plot_iv_fit_k_rows <- function(df, title = NULL) {
    pts <- df
    ln  <- make_curve_from_rows(df)
    ggplot() +
      geom_point(data = pts, aes(x = k, y = iv), alpha = 0.7) +
      geom_line(data = ln, aes(x = k, y = iv_fit), linewidth = 0.9) +
      labs(
        title = title %||% paste("Implied vol vs fitted curve | expiry", unique(df$exdate)),
        x = "log-moneyness k = log(K/F)",
        y = "implied volatility"
      ) +
      theme_minimal()
  }
  
  plot_iv_fit_strike_rows <- function(df, title = NULL) {
    pts <- df
    ln  <- df %>% arrange(K)
    ggplot() +
      geom_point(data = pts, aes(x = K, y = iv), alpha = 0.7) +
      geom_line(data = ln, aes(x = K, y = iv_fit), linewidth = 0.9) +
      labs(
        title = title %||% paste("Implied vol vs fitted curve by strike | expiry", unique(df$exdate)),
        x = "strike",
        y = "implied volatility"
      ) +
      theme_minimal()
  }
  
  plot_totalvar_fit_k_rows <- function(df, title = NULL) {
    pts <- df
    ln  <- df %>% arrange(k)
    ggplot() +
      geom_point(data = pts, aes(x = k, y = total_var), alpha = 0.7) +
      geom_line(data = ln, aes(x = k, y = total_var_fit), linewidth = 0.9) +
      labs(
        title = title %||% paste("Total variance vs fitted curve | expiry", unique(df$exdate)),
        x = "log-moneyness k = log(K/F)",
        y = "total implied variance"
      ) +
      theme_minimal()
  }
  
  plot_totalvar_fit_strike_rows <- function(df, title = NULL) {
    pts <- df
    ln  <- df %>% arrange(K)
    ggplot() +
      geom_point(data = pts, aes(x = K, y = total_var), alpha = 0.7) +
      geom_line(data = ln, aes(x = K, y = total_var_fit), linewidth = 0.9) +
      labs(
        title = title %||% paste("Total variance vs fitted curve by strike | expiry", unique(df$exdate)),
        x = "strike",
        y = "total implied variance"
      ) +
      theme_minimal()
  }
  
  plot_iv_residuals_rows <- function(df) {
    ggplot(df %>% mutate(iv_resid = iv - iv_fit), aes(x = k, y = iv_resid)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      labs(
        title = paste("IV residuals | expiry", unique(df$exdate)),
        x = "log-moneyness k = log(K/F)",
        y = "iv - iv_fit"
      ) +
      theme_minimal()
  }
  
  plot_totalvar_residuals_rows <- function(df) {
    ggplot(df %>% mutate(tv_resid = total_var - total_var_fit), aes(x = k, y = tv_resid)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      labs(
        title = paste("Total variance residuals | expiry", unique(df$exdate)),
        x = "log-moneyness k = log(K/F)",
        y = "total_var - total_var_fit"
      ) +
      theme_minimal()
  }
  
  plots <- list(
    iv_fit_k             = plot_iv_fit_k_rows(fit_df),
    iv_fit_strike        = plot_iv_fit_strike_rows(fit_df),
    totalvar_fit_k       = plot_totalvar_fit_k_rows(fit_df),
    totalvar_fit_strike  = plot_totalvar_fit_strike_rows(fit_df),
    iv_residuals_k       = plot_iv_residuals_rows(fit_df),
    totalvar_residuals_k = plot_totalvar_residuals_rows(fit_df)
  )
  
  if (!is.null(vendor_csv) && !is.null(tau_days)) {
    vendor_df <- readr::read_csv(vendor_csv, show_col_types = FALSE)
    plots$vendor_vs_fit_strike <- plot_vendor_vs_fit_strike(
      fit_obj = pseudo_fit_obj,
      vendor_df = vendor_df,
      tau_days = tau_days
    )
  }
  
  if (!is.null(outdir)) {
    .ensure_dir(outdir)
  }
  
  if (save_plots && !is.null(outdir)) {
    if (is.null(prefix)) {
      prefix <- paste0("fit_", gsub("-", "", as.character(unique(fit_df$exdate)[1])))
    }
    for (nm in names(plots)) {
      .safe_ggsave(
        filename = file.path(outdir, paste0(prefix, "_", nm, ".png")),
        plot = plots[[nm]]
      )
    }
  }
  
  plots
}
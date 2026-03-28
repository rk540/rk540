#!/usr/bin/env Rscript

# ============================================================
# svisurfaceplots.R
#
# Purpose:
#   Build and visualize a full volatility surface from fitted
#   SVI slices across maturities.
#
# What this file does:
#   1. Evaluates fitted SVI slices on a common k-grid
#   2. Constructs a cross-maturity surface table
#   3. Plots heatmaps for implied volatility and total variance
#   4. Plots strike-maturity heatmap
#   5. Optionally creates base-R perspective plots
#
# Expected input:
#   fits list from analyze_vol_surface(...)
#
# Typical usage:
#   source("~/dev/wrds/svisurfaceplots.R")
#   surf_out <- plot_surface_bundle(
#       fits   = res4$fits,
#       outdir = "~/dev/wrds/output/SPY_20240105/plots",
#       prefix = "SPY_20240105"
#   )
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

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
# Evaluate one fitted SVI slice on a common k-grid
# ------------------------------------------------------------

evaluate_svi_slice <- function(fit_obj, k_grid = seq(-0.25, 0.10, length.out = 200)) {
  p <- fit_obj$par
  df <- fit_obj$fit_df
  
  T0 <- unique(df$T)[1]
  F0 <- unique(df$F)[1]
  ex0 <- unique(df$exdate)[1]
  dt0 <- unique(df$date)[1]
  
  total_var <- svi_raw(
    k_grid,
    p["a"], p["b"], p["rho"], p["m"], p["sigma"]
  )
  
  iv <- sqrt(pmax(total_var / T0, 0))
  K <- F0 * exp(k_grid)
  
  tibble::tibble(
    date = as.Date(dt0),
    exdate = as.Date(ex0),
    T = T0,
    tau_days = as.integer(round(T0 * 365)),
    F = F0,
    k = k_grid,
    K = K,
    total_var = total_var,
    iv = iv
  )
}

# ------------------------------------------------------------
# Build full surface table from all fitted expiries
# ------------------------------------------------------------

build_svi_surface_table <- function(fits,
                                    k_grid = seq(-0.25, 0.10, length.out = 200)) {
  if (length(fits) == 0) {
    stop("No fitted expiries supplied.")
  }
  
  pieces <- lapply(fits, evaluate_svi_slice, k_grid = k_grid)
  dplyr::bind_rows(pieces) %>%
    arrange(T, k)
}

# ------------------------------------------------------------
# Heatmaps in (k, T)
# ------------------------------------------------------------

plot_iv_surface_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = k, y = tau_days, fill = iv)) +
    geom_tile() +
    labs(
      title = title %||% "Implied volatility surface heatmap",
      x = "log-moneyness k = log(K/F)",
      y = "maturity (days)",
      fill = "IV"
    ) +
    theme_minimal()
}

plot_totalvar_surface_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = k, y = tau_days, fill = total_var)) +
    geom_tile() +
    labs(
      title = title %||% "Total variance surface heatmap",
      x = "log-moneyness k = log(K/F)",
      y = "maturity (days)",
      fill = "Total var"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Heatmap in (K, T)
# ------------------------------------------------------------

plot_iv_surface_strike_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = K, y = tau_days, fill = iv)) +
    geom_tile() +
    labs(
      title = title %||% "Implied volatility surface in strike-maturity space",
      x = "strike",
      y = "maturity (days)",
      fill = "IV"
    ) +
    theme_minimal()
}

# ------------------------------------------------------------
# Base-R perspective plots
# ------------------------------------------------------------

make_surface_matrix <- function(surface_df, value_col = "iv") {
  k_vals <- sort(unique(surface_df$k))
  t_vals <- sort(unique(surface_df$tau_days))
  
  z <- matrix(NA_real_, nrow = length(k_vals), ncol = length(t_vals))
  
  for (i in seq_along(k_vals)) {
    for (j in seq_along(t_vals)) {
      tmp <- surface_df[surface_df$k == k_vals[i] & surface_df$tau_days == t_vals[j], value_col]
      if (length(tmp) == 1) z[i, j] <- tmp
    }
  }
  
  list(k_vals = k_vals, t_vals = t_vals, z = z)
}

plot_iv_surface_persp <- function(surface_df,
                                  theta = 35, phi = 25,
                                  main = "Implied volatility surface") {
  sm <- make_surface_matrix(surface_df, value_col = "iv")
  persp(
    x = sm$k_vals,
    y = sm$t_vals,
    z = sm$z,
    theta = theta,
    phi = phi,
    expand = 0.6,
    xlab = "k",
    ylab = "days",
    zlab = "IV",
    main = main
  )
}

plot_totalvar_surface_persp <- function(surface_df,
                                        theta = 35, phi = 25,
                                        main = "Total variance surface") {
  sm <- make_surface_matrix(surface_df, value_col = "total_var")
  persp(
    x = sm$k_vals,
    y = sm$t_vals,
    z = sm$z,
    theta = theta,
    phi = phi,
    expand = 0.6,
    xlab = "k",
    ylab = "days",
    zlab = "Total var",
    main = main
  )
}

# ------------------------------------------------------------
# Bundle orchestration
# ------------------------------------------------------------

plot_surface_bundle <- function(fits,
                                k_grid = seq(-0.25, 0.10, length.out = 200),
                                outdir = NULL,
                                prefix = "svi_surface",
                                save_plots = TRUE,
                                save_table = TRUE) {
  surface_df <- build_svi_surface_table(fits, k_grid = k_grid)
  
  if (!is.null(outdir)) {
    .ensure_dir(outdir)
  }
  
  plots <- list(
    iv_heatmap = plot_iv_surface_heatmap(surface_df),
    totalvar_heatmap = plot_totalvar_surface_heatmap(surface_df),
    iv_strike_heatmap = plot_iv_surface_strike_heatmap(surface_df)
  )
  
  if (save_plots && !is.null(outdir)) {
    .safe_ggsave(file.path(outdir, paste0(prefix, "_iv_heatmap.png")), plots$iv_heatmap, width = 9, height = 6)
    .safe_ggsave(file.path(outdir, paste0(prefix, "_totalvar_heatmap.png")), plots$totalvar_heatmap, width = 9, height = 6)
    .safe_ggsave(file.path(outdir, paste0(prefix, "_iv_strike_heatmap.png")), plots$iv_strike_heatmap, width = 9, height = 6)
  }
  
  if (save_table && !is.null(outdir)) {
    readr::write_csv(surface_df, file.path(outdir, paste0(prefix, "_table.csv")))
  }
  
  list(
    surface = surface_df,
    plots = plots
  )
}


#!/usr/bin/env Rscript

# ============================================================
# svisurfaceplots.R
#
# Purpose:
#   Build and visualize a full volatility surface from fitted
#   SVI slices across maturities.
#
# What this file does:
#   1. Evaluates fitted SVI slices on a common k-grid
#   2. Builds a surface table across maturities
#   3. Plots implied vol heatmap in (k, T)
#   4. Plots total variance heatmap in (k, T)
#   5. Plots implied vol heatmap in (K, T)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

.ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.safe_ggsave <- function(filename, plot, width = 9, height = 6, dpi = 150) {
  ggplot2::ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
}

evaluate_svi_slice <- function(fit_obj, k_grid = seq(-0.25, 0.10, length.out = 200)) {
  p <- fit_obj$par
  df <- fit_obj$fit_df
  
  T0 <- unique(df$T)[1]
  F0 <- unique(df$F)[1]
  ex0 <- unique(df$exdate)[1]
  dt0 <- unique(df$date)[1]
  
  total_var <- svi_raw(
    k_grid,
    p["a"], p["b"], p["rho"], p["m"], p["sigma"]
  )
  
  iv <- sqrt(pmax(total_var / T0, 0))
  K <- F0 * exp(k_grid)
  
  tibble::tibble(
    date = as.Date(dt0),
    exdate = as.Date(ex0),
    T = T0,
    tau_days = as.integer(round(T0 * 365)),
    F = F0,
    k = k_grid,
    K = K,
    total_var = total_var,
    iv = iv
  )
}

build_svi_surface_table <- function(fits,
                                    k_grid = seq(-0.25, 0.10, length.out = 200)) {
  if (length(fits) == 0) stop("No fitted expiries supplied.")
  
  pieces <- lapply(fits, evaluate_svi_slice, k_grid = k_grid)
  dplyr::bind_rows(pieces) %>%
    arrange(tau_days, k)
}

plot_iv_surface_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = k, y = tau_days, fill = iv)) +
    geom_tile() +
    labs(
      title = title %||% "SVI implied volatility surface",
      x = "log-moneyness k = log(K/F)",
      y = "maturity (days)",
      fill = "IV"
    ) +
    theme_minimal()
}

plot_totalvar_surface_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = k, y = tau_days, fill = total_var)) +
    geom_tile() +
    labs(
      title = title %||% "SVI total variance surface",
      x = "log-moneyness k = log(K/F)",
      y = "maturity (days)",
      fill = "Total var"
    ) +
    theme_minimal()
}

plot_iv_surface_strike_heatmap <- function(surface_df, title = NULL) {
  ggplot(surface_df, aes(x = K, y = tau_days, fill = iv)) +
    geom_tile() +
    labs(
      title = title %||% "SVI implied volatility surface in strike-maturity space",
      x = "strike",
      y = "maturity (days)",
      fill = "IV"
    ) +
    theme_minimal()
}

plot_surface_bundle <- function(fits,
                                k_grid = seq(-0.25, 0.10, length.out = 200),
                                outdir = NULL,
                                prefix = "svi_surface",
                                save_plots = TRUE,
                                save_table = TRUE) {
  surface_df <- build_svi_surface_table(fits, k_grid = k_grid)
  
  if (!is.null(outdir)) .ensure_dir(outdir)
  
  plots <- list(
    iv_heatmap = plot_iv_surface_heatmap(surface_df),
    totalvar_heatmap = plot_totalvar_surface_heatmap(surface_df),
    iv_strike_heatmap = plot_iv_surface_strike_heatmap(surface_df)
  )
  
  if (save_plots && !is.null(outdir)) {
    .safe_ggsave(file.path(outdir, paste0(prefix, "_iv_heatmap.png")), plots$iv_heatmap)
    .safe_ggsave(file.path(outdir, paste0(prefix, "_totalvar_heatmap.png")), plots$totalvar_heatmap)
    .safe_ggsave(file.path(outdir, paste0(prefix, "_iv_strike_heatmap.png")), plots$iv_strike_heatmap)
  }
  
  if (save_table && !is.null(outdir)) {
    readr::write_csv(surface_df, file.path(outdir, paste0(prefix, "_table.csv")))
  }
  
  list(
    surface = surface_df,
    plots = plots
  )
}
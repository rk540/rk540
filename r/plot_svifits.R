#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

# ============================================================
# Helpers
# ============================================================

.ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.safe_ggsave <- function(filename, plot, width = 8, height = 5, dpi = 150) {
  ggplot2::ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
}

# ============================================================
# Core plotting functions
# fit_df is expected to contain at least:
#   date, exdate, K, k, T, iv, total_var, iv_fit, total_var_fit
# ============================================================

plot_iv_fit_k <- function(fit_df, title = NULL) {
  ggplot(fit_df, aes(x = k)) +
    geom_point(aes(y = iv), alpha = 0.7) +
    geom_line(aes(y = iv_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Implied vol vs SVI fit | expiry", unique(fit_df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "implied volatility"
    ) +
    theme_minimal()
}

plot_totalvar_fit_k <- function(fit_df, title = NULL) {
  ggplot(fit_df, aes(x = k)) +
    geom_point(aes(y = total_var), alpha = 0.7) +
    geom_line(aes(y = total_var_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Total variance vs SVI fit | expiry", unique(fit_df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "total implied variance"
    ) +
    theme_minimal()
}

plot_iv_fit_strike <- function(fit_df, title = NULL) {
  ggplot(fit_df, aes(x = K)) +
    geom_point(aes(y = iv), alpha = 0.7) +
    geom_line(aes(y = iv_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Implied vol vs SVI fit by strike | expiry", unique(fit_df$exdate)),
      x = "strike",
      y = "implied volatility"
    ) +
    theme_minimal()
}

plot_totalvar_fit_strike <- function(fit_df, title = NULL) {
  ggplot(fit_df, aes(x = K)) +
    geom_point(aes(y = total_var), alpha = 0.7) +
    geom_line(aes(y = total_var_fit), linewidth = 0.9) +
    labs(
      title = title %||% paste("Total variance vs SVI fit by strike | expiry", unique(fit_df$exdate)),
      x = "strike",
      y = "total implied variance"
    ) +
    theme_minimal()
}

plot_iv_residuals_k <- function(fit_df, title = NULL) {
  df <- fit_df %>%
    mutate(iv_resid = iv - iv_fit)
  
  ggplot(df, aes(x = k, y = iv_resid)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    labs(
      title = title %||% paste("IV residuals | expiry", unique(fit_df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "iv - iv_fit"
    ) +
    theme_minimal()
}

plot_totalvar_residuals_k <- function(fit_df, title = NULL) {
  df <- fit_df %>%
    mutate(tv_resid = total_var - total_var_fit)
  
  ggplot(df, aes(x = k, y = tv_resid)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    labs(
      title = title %||% paste("Total variance residuals | expiry", unique(fit_df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "total_var - total_var_fit"
    ) +
    theme_minimal()
}

# ============================================================
# Vendor overlay
# vendor_df expected to contain:
#   days, iv, impl_strike
# ============================================================

plot_vendor_vs_fit_strike <- function(fit_df, vendor_df, tau_days, title = NULL) {
  df_fit <- fit_df %>%
    transmute(
      x = K,
      y = iv_fit,
      source = "SVI fit"
    )
  
  df_mkt <- fit_df %>%
    transmute(
      x = K,
      y = iv,
      source = "Market IV"
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

# ============================================================
# Driver functions
# ============================================================

plot_fit_bundle <- function(fit_df,
                            outdir = NULL,
                            prefix = NULL,
                            vendor_df = NULL,
                            tau_days = NULL,
                            save_plots = TRUE) {
  if (is.null(prefix)) {
    prefix <- paste0("fit_", gsub("-", "", as.character(unique(fit_df$exdate)[1])))
  }
  
  if (!is.null(outdir)) {
    .ensure_dir(outdir)
  }
  
  p1 <- plot_iv_fit_k(fit_df)
  p2 <- plot_totalvar_fit_k(fit_df)
  p3 <- plot_iv_fit_strike(fit_df)
  p4 <- plot_totalvar_fit_strike(fit_df)
  p5 <- plot_iv_residuals_k(fit_df)
  p6 <- plot_totalvar_residuals_k(fit_df)
  
  plots <- list(
    iv_fit_k = p1,
    totalvar_fit_k = p2,
    iv_fit_strike = p3,
    totalvar_fit_strike = p4,
    iv_residuals_k = p5,
    totalvar_residuals_k = p6
  )
  
  if (!is.null(vendor_df) && !is.null(tau_days)) {
    plots$vendor_vs_fit_strike <- plot_vendor_vs_fit_strike(
      fit_df = fit_df,
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

plot_fit_bundle_from_csv <- function(fit_csv,
                                     outdir = NULL,
                                     prefix = NULL,
                                     vendor_csv = NULL,
                                     tau_days = NULL,
                                     save_plots = TRUE) {
  fit_df <- readr::read_csv(fit_csv, show_col_types = FALSE)
  
  vendor_df <- NULL
  if (!is.null(vendor_csv)) {
    vendor_df <- readr::read_csv(vendor_csv, show_col_types = FALSE)
  }
  
  plot_fit_bundle(
    fit_df = fit_df,
    outdir = outdir,
    prefix = prefix,
    vendor_df = vendor_df,
    tau_days = tau_days,
    save_plots = save_plots
  )
}

# ============================================================
# small infix helper
# ============================================================

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
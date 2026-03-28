#!/usr/bin/env Rscript

# ============================================================
# sviparamtermstructure.R
#
# Purpose:
#   Extract and plot SVI parameter term structures.
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

.safe_ggsave <- function(filename, plot, width = 8, height = 5, dpi = 150) {
  ggplot2::ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
}

build_svi_parameter_table <- function(fits) {
  if (length(fits) == 0) stop("No fitted expiries supplied.")
  
  rows <- lapply(fits, function(fit_obj) {
    p <- fit_obj$par
    df <- fit_obj$fit_df
    tibble::tibble(
      exdate = unique(df$exdate)[1],
      date = unique(df$date)[1],
      T = unique(df$T)[1],
      tau_days = as.integer(round(unique(df$T)[1] * 365)),
      a = p["a"],
      b = p["b"],
      rho = p["rho"],
      m = p["m"],
      sigma = p["sigma"]
    )
  })
  
  dplyr::bind_rows(rows) %>%
    arrange(tau_days)
}

plot_param_term_structure <- function(param_df, param_name, title = NULL) {
  ggplot(param_df, aes(x = tau_days, y = .data[[param_name]])) +
    geom_point() +
    geom_line() +
    labs(
      title = title %||% paste("SVI parameter term structure:", param_name),
      x = "maturity (days)",
      y = param_name
    ) +
    theme_minimal()
}

plot_param_bundle <- function(fits,
                              outdir = NULL,
                              prefix = "svi_params",
                              save_plots = TRUE,
                              save_table = TRUE) {
  param_df <- build_svi_parameter_table(fits)
  
  if (!is.null(outdir)) .ensure_dir(outdir)
  
  plots <- list(
    a = plot_param_term_structure(param_df, "a"),
    b = plot_param_term_structure(param_df, "b"),
    rho = plot_param_term_structure(param_df, "rho"),
    m = plot_param_term_structure(param_df, "m"),
    sigma = plot_param_term_structure(param_df, "sigma")
  )
  
  if (save_plots && !is.null(outdir)) {
    for (nm in names(plots)) {
      .safe_ggsave(file.path(outdir, paste0(prefix, "_", nm, ".png")), plots[[nm]])
    }
  }
  
  if (save_table && !is.null(outdir)) {
    readr::write_csv(param_df, file.path(outdir, paste0(prefix, "_table.csv")))
  }
  
  list(
    params = param_df,
    plots = plots
  )
}
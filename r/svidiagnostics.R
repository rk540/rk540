svi_diagnostics <- function(fit_obj) {
  p <- fit_obj$par
  df <- fit_obj$fit_df
  
  # choose k closest to ATM
  k_atm <- 0
  
  atm_total_var <- svi_raw(k_atm, p["a"], p["b"], p["rho"], p["m"], p["sigma"])
  atm_slope <- svi_raw_prime(k_atm, p["a"], p["b"], p["rho"], p["m"], p["sigma"])
  atm_curvature <- svi_raw_second(k_atm, p["a"], p["b"], p["rho"], p["m"], p["sigma"])
  
  # asymptotic wing slopes in total variance
  left_wing_slope  <- p["b"] * (p["rho"] - 1)
  right_wing_slope <- p["b"] * (p["rho"] + 1)
  
  resid <- df$total_var - df$total_var_fit
  
  tibble::tibble(
    a = p["a"],
    b = p["b"],
    rho = p["rho"],
    m = p["m"],
    sigma = p["sigma"],
    atm_total_var = atm_total_var,
    atm_iv = sqrt(pmax(atm_total_var / unique(df$T)[1], 0)),
    atm_slope = atm_slope,
    atm_curvature = atm_curvature,
    left_wing_slope = left_wing_slope,
    right_wing_slope = right_wing_slope,
    rmse_total_var = sqrt(mean(resid^2, na.rm = TRUE)),
    mae_total_var = mean(abs(resid), na.rm = TRUE),
    n_points = nrow(df),
    convergence = fit_obj$convergence
  )
}

plot_svi_fit <- function(fit_obj) {
  df <- fit_obj$fit_df
  
  ggplot(df, aes(x = k)) +
    geom_point(aes(y = iv), alpha = 0.7) +
    geom_line(aes(y = iv_fit), linewidth = 0.9) +
    labs(
      title = paste("SVI fit for expiry", unique(df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "implied volatility"
    ) +
    theme_minimal()
}

plot_svi_total_variance_fit <- function(fit_obj) {
  df <- fit_obj$fit_df
  
  ggplot(df, aes(x = k)) +
    geom_point(aes(y = total_var), alpha = 0.7) +
    geom_line(aes(y = total_var_fit), linewidth = 0.9) +
    labs(
      title = paste("SVI total variance fit for expiry", unique(df$exdate)),
      x = "log-moneyness k = log(K/F)",
      y = "total implied variance"
    ) +
    theme_minimal()
}


compare_vendor_vs_svi <- function(fit_obj, vendor_df, tau_days) {
  raw_fit <- fit_obj$fit_df %>%
    transmute(
      x = K,
      iv = iv_fit,
      source = "SVI fit"
    )
  
  vendor_slice <- vendor_df %>%
    filter(days == tau_days) %>%
    transmute(
      x = impl_strike,
      iv = iv,
      source = "Vendor"
    )
  
  bind_rows(raw_fit, vendor_slice)
}

plot_vendor_vs_svi <- function(fit_obj, vendor_df, tau_days) {
  df <- compare_vendor_vs_svi(fit_obj, vendor_df, tau_days)
  
  ggplot(df, aes(x = x, y = iv, linetype = source)) +
    geom_line(linewidth = 0.9) +
    labs(
      title = paste("Vendor vs SVI fit | vendor maturity =", tau_days, "days"),
      x = "strike",
      y = "implied volatility"
    ) +
    theme_minimal()
}
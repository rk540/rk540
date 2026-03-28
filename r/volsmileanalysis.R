suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# --------------------------------------------------
# Pick an expiry with enough points
# --------------------------------------------------

get_expiry_slice <- function(smile_df, exdate, cp_flag = NULL) {
  out <- smile_df %>%
    filter(exdate == as.Date(exdate))
  
  if (!is.null(cp_flag)) {
    out <- out %>% filter(cp_flag == cp_flag)
  }
  
  out %>% arrange(k, K)
}

# safer version avoiding name collision
get_expiry_slice <- function(smile_df, exdate, opt_type = NULL) {
  out <- smile_df %>%
    filter(exdate == as.Date(exdate))
  
  if (!is.null(opt_type)) {
    out <- out %>% filter(cp_flag == opt_type)
  }
  
  out %>% arrange(k, K)
}

# --------------------------------------------------
# Plot raw smile by expiry
# --------------------------------------------------

plot_raw_smile <- function(smile_df, exdate, opt_type = NULL) {
  df <- get_expiry_slice(smile_df, exdate, opt_type = opt_type)
  
  ggplot(df, aes(x = k, y = iv, shape = cp_flag)) +
    geom_point(alpha = 0.7) +
    labs(
      title = paste("Raw implied vol smile:", exdate),
      x = "log-moneyness k = log(K/F)",
      y = "implied volatility"
    ) +
    theme_minimal()
}

# --------------------------------------------------
# Vendor surface slice by maturity in days
# --------------------------------------------------

get_vendor_slice <- function(vsurf_df, days, opt_type = NULL) {
  out <- vsurf_df %>%
    filter(days == days)
  
  if (!is.null(opt_type)) {
    out <- out %>% filter(cp_flag == opt_type)
  }
  
  out %>% arrange(impl_strike)
}

# safer version avoiding name collision
get_vendor_slice <- function(vsurf_df, tau_days, opt_type = NULL) {
  out <- vsurf_df %>%
    filter(days == tau_days)
  
  if (!is.null(opt_type)) {
    out <- out %>% filter(cp_flag == opt_type)
  }
  
  out %>% arrange(impl_strike)
}

# --------------------------------------------------
# Compare raw smile vs vendor surface
# --------------------------------------------------

compare_raw_vs_vendor <- function(smile_df, vsurf_df, exdate, tau_days) {
  
  raw_df <- smile_df %>%
    filter(exdate == as.Date(exdate)) %>%
    mutate(source = "raw") %>%
    transmute(
      x = K,
      iv = iv,
      cp_flag = cp_flag,
      source = source
    )
  
  vendor_df <- vsurf_df %>%
    filter(days == tau_days) %>%
    mutate(source = "vendor") %>%
    transmute(
      x = impl_strike,
      iv = iv,
      cp_flag = cp_flag,
      source = source
    )
  
  bind_rows(raw_df, vendor_df)
}

plot_raw_vs_vendor <- function(smile_df, vsurf_df, exdate, tau_days) {
  df <- compare_raw_vs_vendor(smile_df, vsurf_df, exdate, tau_days)
  
  ggplot(df, aes(x = x, y = iv, shape = cp_flag, linetype = source)) +
    geom_point(data = subset(df, source == "raw"), alpha = 0.65) +
    geom_line(data = subset(df, source == "vendor"), linewidth = 0.8) +
    labs(
      title = paste("Raw smile vs vendor surface | expiry =", exdate,
                    "| vendor days =", tau_days),
      x = "strike",
      y = "implied volatility"
    ) +
    theme_minimal()
}

make_otm_smile <- function(smile_df) {
  smile_df %>%
    filter(
      (cp_flag == "C" & K >= F) |
        (cp_flag == "P" & K <  F)
    ) %>%
    arrange(date, exdate, K)
}


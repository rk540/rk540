suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

make_otm_smile <- function(smile_df) {
  smile_df %>%
    filter(
      (cp_flag == "C" & K >= F) |
        (cp_flag == "P" & K <  F)
    ) %>%
    arrange(date, exdate, K, cp_flag)
}

collapse_otm_smile <- function(otm_df) {
  otm_df %>%
    group_by(date, exdate, K) %>%
    summarise(
      F = first(F),
      T = first(T),
      k = first(k),
      iv = mean(iv, na.rm = TRUE),
      total_var = mean(total_var, na.rm = TRUE),
      mid = mean(mid, na.rm = TRUE),
      spread = mean(spread, na.rm = TRUE),
      n_quotes = n(),
      .groups = "drop"
    ) %>%
    arrange(date, exdate, K)
}

plot_otm_smile <- function(otm_df, exdate) {
  df <- get_expiry_slice(otm_df, exdate)
  
  ggplot(df, aes(x = k, y = iv)) +
    geom_point(alpha = 0.75) +
    labs(
      title = paste("OTM implied vol smile:", exdate),
      x = "log-moneyness k = log(K/F)",
      y = "implied volatility"
    ) +
    theme_minimal()
}
spy_chain_raw %>%
  select(date, exdate, cp_flag, strike_price, forward_price, impl_volatility) %>%
  head(20)

spy_chain_raw <- get_chain_on_date(wrds, "SPY", "2024-01-05")
spy_chain_raw %>%
  select(date, exdate, cp_flag, strike_price, best_bid, best_offer,
         impl_volatility, forward_price, delta) %>%
  head(20)

spy_vsurf_raw <- get_vendor_surface_on_date(wrds, "SPY", "2024-01-05")
head(spy_vsurf_raw, 20)

spy_spot <- get_underlying_history(wrds, "SPY", "2024-01-05", "2024-01-05")

spy_smile <- build_smile_dataset(
  chain = spy_chain_raw,
  underlying = spy_spot,
  strike_scale = 1000
)

nrow(spy_smile)

spy_smile %>%
  select(date, exdate, cp_flag, K, F, iv, k, total_var) %>%
  head(20)

spy_chain_raw %>%
  count(exdate) %>%
  arrange(exdate) %>%
  head(20)


spy_spot <- get_underlying_history(wrds, "SPY", "2024-01-05", "2024-01-05")
spy_spot

spy_smile <- build_smile_dataset(
  chain = spy_chain_raw,
  underlying = spy_spot,
  strike_scale = 1000
)

nrow(spy_smile)

spy_smile %>%
  select(date, exdate, cp_flag, K, F, iv, k, total_var) %>%
  head(20)

spy_smile %>%
  count(exdate) %>%
  arrange(exdate) %>%
  head(20)

source("volsmileanalysis.R")
# raw slice
plot_raw_smile(spy_smile, "2024-01-19")

# vendor/raw comparison
plot_raw_vs_vendor(
  smile_df = spy_smile,
  vsurf_df = spy_vsurf,
  exdate = "2024-01-19",
  tau_days = 14
)


spy_otm <- make_otm_smile(spy_smile)

spy_otm %>%
  count(exdate) %>%
  arrange(exdate) %>%
  head(20)

plot_raw_smile(spy_otm, "2024-01-19")
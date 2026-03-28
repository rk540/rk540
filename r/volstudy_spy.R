source("wrds_optionm.R")

spy_info  <- get_security_info(wrds, "SPY")
spy_spot  <- get_underlying_history(wrds, "SPY", "2024-01-02", "2024-01-10")
spy_chain <- get_chain_on_date(wrds, "SPY", "2024-01-05")
spy_vsurf <- get_vendor_surface_on_date(wrds, "SPY", "2024-01-05")


spy_chain_raw <- get_chain_on_date(wrds, "SPY", "2024-01-05")
spy_smile <- build_smile_dataset(spy_chain_raw)

spy_vsurf_raw <- get_vendor_surface_on_date(wrds, "SPY", "2024-01-05")
spy_vsurf <- build_vendor_surface_dataset(spy_vsurf_raw)

head(spy_smile)
head(spy_vsurf)

summarize_surface_snapshot(spy_smile)

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

spy_info <- get_security_info(wrds, "SPY")
spy_info

spy_info %>% select(secid, ticker, issue_type, index_flag, exchange_d, class, cusip, sic)

for (sid in spy_info$secid) {
  cat("Testing secid:", sid, "\n")
  n_chain <- DBI::dbGetQuery(wrds, sprintf("
    SELECT COUNT(*) AS n
    FROM optionm_all.opprcd2024
    WHERE secid = %s
      AND date = '2024-01-05'
  ", sid))
  print(n_chain)
}

spy_info <- get_security_info(wrds, "SPY")

for (sid in spy_info$secid) {
  cat("\nsecid =", sid, "\n")
  
  print(DBI::dbGetQuery(wrds, sprintf("
    SELECT COUNT(*) AS n
    FROM optionm_all.opprcd2024
    WHERE secid = %s
      AND date = '2024-01-05'
  ", sid)))
  
  print(DBI::dbGetQuery(wrds, sprintf("
    SELECT COUNT(*) AS n
    FROM optionm_all.vsurfd2024
    WHERE secid = %s
      AND date = '2024-01-05'
  ", sid)))
}

source("wrds_optionm.R")

spy_chain_raw <- get_chain_on_date(wrds, "SPY", "2024-01-05")
nrow(spy_chain_raw)

spy_vsurf_raw <- get_vendor_surface_on_date(wrds, "SPY", "2024-01-05")
nrow(spy_vsurf_raw)

spy_chain_raw %>%
  select(date, exdate, cp_flag, strike_price, best_bid, best_offer,
         impl_volatility, forward_price, delta) %>%
  head(20)

spy_vsurf_raw %>%
  head(20)


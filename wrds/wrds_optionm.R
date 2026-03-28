suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(glue)
})

source("wrdsconnect.R")

# --------------------------------------------------
# Helpers
# --------------------------------------------------

.year_seq <- function(start_date, end_date) {
  seq(
    as.integer(format(as.Date(start_date), "%Y")),
    as.integer(format(as.Date(end_date), "%Y")),
    by = 1
  )
}

.year_tables <- function(prefix, start_date, end_date) {
  paste0(prefix, .year_seq(start_date, end_date))
}

.union_year_queries <- function(schema, tables, sql_builder, order_by = NULL) {
  parts <- vapply(
    tables,
    function(tab) sql_builder(schema, tab),
    character(1)
  )
  
  qry <- paste(parts, collapse = "\nUNION ALL\n")
  
  if (!is.null(order_by) && nzchar(order_by)) {
    qry <- paste0(qry, "\nORDER BY ", order_by)
  }
  
  qry
}

# --------------------------------------------------
# Security lookup
# --------------------------------------------------

get_security_info <- function(wrds, ticker, schema = "optionm_all") {
  qry <- glue("
    SELECT *
    FROM {schema}.securd
    WHERE ticker = '{ticker}'
  ")
  out <- dbGetQuery(wrds, qry)
  
  if (nrow(out) == 0) {
    stop(glue("No security info found for ticker: {ticker}"))
  }
  
  out
}

get_secid <- function(wrds, ticker, start_date = NULL, end_date = NULL,
                      schema = "optionm_all") {
  info <- get_security_info(wrds, ticker, schema = schema)
  
  # If no date context is provided, keep old behavior
  if (is.null(start_date) || is.null(end_date)) {
    return(info$secid[1])
  }
  
  years <- seq(
    as.integer(format(as.Date(start_date), "%Y")),
    as.integer(format(as.Date(end_date), "%Y")),
    by = 1
  )
  
  counts <- lapply(info$secid, function(sid) {
    total_n <- 0L
    
    for (yy in years) {
      tab <- paste0("opprcd", yy)
      qry <- glue::glue("
        SELECT COUNT(*) AS n
        FROM {schema}.{tab}
        WHERE secid = {sid}
          AND date BETWEEN '{start_date}' AND '{end_date}'
      ")
      n_df <- DBI::dbGetQuery(wrds, qry)
      total_n <- total_n + n_df$n[1]
    }
    
    data.frame(secid = sid, n = total_n)
  })
  
  counts <- dplyr::bind_rows(counts)
  
  good <- counts[counts$n > 0, , drop = FALSE]
  
  if (nrow(good) == 0) {
    stop(glue::glue(
      "No option-active secid found for ticker {ticker} in requested date range"
    ))
  }
  
  good$secid[which.max(good$n)]
}

# --------------------------------------------------
# Underlying history
# --------------------------------------------------

get_underlying_history <- function(wrds, ticker, start_date, end_date,
                                   schema = "optionm_all") {
  secid <- get_secid(wrds, ticker, start_date, end_date, schema)
  tables <- .year_tables("secprd", start_date, end_date)
  
  qry <- .union_year_queries(
    schema = schema,
    tables = tables,
    sql_builder = function(schema, tab) glue::glue("
      SELECT secid, date, open, high, low, close, volume, return, cfadj
      FROM {schema}.{tab}
      WHERE secid = {secid}
        AND date BETWEEN '{start_date}' AND '{end_date}'
    "),
    order_by = "date"
  )
  
  DBI::dbGetQuery(wrds, qry)
}

# --------------------------------------------------
# Raw option chain
# --------------------------------------------------

get_option_chain <- function(wrds, ticker, start_date, end_date,
                             schema = "optionm_all") {
  secid <- get_secid(wrds, ticker, start_date, end_date, schema)
  tables <- .year_tables("opprcd", start_date, end_date)
  
  qry <- .union_year_queries(
    schema = schema,
    tables = tables,
    sql_builder = function(schema, tab) glue::glue("
      SELECT
        secid, date, symbol, symbol_flag, exdate, last_date, cp_flag,
        strike_price, best_bid, best_offer, volume, open_interest,
        impl_volatility, delta, gamma, vega, theta,
        optionid, cfadj, am_settlement, contract_size, ss_flag,
        forward_price, expiry_indicator, root, suffix
      FROM {schema}.{tab}
      WHERE secid = {secid}
        AND date BETWEEN '{start_date}' AND '{end_date}'
    "),
    order_by = "date, exdate, strike_price, cp_flag"
  )
  
  DBI::dbGetQuery(wrds, qry)
}

get_chain_on_date <- function(wrds, ticker, date, schema = "optionm_all") {
  get_option_chain(wrds, ticker, date, date, schema)
}

# --------------------------------------------------
# Vendor vol surface
# --------------------------------------------------

get_vendor_surface <- function(wrds, ticker, start_date, end_date,
                               schema = "optionm_all") {
  secid <- get_secid(wrds, ticker, start_date, end_date, schema)
  tables <- .year_tables("vsurfd", start_date, end_date)
  
  qry <- .union_year_queries(
    schema = schema,
    tables = tables,
    sql_builder = function(schema, tab) glue::glue("
      SELECT
        secid, date, days, delta, impl_volatility,
        impl_strike, impl_premium, dispersion, cp_flag
      FROM {schema}.{tab}
      WHERE secid = {secid}
        AND date BETWEEN '{start_date}' AND '{end_date}'
    "),
    order_by = "date, days, cp_flag, delta"
  )
  
  DBI::dbGetQuery(wrds, qry)
}

get_vendor_surface_on_date <- function(wrds, ticker, date,
                                       schema = "optionm_all") {
  get_vendor_surface(wrds, ticker, date, date, schema)
}

# --------------------------------------------------
# Build canonical smile dataset from raw chain
# --------------------------------------------------
build_smile_dataset <- function(chain,
                                underlying = NULL,
                                strike_scale = 1000,
                                min_bid = 0.05,
                                min_T = 1 / 365,
                                max_abs_k = 1.5,
                                use_spot_fallback = TRUE) {
  
  out <- chain %>%
    mutate(
      mid = 0.5 * (best_bid + best_offer),
      spread = best_offer - best_bid,
      T = as.numeric(as.Date(exdate) - as.Date(date)) / 365.0,
      K = strike_price / strike_scale,
      iv = impl_volatility,
      F = forward_price
    )
  
  if (use_spot_fallback) {
    if (is.null(underlying)) {
      stop("underlying data must be provided when use_spot_fallback = TRUE")
    }
    
    spot_df <- underlying %>%
      select(date, spot = close)
    
    out <- out %>%
      left_join(spot_df, by = "date") %>%
      mutate(
        F = ifelse(is.na(F) | F <= 0, spot, F)
      )
  }
  
  out <- out %>%
    filter(!is.na(T), T > min_T) %>%
    filter(!is.na(K), K > 0) %>%
    filter(!is.na(iv), iv > 0) %>%
    filter(!is.na(mid), mid >= min_bid) %>%
    filter(!is.na(F), F > 0) %>%
    mutate(
      k = log(K / F),
      total_var = T * iv^2
    ) %>%
    filter(is.finite(k), is.finite(total_var)) %>%
    filter(abs(k) <= max_abs_k)
  
  out
}

# --------------------------------------------------
# Build canonical vendor surface dataset
# --------------------------------------------------

build_vendor_surface_dataset <- function(vsurf,
                                         min_days = 1,
                                         max_abs_delta = 0.999) {
  vsurf %>%
    mutate(
      T = days / 365.0,
      iv = impl_volatility,
      K = impl_strike
    ) %>%
    filter(!is.na(T), T >= min_days / 365.0) %>%
    filter(!is.na(iv), iv > 0) %>%
    filter(!is.na(delta), abs(delta) <= max_abs_delta)
}

# --------------------------------------------------
# Simple join helper for raw-vendor comparison by date
# --------------------------------------------------

summarize_surface_snapshot <- function(smile_df) {
  smile_df %>%
    group_by(date, exdate, cp_flag) %>%
    summarise(
      n = n(),
      k_min = min(k, na.rm = TRUE),
      k_max = max(k, na.rm = TRUE),
      iv_min = min(iv, na.rm = TRUE),
      iv_max = max(iv, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date, exdate, cp_flag)
}
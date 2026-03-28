#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Expected one JSON payload argument.")
}

payload <- jsonlite::fromJSON(args[[1]], simplifyVector = TRUE)

source("~/dev/wrds/run_surface_analysis.R")

ticker <- payload$ticker
trade_date <- payload$trade_date
output_dir <- payload$output_dir
cfg <- payload$config

wrds <- connect()

res <- NULL

if (!is.null(payload$expiry) && !is.na(payload$expiry) && nzchar(payload$expiry)) {
  res <- analyze_single_expiry(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiry = payload$expiry,
    outdir = output_dir,
    strike_scale = ifelse(is.null(cfg$strike_scale), 1000, cfg$strike_scale),
    min_bid = ifelse(is.null(cfg$min_bid), 0.05, cfg$min_bid),
    min_points = ifelse(is.null(cfg$min_points), 8, cfg$min_points),
    max_abs_k = ifelse(is.null(cfg$max_abs_k), 1.5, cfg$max_abs_k)
  )
} else if (!is.null(payload$expiries) && length(payload$expiries) > 0) {
  res <- analyze_vol_surface(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiries = payload$expiries,
    outdir = output_dir,
    strike_scale = ifelse(is.null(cfg$strike_scale), 1000, cfg$strike_scale),
    min_bid = ifelse(is.null(cfg$min_bid), 0.05, cfg$min_bid),
    min_points = ifelse(is.null(cfg$min_points), 8, cfg$min_points),
    max_abs_k = ifelse(is.null(cfg$max_abs_k), 1.5, cfg$max_abs_k)
  )
} else if (!is.null(payload$expiry_start) || !is.null(payload$expiry_end)) {
  res <- analyze_vol_surface(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiry_start = payload$expiry_start,
    expiry_end = payload$expiry_end,
    outdir = output_dir,
    strike_scale = ifelse(is.null(cfg$strike_scale), 1000, cfg$strike_scale),
    min_bid = ifelse(is.null(cfg$min_bid), 0.05, cfg$min_bid),
    min_points = ifelse(is.null(cfg$min_points), 8, cfg$min_points),
    max_abs_k = ifelse(is.null(cfg$max_abs_k), 1.5, cfg$max_abs_k)
  )
} else {
  res <- analyze_all_expiries(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    outdir = output_dir,
    strike_scale = ifelse(is.null(cfg$strike_scale), 1000, cfg$strike_scale),
    min_bid = ifelse(is.null(cfg$min_bid), 0.05, cfg$min_bid),
    min_points = ifelse(is.null(cfg$min_points), 8, cfg$min_points),
    max_abs_k = ifelse(is.null(cfg$max_abs_k), 1.5, cfg$max_abs_k)
  )
}

cat(sprintf("Completed model run for %s on %s\n", ticker, trade_date))

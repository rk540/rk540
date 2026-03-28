#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

source("~/dev/wrds/svi_tools.R")

# ============================================================
# Utility helpers
# ============================================================

.ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.date_to_char <- function(x) {
  format(as.Date(x), "%Y-%m-%d")
}

.pick_expiries <- function(smile_df,
                           expiries = NULL,
                           expiry_start = NULL,
                           expiry_end = NULL,
                           min_points = 8) {
  all_exp <- smile_df %>%
    count(exdate, name = "n_points") %>%
    arrange(exdate)
  
  if (!is.null(expiries)) {
    exp_vec <- as.Date(expiries)
    out <- all_exp %>% filter(exdate %in% exp_vec)
  } else if (!is.null(expiry_start) || !is.null(expiry_end)) {
    lo <- if (is.null(expiry_start)) min(all_exp$exdate) else as.Date(expiry_start)
    hi <- if (is.null(expiry_end)) max(all_exp$exdate) else as.Date(expiry_end)
    out <- all_exp %>% filter(exdate >= lo, exdate <= hi)
  } else {
    out <- all_exp
  }
  
  out %>% filter(n_points >= min_points)
}

.days_between <- function(d1, d2) {
  as.integer(as.Date(d2) - as.Date(d1))
}

.safe_plot_save <- function(plot_obj, filename, width = 8, height = 5, dpi = 150) {
  ggplot2::ggsave(filename, plot = plot_obj, width = width, height = height, dpi = dpi)
}

.safe_fit_svi <- function(df, ...) {
  tryCatch(
    fit_svi_expiry(df, ...),
    error = function(e) {
      structure(
        list(
          error = TRUE,
          message = conditionMessage(e)
        ),
        class = "svi_fit_error"
      )
    }
  )
}

is_svi_fit_error <- function(x) inherits(x, "svi_fit_error")

# ============================================================
# Forward note
# ============================================================
# Current implementation uses:
#   1) WRDS forward_price if populated
#   2) spot fallback from secprd close
#
# This is acceptable for current interview-prep surface work.
# A production-grade implementation would infer/fit forwards using:
#   - rates
#   - dividends
#   - borrow / repo
#   - put-call parity consistency
#   - possibly robust cross-strike fitting
# ============================================================

# ============================================================
# Main orchestration function
# ============================================================

analyze_vol_surface <- function(wrds,
                                ticker,
                                trade_date,
                                expiries = NULL,
                                expiry_start = NULL,
                                expiry_end = NULL,
                                outdir = NULL,
                                strike_scale = 1000,
                                min_bid = 0.05,
                                min_points = 8,
                                max_abs_k = 1.5,
                                save_plots = TRUE,
                                save_data = TRUE,
                                verbose = TRUE) {
  trade_date <- as.Date(trade_date)
  
  if (is.null(outdir)) {
    outdir <- file.path(
      "~/dev/wrds/output",
      paste0(
        ticker, "_",
        format(trade_date, "%Y%m%d")
      )
    )
  }
  
  outdir <- path.expand(outdir)
  .ensure_dir(outdir)
  .ensure_dir(file.path(outdir, "plots"))
  .ensure_dir(file.path(outdir, "data"))
  
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Running vol surface analysis\n")
    cat("Ticker     :", ticker, "\n")
    cat("Trade date :", .date_to_char(trade_date), "\n")
    cat("Output dir :", outdir, "\n")
    cat("--------------------------------------------------\n")
  }
  
  # ----------------------------------------------------------
  # 1. Pull raw data
  # ----------------------------------------------------------
  if (verbose) cat("Pulling underlying history...\n")
  underlying <- get_underlying_history(
    wrds = wrds,
    ticker = ticker,
    start_date = trade_date,
    end_date = trade_date
  )
  
  if (nrow(underlying) == 0) {
    stop("No underlying history returned.")
  }
  
  if (verbose) cat("Pulling raw option chain...\n")
  chain_raw <- get_chain_on_date(
    wrds = wrds,
    ticker = ticker,
    date = trade_date
  )
  
  if (nrow(chain_raw) == 0) {
    stop("No option chain rows returned.")
  }
  
  if (verbose) cat("Pulling vendor surface...\n")
  vendor_raw <- get_vendor_surface_on_date(
    wrds = wrds,
    ticker = ticker,
    date = trade_date
  )
  
  # ----------------------------------------------------------
  # 2. Build smile datasets
  # ----------------------------------------------------------
  if (verbose) cat("Building smile dataset...\n")
  smile <- build_smile_dataset(
    chain = chain_raw,
    underlying = underlying,
    strike_scale = strike_scale,
    min_bid = min_bid,
    max_abs_k = max_abs_k,
    use_spot_fallback = TRUE
  )
  
  if (nrow(smile) == 0) {
    stop("Smile dataset is empty after filtering.")
  }
  
  if (verbose) cat("Applying OTM filtering...\n")
  otm <- make_otm_smile(smile)
  otm_collapsed <- collapse_otm_smile(otm)
  
  if (nrow(otm_collapsed) == 0) {
    stop("OTM collapsed smile is empty.")
  }
  
  vendor <- build_vendor_surface_dataset(vendor_raw)
  
  # ----------------------------------------------------------
  # 3. Pick expiries
  # ----------------------------------------------------------
  exp_tbl <- .pick_expiries(
    smile_df = otm_collapsed,
    expiries = expiries,
    expiry_start = expiry_start,
    expiry_end = expiry_end,
    min_points = min_points
  )
  
  if (nrow(exp_tbl) == 0) {
    stop("No expiries selected after filtering.")
  }
  
  if (verbose) {
    cat("Selected expiries:\n")
    print(exp_tbl)
  }
  
  # ----------------------------------------------------------
  # 4. Save base datasets
  # ----------------------------------------------------------
  if (save_data) {
    if (verbose) cat("Saving base datasets...\n")
    readr::write_csv(chain_raw, file.path(outdir, "data", "chain_raw.csv"))
    readr::write_csv(smile, file.path(outdir, "data", "smile.csv"))
    readr::write_csv(otm_collapsed, file.path(outdir, "data", "otm_collapsed.csv"))
    readr::write_csv(vendor, file.path(outdir, "data", "vendor_surface.csv"))
    readr::write_csv(underlying, file.path(outdir, "data", "underlying.csv"))
    readr::write_csv(exp_tbl, file.path(outdir, "data", "selected_expiries.csv"))
  }
  
  # ----------------------------------------------------------
  # 5. Per-expiry analysis
  # ----------------------------------------------------------
  fits <- list()
  diag_list <- list()
  meta_list <- list()
  
  for (i in seq_len(nrow(exp_tbl))) {
    exd <- exp_tbl$exdate[i]
    npts <- exp_tbl$n_points[i]
    
    if (verbose) {
      cat("\nAnalyzing expiry:", .date_to_char(exd),
          "| points:", npts, "\n")
    }
    
    # df0 <- get_expiry_slice(otm_collapsed, exd)
    df0 <- otm_collapsed[otm_collapsed$exdate == as.Date(exd), , drop = FALSE]
    df0 <- df0[order(df0$K), , drop = FALSE]
    
    print(df0 %>%
            dplyr::summarise(
              n_rows = dplyr::n(),
              n_expiry = dplyr::n_distinct(exdate),
              min_expiry = min(exdate),
              max_expiry = max(exdate)
            ))
    
    fit0 <- .safe_fit_svi(df0)
    
    if (is_svi_fit_error(fit0)) {
      warning(sprintf("SVI fit failed for expiry %s: %s",
                      .date_to_char(exd), fit0$message))
      meta_list[[length(meta_list) + 1]] <- tibble::tibble(
        ticker = ticker,
        trade_date = trade_date,
        exdate = as.Date(exd),
        tau_days = .days_between(trade_date, exd),
        n_points = nrow(df0),
        fit_ok = FALSE,
        fit_message = fit0$message
      )
      next
    }
    
    diag0 <- svi_diagnostics(fit0) %>%
      mutate(
        ticker = ticker,
        trade_date = trade_date,
        exdate = as.Date(exd),
        tau_days = .days_between(trade_date, exd)
      ) %>%
      select(ticker, trade_date, exdate, tau_days, everything())
    
    meta0 <- tibble::tibble(
      ticker = ticker,
      trade_date = trade_date,
      exdate = as.Date(exd),
      tau_days = .days_between(trade_date, exd),
      n_points = nrow(df0),
      fit_ok = TRUE,
      fit_message = NA_character_
    )
    
    fits[[as.character(exd)]] <- fit0
    diag_list[[as.character(exd)]] <- diag0
    meta_list[[as.character(exd)]] <- meta0
    
    if (save_data) {
      ex_tag <- gsub("-", "", .date_to_char(exd))
      readr::write_csv(fit0$fit_df,
                       file.path(outdir, "data", paste0("fit_df_", ex_tag, ".csv")))
      readr::write_csv(diag0,
                       file.path(outdir, "data", paste0("diag_", ex_tag, ".csv")))
    }
    
    if (save_plots) {
      ex_tag <- gsub("-", "", .date_to_char(exd))
      
      p_otm <- plot_otm_smile(otm_collapsed, exd)
      p_fit <- plot_svi_fit(fit0)
      p_tv  <- plot_svi_total_variance_fit(fit0)
      
      .safe_plot_save(p_otm, file.path(outdir, "plots", paste0("otm_smile_", ex_tag, ".png")))
      .safe_plot_save(p_fit, file.path(outdir, "plots", paste0("svi_fit_", ex_tag, ".png")))
      .safe_plot_save(p_tv,  file.path(outdir, "plots", paste0("svi_totalvar_", ex_tag, ".png")))
      
      tau_days <- .days_between(trade_date, exd)
      if (nrow(vendor) > 0 && tau_days %in% vendor$days) {
        p_vendor <- plot_vendor_vs_svi(fit0, vendor, tau_days = tau_days)
        .safe_plot_save(
          p_vendor,
          file.path(outdir, "plots", paste0("vendor_vs_svi_", ex_tag, ".png"))
        )
      }
    }
  }
  
  # ----------------------------------------------------------
  # 6. Aggregate outputs
  # ----------------------------------------------------------
  diag_tbl <- if (length(diag_list) > 0) dplyr::bind_rows(diag_list) else tibble::tibble()
  meta_tbl <- if (length(meta_list) > 0) dplyr::bind_rows(meta_list) else tibble::tibble()
  
  if (save_data) {
    readr::write_csv(meta_tbl, file.path(outdir, "data", "fit_status.csv"))
    if (nrow(diag_tbl) > 0) {
      readr::write_csv(diag_tbl, file.path(outdir, "data", "all_diagnostics.csv"))
    }
  }
  
  if (verbose) {
    cat("\nDone.\n")
    if (nrow(diag_tbl) > 0) {
      cat("Successful fits:", nrow(diag_tbl), "\n")
    } else {
      cat("No successful fits.\n")
    }
  }
  
  list(
    ticker = ticker,
    trade_date = trade_date,
    underlying = underlying,
    chain_raw = chain_raw,
    smile = smile,
    otm_collapsed = otm_collapsed,
    vendor = vendor,
    selected_expiries = exp_tbl,
    fit_status = meta_tbl,
    diagnostics = diag_tbl,
    fits = fits,
    outdir = outdir
  )
}

# ============================================================
# Convenience wrappers
# ============================================================

analyze_single_expiry <- function(wrds, ticker, trade_date, expiry, ...) {
  analyze_vol_surface(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiries = expiry,
    ...
  )
}

analyze_expiry_range <- function(wrds, ticker, trade_date, expiry_start, expiry_end, ...) {
  analyze_vol_surface(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiry_start = expiry_start,
    expiry_end = expiry_end,
    ...
  )
}

analyze_all_expiries <- function(wrds, ticker, trade_date, ...) {
  analyze_vol_surface(
    wrds = wrds,
    ticker = ticker,
    trade_date = trade_date,
    expiries = NULL,
    expiry_start = NULL,
    expiry_end = NULL,
    ...
  )
}
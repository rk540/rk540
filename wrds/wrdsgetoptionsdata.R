#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(readr)
  library(glue)
})

source("wrdsconnect.R")

# --------------------------------------------------
# Utility: list schemas/tables so we confirm access
# --------------------------------------------------
list_wrds_option_tables <- function(wrds) {
  qry <- "
    SELECT table_schema, table_name
    FROM information_schema.tables
    WHERE table_schema ILIKE 'optionm%'
    ORDER BY table_schema, table_name
  "
  dbGetQuery(wrds, qry)
}

list_table_columns <- function(wrds, schema_name, table_name) {
  qry <- glue("
    SELECT column_name, data_type
    FROM information_schema.columns
    WHERE table_schema = '{schema_name}'
      AND table_name   = '{table_name}'
    ORDER BY ordinal_position
  ")
  dbGetQuery(wrds, qry)
}

# --------------------------------------------------
# Pull underlying daily data
# --------------------------------------------------
get_optionm_secprd <- function(wrds, ticker, start_date, end_date,
                               schema_name = "optionm_all") {
  qry <- glue("
    SELECT *
    FROM {schema_name}.secprd
    WHERE ticker = '{ticker}'
      AND date BETWEEN '{start_date}' AND '{end_date}'
    ORDER BY date
  ")
  dbGetQuery(wrds, qry)
}

# --------------------------------------------------
# Pull option chain
# --------------------------------------------------
get_optionm_opprcd <- function(wrds, secid, start_date, end_date,
                               schema_name = "optionm_all") {
  qry <- glue("
    SELECT *
    FROM {schema_name}.opprcd
    WHERE secid = {secid}
      AND date BETWEEN '{start_date}' AND '{end_date}'
    ORDER BY date, exdate, strike_price
  ")
  dbGetQuery(wrds, qry)
}

# --------------------------------------------------
# Resolve secid from ticker
# --------------------------------------------------
get_secid_from_ticker <- function(wrds, ticker,
                                  schema_name = "optionm_all") {
  qry <- glue("
    SELECT DISTINCT secid, ticker
    FROM {schema_name}.secprd
    WHERE ticker = '{ticker}'
    ORDER BY secid
  ")
  dbGetQuery(wrds, qry)
}

# --------------------------------------------------
# Example run
# --------------------------------------------------
ticker <- "SPY"
start_date <- "2024-01-02"
end_date   <- "2024-01-10"
outdir <- "wrds_optionm_output"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

wrds <- connect()
on.exit(dbDisconnect(wrds), add = TRUE)
class(wrds)
DBI::dbIsValid(wrds)
DBI::dbGetInfo(wrds)
cat("Connected.\n")

cat("\n--- Available OptionMetrics tables ---\n")
tbls <- list_wrds_option_tables(wrds)
print(tbls, n = 200)
write_csv(tbls, file.path(outdir, "optionm_tables.csv"))

cat("\n--- secprd columns ---\n")
secprd_cols <- tryCatch(
  list_table_columns(wrds, "optionm_all", "secprd"),
  error = function(e) NULL
)
print(secprd_cols, n = 200)

cat("\n--- opprcd columns ---\n")
opprcd_cols <- tryCatch(
  list_table_columns(wrds, "optionm_all", "opprcd"),
  error = function(e) NULL
)
print(opprcd_cols, n = 200)

cat("\n--- secid lookup ---\n")
secid_df <- get_secid_from_ticker(wrds, ticker)
print(secid_df)

if (nrow(secid_df) == 0) {
  stop("No secid found for ticker.")
}

secid <- secid_df$secid[1]

cat(glue("\nUsing secid = {secid}\n"))

cat("\n--- secprd sample ---\n")
secprd <- get_optionm_secprd(wrds, ticker, start_date, end_date)
print(head(secprd, 20))
write_csv(secprd, file.path(outdir, "secprd.csv"))

cat("\n--- opprcd sample ---\n")
opprcd <- get_optionm_opprcd(wrds, secid, start_date, end_date)
print(head(opprcd, 20))
write_csv(opprcd, file.path(outdir, "opprcd.csv"))

cat("\nDone.\n")
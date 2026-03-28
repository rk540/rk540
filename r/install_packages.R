options(repos = c(CRAN = "https://cloud.r-project.org"))

pkgs <- c("DBI", "RPostgres")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing)
} else {
  cat("All packages already installed.\n")
}

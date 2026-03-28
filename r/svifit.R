source("coresvifunctions.R")

# Fits an SVI volatility Surface.

fit_svi_expiry <- function(df,
                           par_init = c(a = 1e-4, b = 0.05, rho = -0.5, m = 0, sigma = 0.05),
                           use_weights = TRUE) {
  stopifnot(all(c("k", "total_var", "mid") %in% names(df)))
  
  if (dplyr::n_distinct(df$exdate) != 1) {
    stop("SVI fit requires a single expiry. Multiple expiries detected.")
  }
  
  fit_df <- df %>%
    filter(is.finite(k), is.finite(total_var)) %>%
    arrange(k)
  
  if (nrow(fit_df) < 8) {
    stop("Not enough points to fit SVI.")
  }
  
  wts <- NULL
  if (use_weights) {
    # modest liquidity weight using option mid
    wts <- pmax(fit_df$mid, 0.01)
    wts <- wts / mean(wts, na.rm = TRUE)
  }
  
  opt <- optim(
    par = par_init,
    fn = svi_objective,
    k = fit_df$k,
    w = fit_df$total_var,
    weights = wts,
    method = "L-BFGS-B",
    lower = c(-1, 1e-8, -0.999, -2, 1e-6),
    upper = c( 1, 10,    0.999,  2,  2)
  )
  
  pars <- setNames(opt$par, c("a", "b", "rho", "m", "sigma"))
  
  fit_df <- fit_df %>%
    mutate(
      total_var_fit = svi_raw(k, pars["a"], pars["b"], pars["rho"], pars["m"], pars["sigma"]),
      iv_fit = sqrt(pmax(total_var_fit / T, 0))
    )
  
  list(
    par = pars,
    value = opt$value,
    convergence = opt$convergence,
    message = opt$message,
    fit_df = fit_df
  )
}
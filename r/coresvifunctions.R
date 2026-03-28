svi_raw <- function(k, a, b, rho, m, sigma) {
  a + b * (rho * (k - m) + sqrt((k - m)^2 + sigma^2))
}

svi_objective <- function(par, k, w, weights = NULL) {
  a <- par[1]
  b <- par[2]
  rho <- par[3]
  m <- par[4]
  sigma <- par[5]
  
  # simple parameter admissibility penalties
  if (b <= 0 || sigma <= 0 || abs(rho) >= 1) {
    return(1e12)
  }
  
  w_fit <- svi_raw(k, a, b, rho, m, sigma)
  
  if (any(!is.finite(w_fit))) {
    return(1e12)
  }
  
  resid <- w - w_fit
  
  if (is.null(weights)) {
    sum(resid^2)
  } else {
    sum(weights * resid^2)
  }
}

svi_raw_prime <- function(k, a, b, rho, m, sigma) {
  b * (rho + (k - m) / sqrt((k - m)^2 + sigma^2))
}

svi_raw_second <- function(k, a, b, rho, m, sigma) {
  b * sigma^2 / (((k - m)^2 + sigma^2)^(3/2))
}
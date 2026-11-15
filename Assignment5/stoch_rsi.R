# Requires rsi.R and sma.R. Use whole-series normalization as in the brief.
stoch_rsi <- function(data, period, k_period, d_period) {
  stopifnot(length(k_period) == 1L, length(d_period) == 1L)
  periods <- c(k_period, d_period)
  stopifnot(all(is.finite(periods)), all(periods >= 1), all(periods == floor(periods)))
  values <- rsi(data, period)
  valid <- values[is.finite(values)]
  if (!length(valid)) return(list(k_line = numeric(), d_line = numeric()))
  limits <- range(valid)
  # A constant range has no defined stochastic normalization.
  scaled <- if (diff(limits) > 0) (valid - limits[1]) / diff(limits) else rep(NA_real_, length(valid))
  k <- if (length(scaled) >= k_period) sma(scaled, k_period) else numeric()
  d <- if (length(k) >= d_period) sma(k, d_period) else numeric()
  list(k_line = k, d_line = d)
}

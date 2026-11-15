# Wilder RSI. Seed once from the first period price changes.
rsi <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), all(is.finite(data)),
            length(period) == 1L, is.finite(period), period >= 1,
            period == floor(period))
  n <- length(data)
  out <- rep(NA_real_, n)
  if (n <= period) return(out)
  change <- diff(data)
  gain <- pmax(change, 0)
  loss <- pmax(-change, 0)
  g <- mean(gain[seq_len(period)])
  l <- mean(loss[seq_len(period)])
  for (i in seq.int(period + 1, n)) {
    if (i > period + 1) {
      g <- (g * (period - 1) + gain[i - 1]) / period
      l <- (l * (period - 1) + loss[i - 1]) / period
    }
    out[i] <- if (g == 0 && l == 0) 50 else 100 * g / (g + l)
  }
  out
}

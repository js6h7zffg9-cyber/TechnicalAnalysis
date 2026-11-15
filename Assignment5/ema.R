# Seed with the first observation, as specified in the brief.
ema <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  n <- length(data)
  if (!n) return(numeric())
  alpha <- 2 / (period + 1)
  out <- numeric(n)
  out[1] <- data[1]
  if (n > 1L) for (i in 2:n) out[i] <- out[i-1L] + alpha * (data[i] - out[i-1L])
  out
}

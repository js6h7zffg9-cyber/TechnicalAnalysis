# Requires ema.R. All three lines align with the input.
macd <- function(data, short_period, long_period, signal_period) {
  short <- ema(data, short_period)
  long <- ema(data, long_period)
  if (short_period >= long_period) stop("short_period must be below long_period.")
  line <- short - long
  signal <- ema(line, signal_period)
  list(macd_line = line, signal_line = signal, histogram = line - signal)
}

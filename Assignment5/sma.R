# Return one value per complete trailing window.
sma <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  if (length(data) < period) stop("Data length must be at least period.")
  vapply(seq_len(length(data) - period + 1), function(i) {
    mean(data[seq.int(i, length.out = period)])
  }, numeric(1))
}

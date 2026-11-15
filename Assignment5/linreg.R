# Use the brief's offset bounds and local indices in the fitted subset.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  stopifnot(is.numeric(regressionSource), is.null(dim(regressionSource)),
            length(regressionLength) == 1L, is.finite(regressionLength),
            regressionLength == floor(regressionLength),
            regressionLength >= 2, regressionLength <= n,
            length(regressionOffset) == 1L, is.finite(regressionOffset),
            regressionOffset == floor(regressionOffset),
            regressionOffset >= 0, regressionOffset < regressionLength)
  start <- max(1L, n - regressionLength + regressionOffset)
  end <- min(n, n - regressionOffset)
  if (end - start < 1L) stop("Regression interval must contain at least two points.")
  y <- regressionSource[seq.int(start, end)]
  x <- seq_along(y)
  centered <- x - mean(x)
  slope <- sum(centered * (y - mean(y))) / sum(centered^2)
  intercept <- mean(y) - slope * mean(x)
  list(slope = slope, intercept = intercept,
       predicted_values = intercept + slope * x)
}

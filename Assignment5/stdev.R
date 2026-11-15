# Population standard deviation: use n, as specified in the brief.
stdev <- function(data) {
  stopifnot(is.numeric(data), is.null(dim(data)), length(data) > 0L)
  sqrt(mean((data - mean(data))^2))
}

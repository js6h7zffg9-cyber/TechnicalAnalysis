# The first row has no previous observation. Missing pairs yield no event.
crossunder <- function(arr1, arr2) {
  stopifnot(is.numeric(arr1), is.numeric(arr2), length(arr1) == length(arr2))
  n <- length(arr1)
  out <- rep("False", n)
  if (!n) return(out)
  out[1L] <- "None"
  if (n > 1L) for (i in 2:n) {
    if (!all(is.finite(c(arr1[i-1L], arr1[i], arr2[i-1L], arr2[i])))) next
    if (arr1[i] < arr2[i] && arr1[i-1L] >= arr2[i-1L]) out[i] <- "True"
  }
  out
}

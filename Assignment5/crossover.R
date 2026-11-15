# Follow the pseudocode's character output and inclusive previous comparison.
crossover <- function(arr1, arr2) {
  stopifnot(is.numeric(arr1), is.numeric(arr2), length(arr1) == length(arr2))
  n <- length(arr1)
  out <- rep("None", n)
  if (n < 2L) return(out)
  for (i in 2:n) {
    if (!all(is.finite(c(arr1[i-1L], arr1[i], arr2[i-1L], arr2[i])))) next
    if (arr1[i] > arr2[i] && arr1[i-1L] <= arr2[i-1L]) out[i] <- "Up"
    if (arr1[i] < arr2[i] && arr1[i-1L] >= arr2[i-1L]) out[i] <- "Down"
  }
  out
}

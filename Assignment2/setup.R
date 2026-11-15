# Run once in RStudio. CRAN supplies dependencies automatically.
packages <- c("quantmod", "TTR")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
print(sapply(packages, function(p) as.character(packageVersion(p))))
sessionInfo()

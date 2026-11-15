# Ali Muhammad | BDA400 Assignment 2
# AI-assisted implementation. Historical prices are for coursework only.
load_stock_data <- function(file = "portfolio.txt", from = "2025-01-01",
                            to = "2026-01-01") {
  if (!file.exists(file)) stop("Portfolio file does not exist: ", file)
  symbols <- unique(trimws(readLines(file, warn = FALSE)))
  symbols <- symbols[nzchar(symbols)]
  if (!length(symbols)) stop("Portfolio is empty")
  from <- as.Date(from); to <- as.Date(to)
  if (is.na(from) || is.na(to) || from >= to) stop("Invalid date interval")
  stocks <- setNames(vector("list", length(symbols)), symbols)
  for (symbol in symbols) {
    x <- tryCatch(quantmod::getSymbols(symbol, src = "yahoo", from = from,
                         to = to, auto.assign = FALSE),
                  error = function(e) stop("Download failed for ", symbol, ": ", conditionMessage(e)))
    if (!NROW(x)) stop("No observations for ", symbol)
    df <- data.frame(Date = as.Date(zoo::index(x)),
                     Open = as.numeric(quantmod::Op(x)),
                     High = as.numeric(quantmod::Hi(x)),
                     Low = as.numeric(quantmod::Lo(x)),
                     Close = as.numeric(quantmod::Cl(x)),
                     Volume = as.numeric(quantmod::Vo(x)),
                     Adjusted = as.numeric(quantmod::Ad(x)))
    df <- df[order(df$Date), ]
    if (anyDuplicated(df$Date)) stop("Duplicate dates for ", symbol)
    stocks[[symbol]] <- df
  }
  stocks
}

# Return every modal price; all-unique observations have no informative mode.
price_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(numeric(0))
  values <- sort(unique(x)); counts <- tabulate(match(x, values))
  if (max(counts) == 1L) return(numeric(0))
  values[counts == max(counts)]
}

calculate_statistics <- function(stock, window = 20L) {
  if (!is.data.frame(stock) || !all(c("Date", "Adjusted") %in% names(stock)))
    stop("Expected a data frame with Date and Adjusted columns")
  if (length(window) != 1 || !is.finite(window) || window < 1 || window != as.integer(window))
    stop("Window must be a positive integer")
  if (!is.numeric(stock$Adjusted)) stop("Adjusted prices must be numeric")
  stock <- stock[order(stock$Date), ]
  keep <- is.finite(stock$Adjusted)
  clean <- stock[keep, , drop = FALSE]
  if (nrow(clean) < max(2, window)) stop("Too few valid observations")
  if (anyDuplicated(clean$Date) || anyNA(clean$Date)) stop("Invalid dates")
  clean$SMA <- as.numeric(TTR::SMA(clean$Adjusted, n = window))
  # Mode uses prices rounded to cents, avoiding meaningless binary precision differences.
  modes <- price_mode(round(clean$Adjusted, 2))
  summary <- data.frame(N = nrow(clean), Excluded = sum(!keep),
    Mean = mean(clean$Adjusted), Median = median(clean$Adjusted),
    Mode = if(length(modes)) paste(format(modes, nsmall=2), collapse="; ") else "No repeated price",
    SD = sd(clean$Adjusted), Window = window, Last_SMA = tail(clean$SMA, 1))
  list(summary = summary, series = clean)
}

display_stock_data <- function(stocks, rows = 6L) {
  for (symbol in names(stocks)) {
    cat("\n", symbol, " | ", nrow(stocks[[symbol]]), " rows\n", sep = "")
    print(head(stocks[[symbol]], rows), row.names = FALSE)
    print(tail(stocks[[symbol]], rows), row.names = FALSE)
  }
  invisible(stocks)
}

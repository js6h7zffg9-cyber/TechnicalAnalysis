# Run from the Assignment5 folder: source("run_analysis.R")
for (file in c("sma", "ema", "macd", "stdev", "linreg", "rsi",
               "stoch_rsi", "crossover", "crossunder")) source(paste0(file, ".R"))

load_prices <- function(symbol) {
  prices <- read.csv(paste0(symbol, "_prices.csv"))
  prices$Date <- as.Date(prices$Date)
  stopifnot(!anyNA(prices$Date), !anyDuplicated(prices$Date))
  prices[order(prices$Date), ]
}

# Date bounds are inclusive. A bar period means trading days or weeks.
filter_prices <- function(prices, from, to, frame = c("daily", "weekly"),
                          price_source = c("Close", "Adjusted", "Open", "High", "Low")) {
  frame <- match.arg(frame)
  price_source <- match.arg(price_source)
  from <- as.Date(from)
  to <- as.Date(to)
  stopifnot(length(from) == 1L, length(to) == 1L, !is.na(from), !is.na(to), from <= to)
  prices <- prices[prices$Date >= from & prices$Date <= to, , drop = FALSE]
  if (frame == "weekly" && nrow(prices)) {
    weeks <- split(seq_len(nrow(prices)), format(prices$Date, "%G-%V"))
    prices <- do.call(rbind, lapply(weeks, function(idx) {
      bar <- prices[idx[length(idx)], ]
      bar$Open <- prices$Open[idx[1L]]
      bar$High <- max(prices$High[idx])
      bar$Low <- min(prices$Low[idx])
      bar$Volume <- sum(prices$Volume[idx])
      bar
    }))
    rownames(prices) <- NULL
  }
  prices$Source <- prices[[price_source]]
  prices
}

analyse_prices <- function(prices) {
  p <- prices$Source
  stopifnot(length(p) >= 26L, all(is.finite(p)))
  m <- macd(p, 12, 26, 9)
  st <- stoch_rsi(p, 14, 3, 3)
  pad <- function(x) c(rep(NA_real_, length(p) - length(x)), x)
  prices$SMA20 <- pad(sma(p, 20))
  prices$EMA20 <- ema(p, 20)
  prices$MACD <- m$macd_line
  prices$MACDSignal <- m$signal_line
  prices$MACDHistogram <- m$histogram
  prices$RSI14 <- rsi(p, 14)
  prices$StochK <- pad(st$k_line)
  prices$StochD <- pad(st$d_line)
  prices$Cross <- crossover(prices$MACD, prices$MACDSignal)
  prices$Crossunder <- crossunder(prices$MACD, prices$MACDSignal)
  # Signals apply at bar close. A later execution would need a separate fill rule.
  prices$Rule <- ifelse(prices$Cross == "Up", "Buy",
                       ifelse(prices$Crossunder == "True", "Sell", "Hold"))
  prices
}

symbols <- c("AAPL", "MSFT", "GOOG")
stocks <- setNames(lapply(symbols, load_prices), symbols)
analysis <- lapply(stocks, function(x) {
  analyse_prices(filter_prices(x, "2025-01-01", "2025-12-31", "daily", "Close"))
})
summary_table <- do.call(rbind, lapply(symbols, function(symbol) {
  x <- analysis[[symbol]]
  last <- tail(x, 1L)
  data.frame(Symbol = symbol, Bars = nrow(x), PopulationSD = stdev(x$Source),
             RegressionSlope = linreg(x$Source, 20, 0)$slope,
             RSI14 = last$RSI14, AboveSMA = last$Source > last$SMA20,
             Rule = last$Rule, BuySignals = sum(x$Rule == "Buy"),
             SellSignals = sum(x$Rule == "Sell"))
}))
print(summary_table, row.names = FALSE)
cat("\nScreened symbols: closing price above SMA20 and RSI14 below 70\n")
print(subset(summary_table, AboveSMA & RSI14 < 70), row.names = FALSE)

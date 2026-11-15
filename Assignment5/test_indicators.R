source("run_analysis.R")
same <- function(actual, expected) stopifnot(isTRUE(all.equal(actual, expected)))
same(sma(1:5, 3), c(2, 3, 4))
same(ema(c(2, 4, 8), 3), c(2, 3, 5.5))
m <- macd(c(2, 4, 8), 1, 3, 3)
same(m$macd_line, c(0, 1, 2.5))
same(m$signal_line, c(0, .5, 1.5))
same(m$histogram, c(0, .5, 1))
same(stdev(c(1, 3)), 1)
same(linreg(c(1, 3, 5, 7, 9), 4, 1),
     list(slope = 2, intercept = 1, predicted_values = c(3, 5, 7)))
same(rsi(c(10, 12, 11, 12), 2), c(NA, NA, 200/3, 80))
same(rsi(rep(4, 4), 2), c(NA, NA, 50, 50))
same(stoch_rsi(c(10, 12, 11, 12), 2, 1, 2),
     list(k_line = c(0, 1), d_line = .5))
same(crossover(c(0, 2, 0, 1), rep(1, 4)), c("None", "Up", "Down", "None"))
same(crossunder(c(0, 2, 0, 1), rep(1, 4)), c("None", "False", "True", "False"))
same(crossover(c(NA, 2, 0), rep(1, 3)), c("None", "None", "Down"))
stopifnot(inherits(try(sma(1:2, 3), silent = TRUE), "try-error"),
          inherits(try(crossunder(1, 1:2), silent = TRUE), "try-error"))
weekly <- filter_prices(stocks$AAPL, "2025-01-01", "2025-12-31", "weekly", "Adjusted")
same(sum(weekly$Volume), sum(stocks$AAPL$Volume))
stopifnot(nrow(weekly) < nrow(stocks$AAPL), all(is.finite(weekly$Source)),
          all(summary_table$Bars == 250), all(summary_table$BuySignals > 0),
          all(summary_table$SellSignals > 0))
cat("\nPASS: 9 indicators, edge cases, weekly aggregation and three-stock workflow.\n")

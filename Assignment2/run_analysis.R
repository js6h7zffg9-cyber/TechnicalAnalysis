# Open Assignment2 as the RStudio working directory, then source this file.
source("functions.R")
source("setup.R")
dir.create("outputs", showWarnings = FALSE)
stocks <- load_stock_data()
# Each named list entry is a separate data frame: stocks$AAPL, stocks$MSFT, stocks$GOOG.
display_stock_data(stocks)
results <- lapply(stocks, calculate_statistics, window = 20L)
summary_table <- do.call(rbind, lapply(names(results), function(symbol)
  cbind(Symbol = symbol, results[[symbol]]$summary)))
rownames(summary_table) <- NULL
print(summary_table, row.names = FALSE)
write.csv(summary_table, "outputs/statistics.csv", row.names = FALSE)
for (symbol in names(results)) {
  write.csv(stocks[[symbol]], file.path("outputs", paste0(symbol, "_prices.csv")), row.names = FALSE)
  series <- results[[symbol]]$series
  write.csv(series, file.path("outputs", paste0(symbol, "_sma.csv")), row.names = FALSE)
  png(file.path("outputs", paste0(symbol, "_chart.png")), width = 1200, height = 700, res = 120)
  plot(series$Date, series$Adjusted, type = "l", col = "#204864",
       xlab = "Date", ylab = "Adjusted price (USD)",
       main = paste(symbol, "adjusted prices and 20 observation SMA"))
  lines(series$Date, series$SMA, col = "#c05b19", lwd = 2)
  legend("topleft", c("Adjusted close", "20 observation SMA"),
         col = c("#204864", "#c05b19"), lty = 1, bty = "n")
  dev.off()
}
writeLines(c(paste("Retrieved:", format(Sys.time(), tz = "UTC")),
             "Source: Yahoo Finance through quantmod",
             "Requested interval: 2025-01-01 inclusive to 2026-01-01 exclusive",
             capture.output(sessionInfo())), "outputs/session_info.txt")

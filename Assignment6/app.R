# BDA400 Assignment 6 | Ali Muhammad | Portfolio dashboard
# Install once: install.packages(c("shiny", "ggplot2", "quantmod"))
# Run from this folder: shiny::runApp()
library(shiny)
library(ggplot2)
library(quantmod)

# 1. Collect daily bars from Yahoo Finance and aggregate the chosen timeframe.
fetch_prices <- function(symbol, dates) {
  stopifnot(length(dates) == 2L, all(!is.na(dates)), dates[1] <= dates[2])
  # Yahoo's end date is exclusive, whereas the dashboard end date is inclusive.
  x <- getSymbols(symbol, src = "yahoo", from = dates[1],
                  to = dates[2] + 1L, auto.assign = FALSE)
  out <- data.frame(Date = as.Date(zoo::index(x)), Open = as.numeric(Op(x)),
                    High = as.numeric(Hi(x)), Low = as.numeric(Lo(x)),
                    Close = as.numeric(Cl(x)), Volume = as.numeric(Vo(x)))
  out <- out[complete.cases(out) & out$Date >= dates[1] & out$Date <= dates[2], ]
  out <- out[order(out$Date), ]
  if (!nrow(out)) stop("No complete trading bars in this date range.")
  out
}

aggregate_prices <- function(x, frame) {
  frame <- match.arg(frame, c("Daily", "Weekly", "Monthly"))
  if (frame == "Daily") return(x)
  key <- format(x$Date, if (frame == "Weekly") "%G-%V" else "%Y-%m")
  groups <- split(seq_len(nrow(x)), key)
  do.call(rbind, lapply(groups, function(i) {
    first <- i[1L]
    last <- tail(i, 1L)
    data.frame(Date = x$Date[last], Open = x$Open[first], High = max(x$High[i]),
               Low = min(x$Low[i]), Close = x$Close[last], Volume = sum(x$Volume[i]))
  }))
}

# 2. Reuse the Assignment 5 calculations inside this standalone script.
# Return one value per complete trailing window.
sma <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  if (length(data) < period) stop("Data length must be at least period.")
  vapply(seq_len(length(data) - period + 1), function(i) {
    mean(data[seq.int(i, length.out = period)])
  }, numeric(1))
}

# Seed with the first observation, as specified in the brief.
ema <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), length(period) == 1L,
            is.finite(period), period >= 1, period == floor(period))
  n <- length(data)
  if (!n) return(numeric())
  alpha <- 2 / (period + 1)
  out <- numeric(n)
  out[1] <- data[1]
  if (n > 1L) for (i in 2:n) out[i] <- out[i-1L] + alpha * (data[i] - out[i-1L])
  out
}

# Requires ema.R. All three lines align with the input.
macd <- function(data, short_period, long_period, signal_period) {
  short <- ema(data, short_period)
  long <- ema(data, long_period)
  if (short_period >= long_period) stop("short_period must be below long_period.")
  line <- short - long
  signal <- ema(line, signal_period)
  list(macd_line = line, signal_line = signal, histogram = line - signal)
}

# Wilder RSI. Seed once from the first period price changes.
rsi <- function(data, period) {
  stopifnot(is.numeric(data), is.null(dim(data)), all(is.finite(data)),
            length(period) == 1L, is.finite(period), period >= 1,
            period == floor(period))
  n <- length(data)
  out <- rep(NA_real_, n)
  if (n <= period) return(out)
  change <- diff(data)
  gain <- pmax(change, 0)
  loss <- pmax(-change, 0)
  g <- mean(gain[seq_len(period)])
  l <- mean(loss[seq_len(period)])
  for (i in seq.int(period + 1, n)) {
    if (i > period + 1) {
      g <- (g * (period - 1) + gain[i - 1]) / period
      l <- (l * (period - 1) + loss[i - 1]) / period
    }
    out[i] <- if (g == 0 && l == 0) 50 else 100 * g / (g + l)
  }
  out
}

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

indicators <- function(x, short_period, long_period) {
  n <- nrow(x)
  aligned_sma <- function(period) {
    if (n < period) return(rep(NA_real_, n))
    c(rep(NA_real_, period - 1L), sma(x$Close, period))
  }
  x$Short <- aligned_sma(short_period)
  x$Long <- aligned_sma(long_period)
  x$RSI <- rsi(x$Close, 14)
  m <- macd(x$Close, 12, 26, 9)
  x$MACD <- m$macd_line
  x$MACDSignal <- m$signal_line
  x$Histogram <- m$histogram
  # 3. Trigger on the crossing bar only. Wait for two valid MA pairs.
  crosses <- crossover(x$Short, x$Long)
  x$Signal <- ifelse(crosses == "Up", "Buy", ifelse(crosses == "Down", "Sell", "Hold"))
  ready <- is.finite(x$Short) & is.finite(x$Long)
  x$Signal[!(ready & c(FALSE, head(ready, -1L)))] <- "Warm-up"
  x
}

# 4. Draw price, overlays and annotations on a common date axis.
# RSI and MACD get their own scales so they do not distort the price axis.
make_chart <- function(x, chart, overlays, annotations, title, short_period, long_period) {
  panels <- c("Price (USD)", intersect(c("RSI", "MACD"), overlays))
  in_panel <- function(data, label) {
    data$Panel <- factor(rep(label, nrow(data)), levels = panels)
    data
  }
  price <- in_panel(x, panels[1L])
  p <- ggplot() + geom_blank(data = price, aes(Date, Close))
  if (chart == "Area") {
    p <- p + geom_area(data = price, aes(Date, Close), fill = "#a1cbe2")
  } else if (chart == "Candlestick") {
    step <- if (nrow(price) > 1L) {
      median(as.numeric(diff(price$Date)))
    } else 1
    price$Direction <- ifelse(price$Close >= price$Open, "Up", "Down")
    price$Left <- price$Date - .27 * step
    price$Right <- price$Date + .27 * step
    p <- p + geom_linerange(data = price, aes(Date, ymin = Low, ymax = High),
                             color = "#475569") +
      geom_rect(data = price, aes(xmin = Left, xmax = Right, ymin = pmin(Open, Close),
                                 ymax = pmax(Open, Close), fill = Direction)) +
      scale_fill_manual(values = c(Up = "#137d65", Down = "#bc4b44"), name = "Candle")
  } else {
    p <- p + geom_line(data = price, aes(Date, Close), color = "#24577b", linewidth = .7)
    if (nrow(price) == 1L) p <- p + geom_point(data = price, aes(Date, Close), color = "#24577b")
  }
  add_line <- function(plot, data, column, label) {
    data$Value <- data[[column]]
    data$Series <- rep(label, nrow(data))
    plot + geom_line(data = data, aes(Date, Value, color = Series),
                     linewidth = .75, na.rm = TRUE)
  }
  short_name <- paste("SMA", short_period)
  long_name <- paste("SMA", long_period)
  if ("Moving averages" %in% overlays) {
    p <- add_line(p, price, "Short", short_name)
    p <- add_line(p, price, "Long", long_name)
  }
  if ("RSI" %in% overlays) {
    r <- in_panel(x, "RSI")
    levels <- in_panel(data.frame(Guide = c(30, 70)), "RSI")
    p <- p + geom_hline(data = levels, aes(yintercept = Guide),
                        linetype = "dashed", color = "#64748b")
    p <- add_line(p, r, "RSI", "RSI 14")
  }
  if ("MACD" %in% overlays) {
    m <- in_panel(x, "MACD")
    p <- p + geom_col(data = m, aes(Date, Histogram), fill = "#d0dce6", alpha = .85)
    p <- add_line(p, m, "MACD", "MACD")
    p <- add_line(p, m, "MACDSignal", "Signal line")
  }
  events <- price[price$Signal %in% c("Buy", "Sell"), ]
  if (annotations && nrow(events)) {
      events$Anchor <- ifelse(events$Signal == "Buy", events$Low, events$High)
      events$Justify <- ifelse(events$Signal == "Buy", 1.4, -.7)
      p <- p + geom_point(data = events, aes(Date, Anchor, color = Signal), size = 2.1) +
        geom_text(data = events, aes(Date, Anchor, color = Signal, label = Signal,
                                     vjust = Justify), size = 3, show.legend = FALSE)
  }
  colors <- setNames(c("#16766c", "#a76517", "#71489b", "#24577b",
                       "#b04e76", "#137d65", "#bc4b44"),
                     c(short_name, long_name, "RSI 14", "MACD", "Signal line", "Buy", "Sell"))
  if (length(overlays) || (annotations && nrow(events))) {
    p <- p + scale_color_manual(values = colors, name = NULL)
  }
  p + facet_grid(Panel ~ ., scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(.13, .15))) +
    labs(title = title, x = NULL, y = NULL, caption = "Source: Yahoo Finance via quantmod") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 16))
}

# 5. Connect user controls, Yahoo data, plots and the current signal.
ui <- fluidPage(
  tags$head(tags$style(HTML(
    "body{background:#f3f6fa;color:#18283b} .well{background:#fff;border:1px solid #dce5ee}
     .chart{background:#fff;padding:16px;border:1px solid #dce5ee;border-radius:6px}
     .state{font-weight:600;margin:12px 0} .hint{color:#485c73}"))),
  titlePanel("Portfolio dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Stock", c("AAPL", "MSFT", "GOOG")),
      dateRangeInput("dates", "Trading dates", start = "2023-01-01", end = "2025-12-31"),
      actionButton("fetch", "Load Yahoo data", class = "btn-primary"),
      tags$hr(),
      selectInput("frame", "Timeframe", c("Daily", "Weekly", "Monthly")),
      selectInput("chart", "Price chart", c("Line", "Candlestick", "Area")),
      checkboxGroupInput("overlays", "Indicators", c("Moving averages", "RSI", "MACD"),
                         selected = c("Moving averages", "RSI", "MACD")),
      numericInput("short", "Short SMA (bars)", value = 20, min = 1, step = 1),
      numericInput("long", "Long SMA (bars)", value = 50, min = 2, step = 1),
      checkboxInput("annotate", "Show Buy and Sell labels", TRUE)
    ),
    mainPanel(
      div(class = "chart", plotOutput("stock_chart", height = "auto"),
          div(class = "state", textOutput("state")),
          p(class = "hint", "Buy and Sell mark crossings. Hold means no new crossing. Warm-up means there is insufficient history for a comparison. Periods count bars in the selected timeframe."),
          tableOutput("recent"))
    )
  )
)

server <- function(input, output, session) {
  query <- eventReactive(input$fetch, {
    dates <- input$dates
    validate(need(length(dates) == 2L && all(!is.na(dates)) && dates[1] <= dates[2],
                  "Choose a valid date range, then load data."))
    result <- tryCatch(fetch_prices(input$symbol, dates), error = function(e) e)
    validate(need(!inherits(result, "error"),
                  if (inherits(result, "error")) paste("Yahoo download failed:", conditionMessage(result))))
    list(symbol = input$symbol, prices = result)
  }, ignoreNULL = FALSE)

  current <- reactive({
    q <- query()
    req(input$frame)
    short <- input$short
    long <- input$long
    good <- function(x) length(x) == 1L && is.finite(x) && x >= 1 && x == floor(x)
    validate(need(good(short) && good(long) && short < long,
                  "Enter positive whole-number SMA periods with short below long."))
    bars <- aggregate_prices(q$prices, input$frame)
    indicators(bars, short, long)
  })
  output$stock_chart <- renderPlot({
    req(input$chart)
    print(make_chart(current(), input$chart, input$overlays, isTRUE(input$annotate),
                     paste(query()$symbol, "|", input$frame), input$short, input$long))
  }, height = function() 430 + 130 * sum(c("RSI", "MACD") %in% input$overlays), res = 96)
  output$state <- renderText({
    last <- tail(current(), 1L)
    paste(query()$symbol, "|", format(last$Date), "|", last$Signal)
  })
  output$recent <- renderTable({
    recent <- tail(current()[, c("Date", "Close", "Short", "Long", "Signal")], 5)
    recent$Date <- format(recent$Date)
    names(recent)[3:4] <- c("Short SMA", "Long SMA")
    recent[rev(seq_len(nrow(recent))), ]
  }, digits = 2, na = "—")
}
shinyApp(ui = ui, server = server)

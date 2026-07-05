# BDA400 Assignment 6
# Technical Analysis using R, Visualization Phase
# Student: Olubunmi Fabanwo

# AI Assistance Declaration:
# I used ChatGPT (GPT-5.5, July 2026) for Shiny dashboard structure,
# R code organization, trading rule logic, documentation, and debugging support.
# I verified the app by running it in RStudio, testing multiple symbols, switching
# chart types, toggling indicators, and checking that buy/sell/hold annotations display.

# Install packages once if needed:
# install.packages(c("shiny", "ggplot2", "quantmod", "dplyr", "zoo", "scales"))

library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)
library(zoo)
library(scales)

############################################################
# Base R helper functions for indicators
############################################################

sma <- function(data, period) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (period <= 0) stop("period must be greater than 0")
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  result <- rep(NA, length(data))
  for (i in period:length(data)) {
    result[i] <- sum(data[(i - period + 1):i], na.rm = TRUE) / period
  }
  return(result)
}

ema <- function(data, period) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (period <= 0) stop("period must be greater than 0")
  result <- rep(NA, length(data))
  result[1] <- data[1]
  multiplier <- 2 / (period + 1)
  for (i in 2:length(data)) {
    result[i] <- (data[i] - result[i - 1]) * multiplier + result[i - 1]
  }
  return(result)
}

rsi <- function(data, period = 14) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (length(data) <= period) return(rep(NA, length(data)))
  changes <- diff(data)
  gains <- ifelse(changes > 0, changes, 0)
  losses <- ifelse(changes < 0, abs(changes), 0)
  avg_gain <- mean(gains[1:period], na.rm = TRUE)
  avg_loss <- mean(losses[1:period], na.rm = TRUE)
  rsi_values <- rep(NA, length(data))
  if (avg_loss == 0) {
    rsi_values[period + 1] <- 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }
  if ((period + 2) <= length(data)) {
    for (i in (period + 2):length(data)) {
      idx <- i - 1
      avg_gain <- ((avg_gain * (period - 1)) + gains[idx]) / period
      avg_loss <- ((avg_loss * (period - 1)) + losses[idx]) / period
      if (avg_loss == 0) {
        rsi_values[i] <- 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }
  return(rsi_values)
}

macd_calc <- function(data, short_period = 12, long_period = 26, signal_period = 9) {
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  return(list(macd_line = macd_line, signal_line = signal_line, histogram = histogram))
}

make_signals <- function(short_ma, long_ma) {
  signals <- rep("Hold", length(short_ma))
  for (i in 2:length(short_ma)) {
    if (is.na(short_ma[i]) || is.na(long_ma[i]) || is.na(short_ma[i - 1]) || is.na(long_ma[i - 1])) {
      signals[i] <- "Hold"
    } else if (short_ma[i] > long_ma[i] && short_ma[i - 1] <= long_ma[i - 1]) {
      signals[i] <- "Buy"
    } else if (short_ma[i] < long_ma[i] && short_ma[i - 1] >= long_ma[i - 1]) {
      signals[i] <- "Sell"
    } else {
      signals[i] <- "Hold"
    }
  }
  return(signals)
}

############################################################
# Shiny UI
############################################################

ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Time Frame", choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
      selectInput("chart_type", "Chart Type", choices = c("Line", "Area", "Candlestick"), selected = "Line"),
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators",
        choices = c("Short Moving Average", "Long Moving Average", "RSI", "MACD"),
        selected = c("Short Moving Average", "Long Moving Average")
      ),
      numericInput("short_period", "Short MA Period", value = 20, min = 2, max = 100),
      numericInput("long_period", "Long MA Period", value = 50, min = 5, max = 250),
      checkboxInput("show_signals", "Show Buy/Sell/Hold Signals", value = TRUE),
      actionButton("load_data", "Load Stock Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Price Chart", plotOutput("stock_chart", height = "600px")),
        tabPanel("RSI", plotOutput("rsi_chart", height = "350px")),
        tabPanel("MACD", plotOutput("macd_chart", height = "350px")),
        tabPanel("Signal Table", tableOutput("signal_table")),
        tabPanel("Data Preview", tableOutput("data_preview"))
      )
    )
  )
)

############################################################
# Shiny Server
############################################################

server <- function(input, output, session) {
  raw_stock <- eventReactive(input$load_data, {
    validate(need(nchar(input$symbol) > 0, "Enter a stock symbol."))
    tryCatch({
      getSymbols(
        Symbols = toupper(input$symbol),
        src = "yahoo",
        from = input$date_range[1],
        to = input$date_range[2],
        auto.assign = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Data fetch failed:", e$message), type = "error")
      return(NULL)
    })
  }, ignoreNULL = FALSE)

  prepared_data <- reactive({
    stock_data <- raw_stock()
    validate(need(!is.null(stock_data), "No data available. Check symbol or date range."))

    if (input$time_frame == "Weekly") {
      stock_data <- to.weekly(stock_data, indexAt = "lastof", drop.time = TRUE)
    } else if (input$time_frame == "Monthly") {
      stock_data <- to.monthly(stock_data, indexAt = "lastof", drop.time = TRUE)
    }

    data.frame(
      Date = as.Date(index(stock_data)),
      Open = as.numeric(Op(stock_data)),
      High = as.numeric(Hi(stock_data)),
      Low = as.numeric(Lo(stock_data)),
      Close = as.numeric(Cl(stock_data)),
      Volume = as.numeric(Vo(stock_data))
    ) %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      mutate(
        ShortMA = sma(Close, input$short_period),
        LongMA = sma(Close, input$long_period),
        RSI = rsi(Close, 14),
        MACD = macd_calc(Close)$macd_line,
        MACDSignal = macd_calc(Close)$signal_line,
        MACDHistogram = macd_calc(Close)$histogram,
        Signal = make_signals(ShortMA, LongMA)
      )
  })

  output$stock_chart <- renderPlot({
    df <- prepared_data()
    validate(need(nrow(df) > 0, "No rows available for this selection."))

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), linewidth = 0.8)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), alpha = 0.35) + geom_line(aes(y = Close), linewidth = 0.7)
    } else {
      p <- p + geom_segment(aes(xend = Date, y = Low, yend = High)) +
        geom_rect(aes(xmin = Date - 0.35, xmax = Date + 0.35, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Close >= Open), alpha = 0.7) +
        scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "firebrick"), guide = "none")
    }

    if ("Short Moving Average" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = ShortMA), linewidth = 0.7, linetype = "dashed")
    }

    if ("Long Moving Average" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = LongMA), linewidth = 0.7, linetype = "dotdash")
    }

    if (isTRUE(input$show_signals)) {
      signal_points <- df %>% filter(Signal %in% c("Buy", "Sell"))
      p <- p + geom_point(data = signal_points, aes(y = Close, shape = Signal), size = 3) +
        geom_text(data = signal_points, aes(y = Close, label = Signal), vjust = -1, size = 3)
    }

    p +
      labs(
        title = paste(toupper(input$symbol), "Stock Price Dashboard"),
        subtitle = paste(input$time_frame, "data with technical indicator overlays"),
        x = "Date",
        y = "Price",
        caption = "Source: Yahoo Finance via quantmod"
      ) +
      scale_x_date(labels = date_format("%b %Y")) +
      theme_minimal()
  })

  output$rsi_chart <- renderPlot({
    df <- prepared_data()
    validate(need("RSI" %in% input$technical_indicators, "Select RSI in Technical Indicators to display this chart."))
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line() +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      labs(title = "Relative Strength Index", x = "Date", y = "RSI") +
      theme_minimal()
  })

  output$macd_chart <- renderPlot({
    df <- prepared_data()
    validate(need("MACD" %in% input$technical_indicators, "Select MACD in Technical Indicators to display this chart."))
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACDHistogram), alpha = 0.5) +
      geom_line(aes(y = MACD), linewidth = 0.8) +
      geom_line(aes(y = MACDSignal), linewidth = 0.8, linetype = "dashed") +
      labs(title = "MACD Indicator", x = "Date", y = "MACD") +
      theme_minimal()
  })

  output$signal_table <- renderTable({
    df <- prepared_data()
    df %>%
      select(Date, Close, ShortMA, LongMA, RSI, MACD, MACDSignal, Signal) %>%
      tail(20)
  }, digits = 2)

  output$data_preview <- renderTable({
    head(prepared_data(), 15)
  }, digits = 2)
}

shinyApp(ui = ui, server = server)

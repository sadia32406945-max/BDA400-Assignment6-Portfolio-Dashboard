# ==============================================================================
# BDA400 / Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Student Name: Sadia
# ==============================================================================

# ==============================================================================
# SECTION 1: DATA COLLECTION AND SETUP
# ==============================================================================
# Install required packages if not already installed:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "gridExtra"))

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(gridExtra)

# Helper function to safely fetch stock data from Yahoo Finance with error handling
fetch_stock_data <- function(symbol, start_date, end_date, time_frame) {
  tryCatch({
    # Fetch daily OHLCV data from Yahoo Finance
    raw_xts <- getSymbols(
      Symbols     = toupper(trimws(symbol)),
      src         = "yahoo",
      from        = start_date,
      to          = end_date,
      auto.assign = FALSE
    )
    
    # Remove any missing values (NA rows)
    raw_xts <- na.omit(raw_xts)
    
    # Aggregate to Weekly or Monthly if selected by the user
    if (time_frame == "Weekly") {
      raw_xts <- to.weekly(raw_xts, indexAt = "lastof", drop.time = TRUE)
    } else if (time_frame == "Monthly") {
      raw_xts <- to.monthly(raw_xts, indexAt = "lastof", drop.time = TRUE)
    }
    
    # Convert xts object into a clean data.frame for ggplot2
    df <- data.frame(
      Date   = as.Date(index(raw_xts)),
      Open   = as.numeric(Op(raw_xts)),
      High   = as.numeric(Hi(raw_xts)),
      Low    = as.numeric(Lo(raw_xts)),
      Close  = as.numeric(Cl(raw_xts)),
      Volume = as.numeric(Vo(raw_xts))
    )
    
    return(df)
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# SECTION 2: VISUALIZING STOCK DATA (SHINY UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis & Trading Signal Dashboard — Sadia"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # 1. Stock & Date Controls
      h4("1. Market Data Settings"),
      textInput("stock_symbol", "Stock Ticker Symbol (Yahoo Finance):", value = "AAPL"),
      helpText("Examples: AAPL, MSFT, TSLA, GOOGL, NVDA, AMZN"),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end   = "2024-01-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices  = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices  = c("Line Chart", "Area Chart", "Candlestick Chart"),
        selected = "Candlestick Chart"
      ),
      
      hr(),
      
      # 2. Technical Indicator Toggles (On/Off)
      h4("2. Technical Indicators"),
      checkboxGroupInput(
        "technical_indicators",
        "Toggle Indicator Overlays:",
        choices  = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI")
      ),
      
      hr(),
      
      # 3. Customizable Trading Rule Parameters (MA Crossover)
      h4("3. MA Crossover Strategy"),
      numericInput("short_ma_n", "Short-Term SMA Period:", value = 20, min = 5, max = 100),
      numericInput("long_ma_n",  "Long-Term SMA Period:",  value = 50, min = 10, max = 250),
      checkboxInput("show_signals", "Annotate Buy/Sell Signals on Chart", value = TRUE),
      
      actionButton("update_btn", "Update Dashboard", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      # Summary status banner showing latest price and current trading signal
      uiOutput("signal_banner"),
      br(),
      # Main multi-panel ggplot output
      plotOutput("stock_chart", height = "720px"),
      br(),
      # Recent trading signals table
      h4("Recent Trading Signals Log"),
      tableOutput("signals_table")
    )
  )
)

# ==============================================================================
# SECTION 3 & 4: SERVER LOGIC, INDICATORS, TRADING RULES & ANNOTATIONS
# ==============================================================================
server <- function(input, output) {
  
  # Reactive data pipeline triggered on startup and when "Update Dashboard" is clicked
  processed_data <- eventReactive(input$update_btn, {
    validate(
      need(input$stock_symbol != "", "Please enter a valid stock ticker symbol."),
      need(input$date_range[1] < input$date_range[2], "Start date must be earlier than end date."),
      need(input$short_ma_n < input$long_ma_n, "Short-Term MA period must be smaller than Long-Term MA period.")
    )
    
    df <- fetch_stock_data(
      symbol     = input$stock_symbol,
      start_date = input$date_range[1],
      end_date   = input$date_range[2],
      time_frame = input$time_frame
    )
    
    validate(
      need(!is.null(df) && nrow(df) > input$long_ma_n,
           "Unable to fetch sufficient data. Check the ticker symbol or widen the date range.")
    )
    
    # --- STEP 3: CALCULATE TECHNICAL INDICATORS ---
    # 1. Moving Averages (Short-term & Long-term SMA)
    df$Short_MA <- SMA(df$Close, n = input$short_ma_n)
    df$Long_MA  <- SMA(df$Close, n = input$long_ma_n)
    
    # 2. Relative Strength Index (14-period RSI)
    df$RSI <- RSI(df$Close, n = 14)
    
    # 3. MACD (12, 26, 9 standard parameters)
    macd_vals      <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
    df$MACD        <- macd_vals[, "macd"]
    df$MACD_Signal <- macd_vals[, "signal"]
    df$MACD_Hist   <- df$MACD - df$MACD_Signal
    
    # --- STEP 4: IMPLEMENT TRADING RULES (MA CROSSOVER) ---
    # State condition: Bullish when Short MA > Long MA, Bearish when Short MA < Long MA
    df$Position <- ifelse(
      is.na(df$Short_MA) \vert{} is.na(df$Long_MA), "Hold",
      ifelse(df$Short_MA > df$Long_MA, "Bullish",
             ifelse(df$Short_MA < df$Long_MA, "Bearish", "Hold"))
    )
    
    # Detect exact crossover events to generate actionable Buy / Sell / Hold signals
    prev_short <- c(NA, head(df$Short_MA, -1))
    prev_long  <- c(NA, head(df$Long_MA, -1))
    
    df$Signal <-
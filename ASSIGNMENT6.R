library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "AAPL"
stock_data <- getSymbols(stock_symbol, src = "yahoo",
                         from = "2023-01-01", to = "2023-07-01",
                         auto.assign = FALSE)
ui <- fluidPage(
  titlePanel("Portfolio Dashboard"),
  dateRangeInput("date_range", "Select Date Range:",
                 start = "2023-01-01", end = "2023-07-01"),
  selectInput("time_frame", "Select Time Frame:",
              choices = c("Daily", "Weekly", "Monthly")),
  checkboxGroupInput("technical_indicators", "Technical Indicators:",
                     choices = c("Moving Averages", "RSI", "MACD")),
  plotOutput("stock_chart"),
  plotOutput("rsi_chart"),
  plotOutput("macd_chart")
)

server <- function(input, output) {
  
  # Reactive: filter data by date range and time frame
  filtered_data <- reactive({
    req(input$date_range)
    data <- stock_data[paste0(input$date_range[1], "/", input$date_range[2])]
    if (input$time_frame == "Weekly")  data <- to.weekly(data)
    if (input$time_frame == "Monthly") data <- to.monthly(data)
    data
  })
  
  output$stock_chart <- renderPlot({
    data <- filtered_data()
    
    df <- data.frame(Date  = index(data),
                     Close = as.numeric(Cl(data)))
    
    # Base line chart of closing price
    p <- ggplot(df, aes(x = Date, y = Close)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = paste(stock_symbol, "Stock Price"),
           x = "Date", y = "Price ($)") +
      theme_minimal()
    
    # Overlay Moving Averages when selected
    if ("Moving Averages" %in% input$technical_indicators) {
      df$MA20 <- as.numeric(SMA(df$Close, n = 20))
      df$MA50 <- as.numeric(SMA(df$Close, n = 50))
      p <- p + geom_line(data = df, aes(x = Date, y = MA20), color = "red", na.rm = TRUE) +
        geom_line(data = df, aes(x = Date, y = MA50), color = "darkgreen", na.rm = TRUE)
    }
    
    # MA crossover trading rule: Buy / Sell / Hold
    short_ma <- as.numeric(SMA(df$Close, n = 20))
    long_ma  <- as.numeric(SMA(df$Close, n = 50))
    df$Signal <- ifelse(short_ma > long_ma, "Buy",
                        ifelse(short_ma < long_ma, "Sell", "Hold"))
    # Show labels only at crossover points (where the signal changes)
    prev <- c(NA, head(df$Signal, -1))
    sig <- df[!is.na(df$Signal) & df$Signal != "Hold" &
                (is.na(prev) | df$Signal != prev), ]
    
    # Annotate Buy/Sell signals on the chart
    p <- p + geom_point(data = sig,
                        aes(x = Date, y = Close, color = Signal), size = 2.5) +
      geom_text(data = sig,
                aes(x = Date, y = Close, label = Signal),
                vjust = -1, size = 3.5, show.legend = FALSE) +
      scale_color_manual(values = c("Buy" = "darkgreen",
                                    "Sell" = "red"))
    print(p)
  })
  # RSI panel (own scale 0-100, shown when RSI is ticked)
  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    data <- filtered_data()
    df <- data.frame(Date  = index(data),
                     Close = as.numeric(Cl(data)))
    df$RSI <- as.numeric(RSI(df$Close, n = 14))
    
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "purple", linewidth = 1, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
      labs(title = "RSI (14)", x = "Date", y = "RSI") +
      theme_minimal()
  })
  
  # MACD panel (shown when MACD is ticked)
  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    data <- filtered_data()
    df <- data.frame(Date  = index(data),
                     Close = as.numeric(Cl(data)))
    macd_vals <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9)
    df$MACD_Line <- as.numeric(macd_vals[, "macd"])
    df$MACD_Signal <- as.numeric(macd_vals[, "signal"])
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD_Line), color = "blue", linewidth = 1, na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal), color = "red", na.rm = TRUE) +
      labs(title = "MACD", x = "Date", y = "Value") +
      theme_minimal()
  })
}
shinyApp(ui, server)


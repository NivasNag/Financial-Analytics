library(shiny)
library(quantmod)
library(forecast)
library(PerformanceAnalytics)
library(ggplot2)

nifty50_stocks <- c("RELIANCE.NS", "TCS.NS", "INFY.NS", "HDFCBANK.NS", "ICICIBANK.NS", "HINDUNILVR.NS", 
                    "ITC.NS", "KOTAKBANK.NS", "LT.NS", "AXISBANK.NS", "SBIN.NS", "BHARTIARTL.NS", 
                    "BAJFINANCE.NS", "ASIANPAINT.NS", "HCLTECH.NS", "MARUTI.NS", "ULTRACEMCO.NS", 
                    "TITAN.NS", "SUNPHARMA.NS", "HDFCLIFE.NS", "WIPRO.NS", "BAJAJFINSV.NS", "ONGC.NS", 
                    "TATASTEEL.NS", "POWERGRID.NS", "ADANIPORTS.NS", "NTPC.NS", "INDUSINDBK.NS", 
                    "DRREDDY.NS", "DIVISLAB.NS", "HDFC.NS", "JSWSTEEL.NS", "HEROMOTOCO.NS", "GRASIM.NS", 
                    "NESTLEIND.NS", "SBILIFE.NS", "TECHM.NS", "CIPLA.NS", "BAJAJ-AUTO.NS", "EICHERMOT.NS", 
                    "M&M.NS", "BRITANNIA.NS", "COALINDIA.NS", "BPCL.NS", "SHREECEM.NS", "IOC.NS", 
                    "APOLLOHOSP.NS", "TATAMOTORS.NS", "UPL.NS", "ADANIENT.NS")

ui <- fluidPage(
  titlePanel("Stock Prediction for Nifty50 Stocks"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock", 
                  choices = c("", nifty50_stocks), 
                  selected = ""),
      sliderInput("forecast_days", "Number of Forecast Days", 
                  min = 1, max = 365, value = 100),
      selectInput("method", "Prediction Method", 
                  choices = c("ARIMA", "Random Walk"), selected = "ARIMA"),
      actionButton("run", "Run Forecast"),
      downloadButton("downloadPlot", "Download PDF")
    ),
    mainPanel(
      plotOutput("stockPlot"),
      plotOutput("forecastPlot")
    )
  )
)

server <- function(input, output, session) {
  
  stock_data <- reactive({
    req(input$stock)
    tryCatch({
      stock_data <- getSymbols(input$stock, from = "2020-01-01", to = "2024-10-01", auto.assign = FALSE)
      stock_prices <- Cl(stock_data)
      return(stock_prices)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to fetch data for", input$stock, ": ", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
  })
  
  stock_returns <- reactive({
    stock_prices <- stock_data()
    if (is.null(stock_prices)) return(NULL)
    returns <- Return.calculate(stock_prices, method = "discrete")
    return(na.omit(returns))
  })
  
  output$stockPlot <- renderPlot({
    req(input$run)
    req(input$stock)
    stock_prices <- stock_data()
    if (is.null(stock_prices)) return(NULL)
    plot(stock_prices, 
         main = paste(input$stock, "Stock Prices"), 
         ylab = "Price", 
         xlab = "Date", 
         col = "blue", 
         lwd = 2, 
         grid = TRUE)
  })
  
  observeEvent(input$run, {
    req(input$stock)
    output$forecastPlot <- renderPlot({
      stock_prices <- stock_data()
      if (is.null(stock_prices)) return(NULL)
      
      if (input$method == "ARIMA") {
        arima_model <- auto.arima(stock_prices)
        forecast_prices <- forecast(arima_model, h = input$forecast_days)
        
        plot(forecast_prices, 
             main = paste("ARIMA Forecast for", input$stock, "Stock Price"),
             ylab = "Price", 
             xlab = "Date", 
             col = "red", 
             lwd = 2)
        
        polygon(c(forecast_prices$mean, rev(forecast_prices$mean + forecast_prices$lower[,2])), 
                c(forecast_prices$mean, rev(forecast_prices$upper[,2])), 
                col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
        
      } else if (input$method == "Random Walk") {
        stock_returns_data <- stock_returns()
        if (is.null(stock_returns_data)) return(NULL)
        mu <- mean(stock_returns_data)
        sigma <- sd(stock_returns_data)
        
        n <- input$forecast_days
        random_walk <- numeric(n)
        random_walk[1] <- as.numeric(last(stock_prices))
        set.seed(123)
        
        for (i in 2:n) {
          random_walk[i] <- random_walk[i - 1] * (1 + mu + sigma * rnorm(1))
        }
        
        future_dates <- seq(as.Date("2024-10-21"), by = "days", length.out = n)
        random_walk_df <- data.frame(Date = future_dates, Price = random_walk)
        
        ggplot(random_walk_df, aes(x = Date, y = Price)) +
          geom_line(colour = "blue", size = 1.5) +
          ggtitle(paste("Random Walk Model Simulation for", input$stock, "Stock Prices")) +
          xlab("Date") + 
          ylab("Price") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_smooth(method = "loess", color = "red", size = 1, linetype = "dashed") +
          annotate("text", x = as.Date("2024-10-21") + n/2, y = tail(random_walk, 1), 
                   label = paste("Last Price:", round(tail(random_walk, 1), 2)), hjust = 0.5, vjust = -1)
      }
    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$stock, "Forecast Plot.pdf", sep = "_")
    },
    content = function(file) {
      pdf(file)
      stock_prices <- stock_data()
      if (is.null(stock_prices)) return(NULL)
      
      if (input$method == "ARIMA") {
        arima_model <- auto.arima(stock_prices)
        forecast_prices <- forecast(arima_model, h = input$forecast_days)
        
        plot(forecast_prices, 
             main = paste("ARIMA Forecast for", input$stock, "Stock Price"),
             ylab = "Price", 
             xlab = "Date", 
             col = "red", 
             lwd = 2)
        
        polygon(c(forecast_prices$mean, rev(forecast_prices$mean + forecast_prices$lower[,2])), 
                c(forecast_prices$mean, rev(forecast_prices$upper[,2])), 
                col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
        
      } else if (input$method == "Random Walk") {
        stock_returns_data <- stock_returns()
        if (is.null(stock_returns_data)) return(NULL)
        mu <- mean(stock_returns_data)
        sigma <- sd(stock_returns_data)
        
        n <- input$forecast_days
        random_walk <- numeric(n)
        random_walk[1] <- as.numeric(last(stock_prices))
        set.seed(123)
        
        for (i in 2:n) {
          random_walk[i] <- random_walk[i - 1] * (1 + mu + sigma * rnorm(1))
        }
        
        future_dates <- seq(as.Date("2024-10-21"), by = "days", length.out = n)
        random_walk_df <- data.frame(Date = future_dates, Price = random_walk)
        
        p <- ggplot(random_walk_df, aes(x = Date, y = Price)) +
          geom_line(colour = "blue", size = 1.5) +
          ggtitle(paste("Random Walk Model Simulation for", input$stock, "Stock Prices")) +
          xlab("Date") + 
          ylab("Price") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_smooth(method = "loess", color = "red", size = 1, linetype = "dashed") +
          annotate("text", x = as.Date("2024-10-21") + n/2, y = tail(random_walk, 1), 
                   label = paste("Last Price:", round(tail(random_walk, 1), 2)), hjust = 0.5, vjust = -1)
        
        print(p)
      }
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
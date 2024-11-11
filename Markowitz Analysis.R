library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(timeSeries)
library(gridExtra)

symbols <- c("RELIANCE.NS", "TCS.NS", "INFY.NS", "HDFCBANK.NS", "ICICIBANK.NS", "HINDUNILVR.NS", 
             "ITC.NS", "KOTAKBANK.NS", "LT.NS", "AXISBANK.NS", "SBIN.NS", "BHARTIARTL.NS", 
             "BAJFINANCE.NS", "ASIANPAINT.NS", "HCLTECH.NS", "MARUTI.NS", "ULTRACEMCO.NS", 
             "TITAN.NS", "SUNPHARMA.NS", "HDFCLIFE.NS", "WIPRO.NS", "BAJAJFINSV.NS", "ONGC.NS", 
             "TATASTEEL.NS", "POWERGRID.NS", "ADANIPORTS.NS", "NTPC.NS", "INDUSINDBK.NS", 
             "DRREDDY.NS", "DIVISLAB.NS", "HDFC.NS", "JSWSTEEL.NS", "HEROMOTOCO.NS", "GRASIM.NS", 
             "NESTLEIND.NS", "SBILIFE.NS", "TECHM.NS", "CIPLA.NS", "BAJAJ-AUTO.NS", "EICHERMOT.NS", 
             "M&M.NS", "BRITANNIA.NS", "COALINDIA.NS", "BPCL.NS", "SHREECEM.NS", "IOC.NS", 
             "APOLLOHOSP.NS", "TATAMOTORS.NS", "UPL.NS", "ADANIENT.NS")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f0f8ff; font-family: 'Arial', sans-serif; }
    .panel { background-color: #ffebcd; border-radius: 10px; padding: 20px; }
    .btn-primary { background-color: #4CAF50; border-color: #4CAF50; color: white; }
    .btn-primary:hover { background-color: #45a049; }
    .title { color: #ff6347; font-size: 28px; font-weight: bold; }
    .output-text { color: #4682b4; font-size: 16px; }
    .header { text-align: center; font-size: 30px; font-weight: bold; color: #2e8b57; }
    .subtitle { text-align: center; font-size: 20px; color: #8b4513; }
    .button-group { margin-bottom: 20px; } 
    .download-button { margin-top: 20px; }  
    .portfolio-return { font-size: 24px; font-weight: bold; color: #2e8b57; }
  "))),
  
  div(class = "header", "Markowitz Portfolio Analysis for Nifty 50 Stocks"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "subtitle", "Select at least two stocks from the Nifty 50 list:"),
      selectInput("selectedStocks", "Select Stocks for Analysis:", symbols, selected = NULL, multiple = TRUE, selectize = TRUE),
      dateRangeInput("dateRange", "Select Date Range:", start = Sys.Date() - 365 * 3, end = Sys.Date()),
      sliderInput("riskFreeRate", "Risk-Free Rate (%)", 0, 15, 7, 0.5),
      numericInput("investmentAmount", "Investment Amount (INR):", 100000, 0, 10000000, 10000),
      selectInput("portfolioType", "Select Portfolio Type:", c("Optimal Portfolio", "Minimum Risk Portfolio", "Equal Weighted Portfolio")),
      
      div(class = "button-group",
          actionButton("submit", "Run Analysis", class = "btn-primary")
      )
    ),
    
    mainPanel(
      div(class = "output-text", textOutput("selectedOutput")),
      plotOutput("efficientFrontierPlot"),
      tableOutput("portfolioTable"),
      div(class = "portfolio-return", textOutput("portfolioReturn")), 
      div(class = "portfolio-return", textOutput("portfolioReturnAmount")) 
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    if (length(input$selectedStocks) < 2) {
      showModal(modalDialog(title = "Error", "Please select at least two stocks for analysis.", easyClose = TRUE))
    }
  })
  
  output$selectedOutput <- renderText({
    paste("Selected Stocks:", paste(input$selectedStocks, collapse = ", "), "| Risk-Free Rate:", input$riskFreeRate, "%")
  })
  
  output$efficientFrontierPlot <- renderPlot({
    req(input$submit)
    if (length(input$selectedStocks) >= 2) {
      stock_data <- lapply(input$selectedStocks, function(X) getSymbols(X, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE))
      stock_data_df <- do.call(merge, lapply(stock_data, Ad))
      stock_returns <- na.omit(Return.calculate(stock_data_df))
      efficient_frontier <- portfolioFrontier(as.timeSeries(stock_returns), `setRiskFreeRate<-`(portfolioSpec(), input$riskFreeRate / 100 / 252), constraints = "longOnly")
      plot(efficient_frontier, c(1, 2, 3, 5, 7, 8))
    } else {
      plot(1, type = "n", main = "Please select at least two stocks for analysis", xlab = "", ylab = "")
    }
  })
  
  output$portfolioTable <- renderTable({
    req(input$submit)
    if (length(input$selectedStocks) >= 2) {
      stock_data <- lapply(input$selectedStocks, function(X) getSymbols(X, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE))
      stock_data_df <- do.call(merge, lapply(stock_data, Ad))
      stock_returns <- na.omit(Return.calculate(stock_data_df))
      
      if (input$portfolioType == "Optimal Portfolio") {
        portfolio <- tangencyPortfolio(as.timeSeries(stock_returns), `setRiskFreeRate<-`(portfolioSpec(), input$riskFreeRate / 100 / 252), constraints = "longOnly")
      } else if (input$portfolioType == "Minimum Risk Portfolio") {
        portfolio <- minriskPortfolio(as.timeSeries(stock_returns), constraints = "longOnly")
      } else if (input$portfolioType == "Equal Weighted Portfolio") {
        n <- length(input$selectedStocks)
        equal_weights <- rep(1 / n, n)  
        portfolio_table <- data.frame(Stock = input$selectedStocks, Weights = round(equal_weights, 4))
        portfolio_table$InvestmentAmount <- round(equal_weights * input$investmentAmount, 2)
        return(portfolio_table)
      }
      
      if (input$portfolioType != "Equal Weighted Portfolio") {
        weights <- getWeights(portfolio)
        investment_distribution <- round(weights * input$investmentAmount, 2)
        data.frame(Stock = names(weights), Weights = round(weights, 4), InvestmentAmount = investment_distribution)
      }
    }
  })
  
  output$portfolioReturn <- renderText({
    req(input$submit)
    if (length(input$selectedStocks) >= 2) {
      stock_data <- lapply(input$selectedStocks, function(X) getSymbols(X, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE))
      stock_data_df <- do.call(merge, lapply(stock_data, Ad))
      stock_returns <- na.omit(Return.calculate(stock_data_df))
      
      if (input$portfolioType == "Optimal Portfolio") {
        portfolio <- tangencyPortfolio(as.timeSeries(stock_returns), `setRiskFreeRate<-`(portfolioSpec(), input$riskFreeRate / 100 / 252), constraints = "longOnly")
      } else if (input$portfolioType == "Minimum Risk Portfolio") {
        portfolio <- minriskPortfolio(as.timeSeries(stock_returns), constraints = "longOnly")
      } else if (input$portfolioType == "Equal Weighted Portfolio") {
        n <- length(input$selectedStocks)
        equal_weights <- rep(1 / n, n)  # Equal weights for each stock
        portfolio_return <- sum(equal_weights * colMeans(stock_returns)) * 252  
        return(paste("Portfolio Return: ", round(portfolio_return * 100, 2), "%"))
      }
      
      portfolio_return <- sum(getWeights(portfolio) * colMeans(stock_returns)) * 252  
      return(paste("Portfolio Return: ", round(portfolio_return * 100, 2), "%"))
    } else {
      "Select at least two stocks to calculate portfolio return."
    }
  })
  
  output$portfolioReturnAmount <- renderText({
    req(input$submit)
    if (length(input$selectedStocks) >= 2) {
      stock_data <- lapply(input$selectedStocks, function(X) getSymbols(X, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE))
      stock_data_df <- do.call(merge, lapply(stock_data, Ad))
      stock_returns <- na.omit(Return.calculate(stock_data_df))
      
      if (input$portfolioType == "Optimal Portfolio") {
        portfolio <- tangencyPortfolio(as.timeSeries(stock_returns), `setRiskFreeRate<-`(portfolioSpec(), input$riskFreeRate / 100 / 252), constraints = "longOnly")
      } else if (input$portfolioType == "Minimum Risk Portfolio") {
        portfolio <- minriskPortfolio(as.timeSeries(stock_returns), constraints = "longOnly")
      } else if (input$portfolioType == "Equal Weighted Portfolio") {
        n <- length(input$selectedStocks)
        equal_weights <- rep(1 / n, n)
        portfolio_return <- sum(equal_weights * colMeans(stock_returns)) * 252
        portfolio_amount <- input$investmentAmount * (portfolio_return + 1)
        return(paste("Portfolio Return in INR: ₹", format(round(portfolio_amount - input$investmentAmount, 2), big.mark = ","), sep = ""))
      }
      
      portfolio_return <- sum(getWeights(portfolio) * colMeans(stock_returns)) * 252
      portfolio_amount <- input$investmentAmount * (portfolio_return + 1)
      return(paste("Portfolio Return in INR: ₹", format(round(portfolio_amount - input$investmentAmount, 2), big.mark = ","), sep = ""))
    } else {
      "Select at least two stocks to calculate portfolio return in INR."
    }
  })
}

shinyApp(ui = ui, server = server)
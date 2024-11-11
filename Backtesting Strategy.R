library(quantmod)
library(PerformanceAnalytics)
library(fPortfolio)
library(dplyr)
library(writexl)
library(ggplot2)


# Backtesting of SMA Trading Strategy
SMA_Trading_Function <- function(symbol, n1 = 10, n2 = 50) {
  
  # Step 1: Historical stock data
  Stock <- getSymbols(symbol, from = "2022-04-01", auto.assign = FALSE)
  Stock <- na.locf(Stock)
  
  # Step 2: SMA Calculation
  SMA_N1 <- SMA(Ad(Stock), n = n1)
  SMA_N2 <- SMA(Ad(Stock), n = n2)
  
  # Step 3: SMA Trading Signal
  SMA_Trading_Signal <- lag(ifelse((SMA_N1 > SMA_N2) & lag(SMA_N1) < lag(SMA_N2), 1, ifelse((SMA_N1 < SMA_N2) & lag(SMA_N1) > lag(SMA_N2), -1, 0)))
  SMA_Trading_Signal[is.na(SMA_Trading_Signal)] <- 0
  
  # Step 4: SMA Trading Positions
  Changeover <- 0
  SMA_Trading_Positions <- vector("numeric", 0)
  for (i in 1:nrow(Stock)) {
    if (SMA_Trading_Signal[i] == 1) {
      SMA_Trading_Positions[i] <- 1
      Changeover <- 1
    } else if (SMA_Trading_Signal[i] == -1) {
      SMA_Trading_Positions[i] <- 0
      Changeover <- 0
    } else {
      SMA_Trading_Positions[i] <- Changeover
    }
  }
  
  # Step 5: Calculation of Daily and Strategy Returns
  Daily_Returns <- Return.calculate(Ad(Stock), method = "discrete")
  Daily_Returns[is.na(Daily_Returns)] <- 0
  Strategy_Returns <- Daily_Returns * SMA_Trading_Positions
  
  # Step 6: Calculation of Cummulative Daily and Strategy Returns
  Cumulative_Daily_Return <- cumprod(1 + Daily_Returns) - 1
  Cumulative_Strategy_Returns <- cumprod(1 + Strategy_Returns) - 1
  
  #Step 7: Output Summary Statistics
  cat("Annualised Return (Buy and Hold):", Return.annualized(Daily_Returns),"\n")
  cat("Annualized Return (Strategy):", Return.annualized(Strategy_Returns), "\n")
  cat("Sharpe Ratio (Strategy):", SharpeRatio.annualized(Strategy_Returns), "\n")
  cat("Maximum Drawdown (Strategy):", maxDrawdown(Strategy_Returns), "\n")
}

# Backtesting of EMA Trading Strategy
EMA_Trading_Function <- function(symbol, n1 = 5, n2 = 21) {
  
  # Step 1: Historical stock data
  Stock <- getSymbols(symbol, from = "2022-04-01", auto.assign = FALSE)
  Stock <- na.locf(Stock)
  
  # Step 2: EMA5 and EMA21
  EMA_N1 <- EMA(Ad(Stock), n = n1)
  EMA_N2 <- EMA(Ad(Stock), n = n2)
  
  # Step 3: EMA Trading Signal
  EMA_Trading_Signal <- lag(ifelse((EMA_N1 > EMA_N2) & lag(EMA_N1) < lag(EMA_N2), 1, ifelse((EMA_N1 < EMA_N2) & lag(EMA_N1) > lag(EMA_N2), -1, 0)))
  EMA_Trading_Signal[is.na(EMA_Trading_Signal)] <- 0
  
  # Step 4: EMA Trading Position
  Changeover <- 0
  EMA_Trading_Positions <- vector("numeric",0)
  for (i in 1:nrow(Stock)) {
    if (EMA_Trading_Signal[i] == 1) {
      EMA_Trading_Positions[i] <- 1 
      Changeover <- 1
    } else if (EMA_Trading_Signal[i] == -1) {
      EMA_Trading_Positions[i] <- 0
      Changeover <- 0
    } else {
      EMA_Trading_Positions[i] <- Changeover
    }
  }
  
  # Step 5: Calculation of Daily and Strategy Returns
  Daily_Returns <- Return.calculate(Ad(Stock), method = "discrete")
  Daily_Returns[is.na(Daily_Returns)] <- 0
  Strategy_Returns <- Daily_Returns * EMA_Trading_Positions
  
  # Step 6: Calculation of Cummulative Daily and Strategy Returns
  Cumulative_Daily_Return <- cumprod(1 + Daily_Returns) - 1
  Cumulative_Strategy_Returns <- cumprod(1 + Strategy_Returns) - 1
  
  #Step 7: Output Summary Statistics
  cat("Annualised Return (Buy and Hold):", Return.annualized(Daily_Returns),"\n")
  cat("Annualized Return (Strategy):", Return.annualized(Strategy_Returns), "\n")
  cat("Sharpe Ratio (Strategy):", SharpeRatio.annualized(Strategy_Returns), "\n")
  cat("Maximum Drawdown (Strategy):", maxDrawdown(Strategy_Returns), "\n")
}


# Backtesting of MACD Trading Strategy
MACD_Trading_Function <- function(symbol) {
  
  # Step 1: Historical stock data
  Stock <- getSymbols(symbol, from = "2022-04-01", auto.assign = FALSE)
  Stock <- na.locf(Stock)
  
  # Step 2: MACD Calculation
  MACD_Signal <- MACD(Ad(Stock), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)
  MACD_Signal[is.na(MACD_Signal)] <- 0
  MACD_Line <- MACD_Signal[, 1]
  Signal_Line <- MACD_Signal[, 2]
  
  # Step 3: MACD Trading Signal
  MACD_Trading_Signal <- lag(ifelse((MACD_Line > Signal_Line) & lag(MACD_Line) < lag(Signal_Line), 1, 
                                    ifelse((MACD_Line < Signal_Line) & lag(MACD_Line) > lag(Signal_Line), -1, 0)))
  MACD_Trading_Signal[is.na(MACD_Trading_Signal)] <- 0
  
  # Step 4: MACD Trading Positions
  Changeover <- 0
  MACD_Trading_Positions <- vector("numeric", 0)
  for (i in 1:nrow(Stock)) {
    if (MACD_Trading_Signal[i] == 1) {
      MACD_Trading_Positions[i] <- 1 
      Changeover <- 1
    } else if (MACD_Trading_Signal[i] == -1) {
      MACD_Trading_Positions[i] <- 0
      Changeover <- 0
    } else {
      MACD_Trading_Positions[i] <- Changeover
    }
  }
  
  # Step 5: Calculation of Daily and Strategy Returns
  Daily_Returns <- Return.calculate(Ad(Stock), method = "discrete")
  Daily_Returns[is.na(Daily_Returns)] <- 0
  Strategy_Returns <- Daily_Returns * MACD_Trading_Positions
  
  # Step 6: Calculation of Cumulative Returns
  Cumulative_Daily_Return <- cumprod(1 + Daily_Returns) - 1
  Cumulative_Strategy_Returns <- cumprod(1 + Strategy_Returns) - 1
  
  #Step 7: Output Summary Statistics
  cat("Annualised Return (Buy and Hold):", Return.annualized(Daily_Returns),"\n")
  cat("Annualized Return (Strategy):", Return.annualized(Strategy_Returns), "\n")
  cat("Sharpe Ratio (Strategy):", SharpeRatio.annualized(Strategy_Returns), "\n")
  cat("Maximum Drawdown (Strategy):", maxDrawdown(Strategy_Returns), "\n")
}


# Backtesting of RSI Trading Strategy
RSI_Trading_Function <- function(symbol, RSI_N = 14, overbought = 70, oversold = 30) {
  
  # Step 1: Historical stock data
  Stock <- getSymbols(symbol, from = "2022-04-01", auto.assign = FALSE)
  Stock <- na.locf(Stock)
  
  # Step 2: RSI Calculation
  RSI_Values <- RSI(Ad(Stock), n = RSI_N)
  
  # Step 3: RSI Trading Signal
  RSI_Trading_Signal <- lag(ifelse(RSI_Values > overbought, -1, ifelse(RSI_Values < oversold, 1, 0)))
  RSI_Trading_Signal[is.na(RSI_Trading_Signal)] <- 0
  
  # Step 4: RSI Trading Positions
  Changeover <- 0
  RSI_Trading_Positions <- vector("numeric", 0)
  for (i in 1:nrow(Stock)) {
    if (RSI_Trading_Signal[i] == 1) {
      RSI_Trading_Positions[i] <- 1
      Changeover <- 1
    } else if (RSI_Trading_Signal[i] == -1) {
      RSI_Trading_Positions[i] <- 0
      Changeover <- 0
    } else {
      RSI_Trading_Positions[i] <- Changeover
    }
  }
  
  # Step 5: Calculation of Daily and Strategy Returns
  Daily_Returns <- Return.calculate(Ad(Stock), method = "discrete")
  Daily_Returns[is.na(Daily_Returns)] <- 0
  Strategy_Returns <- Daily_Returns * RSI_Trading_Positions
  
  # Step 6: Calculation of Cumulative Returns
  Cumulative_Daily_Return <- cumprod(1 + Daily_Returns) - 1
  Cumulative_Strategy_Returns <- cumprod(1 + Strategy_Returns) - 1
  
  #Step 7: Output Summary Statistics
  cat("Annualised Return (Buy and Hold):", Return.annualized(Daily_Returns),"\n")
  cat("Annualized Return (Strategy):", Return.annualized(Strategy_Returns), "\n")
  cat("Sharpe Ratio (Strategy):", SharpeRatio.annualized(Strategy_Returns), "\n")
  cat("Maximum Drawdown (Strategy):", maxDrawdown(Strategy_Returns), "\n")
}


# Switch statement to call the appropriate strategy
Result <- function(Choice, symbol) {
  The_Result <- switch(as.character(Choice),
                       "1" = {SMA_Trading_Function(symbol, 20, 50)},
                       "2" = {EMA_Trading_Function(symbol, 5, 21)},
                       "3" = {MACD_Trading_Function(symbol)}, 
                       "4" = {RSI_Trading_Function(symbol)},
                       stop("Invalid indicator choice"))
  return(The_Result)
}

# User input loop
for (i in 1) {
  print("Please enter the Stock name as in Yahoo finance")
  symbol <- scan(what = "")
  
  print("Choose the type of startegy:")
  print("1 - SMA strategy with period 20 and 50") 
  print("2 - EMA strategy with shorter period as 5 and longer period as 21")
  print("3 - MACD strategy")
  print("4 - RSI strategy")
  
  Strategy <- scan(what = "")
  
  # Call the Result function with the selected strategy
  Result(Strategy, symbol)
}





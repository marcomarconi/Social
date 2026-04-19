library(tidyverse)  # For getting stock data
library(quantmod)  # For getting stock data
library(TTR)       # For moving averages
library(GA)        # Genetic Algorithm
getSymbols("^GSPC", from = "2000-01-01", to = "2024-01-01", src = "yahoo")
sp500 <- as.vector(Cl(GSPC))   # Closing prices
sp500 <- unlist(gbm_vec(1, 100000))
sp500_train <- sp500[1:(length(sp500)/2)]

# Define the Trading Strategy Using Sharpe Ratio
trade_strategy <- function(params) {
    x <- sp500_train
    
    short_period <- round(params[1])
    long_period  <- round(params[2])
    rsi_period  <- round(params[3])
    dvi_period  <- round(params[4])
    
    if (short_period >= long_period) return(-Inf)  # Avoid invalid parameter sets
    
    short_ma <- EMA(x, n = short_period)
    long_ma  <- EMA(x, n = long_period)
    rsi  <- RSI(x, n = rsi_period, maType = SMA)
    dvi  <- DVI(x, n = dvi_period)[,3]
    
    signal_ma <- ifelse(short_ma > long_ma, 1, -1)  # 1: Buy, -1: Sell
    signal <- ifelse(signal_ma > 0 & rsi > 50 , 1, ifelse(signal_ma < 0 & rsi < 50 , -1, 0)) %>% as.vector()
    returns <- c(0, diff(log(x)))
    daily_returns <- returns * dplyr::lag(signal,1)  # Apply lag to avoid lookahead bias
    daily_returns <- na.omit(daily_returns)  # Remove NA values
    
    if (length(daily_returns) < 2) return(-Inf)  # Avoid division by zero
    
    mean_return <- mean(daily_returns)
    sd_return <- sd(daily_returns)
    
    sharpe_ratio <- ifelse(sd_return > 0, mean_return / sd_return, -Inf)  # Sharpe Ratio
    
    return(sharpe_ratio)  # Higher is better
}

#set.seed(123)  # Ensure reproducibility

ga_optimization <- ga(
    type = "real-valued",
    fitness = function(x) trade_strategy(x),  # Fitness function
    lower = c(5, 10, 50, 30),   # Lower bounds for short and long MA
    upper = c(50, 200, 500, 200), # Upper bounds
    popSize = 50,       # Population size
    maxiter = 50,      # Number of generations
    pcrossover = 0.8,   # Crossover probability (fix)
    pmutation = 0.1     # Mutation probability (fix)
)


summary(ga_optimization)
best_params <- ga_optimization@solution
print(paste("Best Short MA:", round(best_params[1])))
print(paste("Best Long MA:", round(best_params[2])))
ga_optimization@solution %>% tail(1) -> params
daily_returns %>% cumsum %>% plot.ts


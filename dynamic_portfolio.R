## Greedy algorithm to find a set of positions closest to the optimal provided. Adapted from "Advanced futures trading strategies (2022)".
## refer to the book for details. 
# capital : your money in your account currency
# optimal_positions : vector of the best un-rounded positions (in contracts) corresponding to your forecast. 
#                     Forecasts are usually from -20 (max short position) to 20. Optimal positions in contracts are calculated as following:
#                     (capital * instrument_weight * IDM  * target_volatility / instrument_volatility  * FX_rate * Forecast/10) / (contract_size * price) 
# notional_exposures : vector of the values in account currency of one contract (usually contract_size * price / FX_rate)
# cov_matrix : covariance matrix of instruments returns, usually calculated from the last 6 months of (daily or weekly) returns.
# previous_position : vector of the previous optimal positions. All zeroes if not provided.
# max_positions : vector of the max allowed positions (in absolute contracts), usually corresponding to a forecast of 20 (see above formula). Ignored if NULL
# min_positions : vector of the min allowed positions (in absolute contracts). If NULL, it is set to the minimum incremental step (1 contract for futures).
# costs_per_contract : vector of the costs to trade one contract, in price scale. 
# trade_shadow_cost : a factor multiplier of the cost per contracts.
# fractional : TRUE is your broker allow fractional contracts, like for CFDs. The algorithm will use the decimal part of the positions as incremental step 
#              in the greedy algorithm. If you are trading futures where all contracts are 1, just set it to FALSE.
# max_factor : maximum multiple of optimal position allowed (e.g. if optimal position == 2 and max_factor == 2, the optimized position will be <= 4). 
#
# returned value: a vector of optimized positions according to the dynamic portfolio algorithm.
dynamic_portfolio <- function(capital, optimal_positions, notional_exposures, cov_matrix, 
                              previous_position = NULL, min_positions=NULL, max_positions=NULL, costs_per_contract = NULL, trade_shadow_cost = 1, fractional=FALSE, max_factor=2) {
    # Calculate the cost of making trades. trade_shadow_cost represents the number of expected trades in year
    calculate_costs <- function(weights) {
        trade_gap <- abs(weights_previous - weights)
        trade_costs <- trade_shadow_cost * sum(trade_gap * costs_per_trade_in_weight)
        return(trade_costs)
    }
    # Calculate the error of given weights from the optimal weights considering instruments correlations, plus optional costs
    evaluate <- function(weights_optimal, weights, cov_matrix) {
        solution_gap <- weights_optimal - weights
        track_error <- as.numeric(sqrt(t(solution_gap) %*% cov_matrix %*% solution_gap))
        trade_costs <- calculate_costs(weights)
        return(track_error + trade_costs)
    }
    # The greedy algorithm (see https://qoppac.blogspot.com/2021/10/mr-greedy-and-tale-of-minimum-tracking.html)
    find_possible_new_best <- function(weights_optimal, weights_max, weights_per_contract, direction, best_solution, best_value, cov_matrix, max_factor) {
        new_best_value <- best_value
        new_solution <- best_solution
        count_assets <- length(best_solution)
        for (i in sample(1:count_assets)) {
            temp_step <- best_solution
            if(temp_step[i] == 0) {
                temp_step[i] <- temp_step[i] + weights_min[i] * direction[i]
            } else {
                temp_step[i] <- temp_step[i] + weights_per_contract[i] * fractional[i] * direction[i]
            }
            if(abs(temp_step[i]) > weights_max[i])
                temp_step[i] <- weights_max[i] * sign(temp_step[i])
            else if (abs(temp_step[i]) > max_factor * abs(weights_optimal[i]))
                temp_step[i] <- max_factor * weights_optimal[i] 
            temp_objective_value <- evaluate(weights_optimal, temp_step, cov_matrix)
            if (temp_objective_value < new_best_value) {
                new_best_value <- temp_objective_value
                new_solution <- temp_step
            }
        }
        return(list(new_best_value, new_solution))
    }
    
    # Number os instruments
    n <- nrow(cov_matrix)
    # Set previous positions as zero if not specified
    if (is.null(previous_position)) {
        previous_position <- rep(0, n)
    }
    # Set trading costs to zero if not specified
    if (is.null(costs_per_contract)) {
        costs_per_contract <- rep(0, n)
    }
    # Find a fractional increments from positions (e.g. if position == 1.2 then the increment is 0.1)
    if (!fractional) {
        fractional <- rep(1, n)
    } else {
        fractional <-  10^(floor(log10(abs(optimal_positions)))-1)
    }
    weights_per_contract <- notional_exposures / capital
    weights_optimal <- optimal_positions * weights_per_contract 
    weights_max <- if(!is.null(max_positions)) max_positions * weights_per_contract else rep(Inf, n)
    weights_min <- if(!is.null(min_positions)) min_positions * weights_per_contract else weights_per_contract * fractional
    weights_previous <- previous_position * weights_per_contract
    costs_per_trade_in_weight <- (costs_per_contract  / capital) / weights_per_contract
    best_solution <- rep(0, n)
    best_value <- evaluate(weights_optimal, best_solution, cov_matrix)
    while (1) {
        res <- find_possible_new_best(weights_optimal, weights_max, weights_per_contract, sign(weights_optimal), best_solution, best_value, cov_matrix, max_factor)
        new_best_value <- res[[1]]
        new_solution <- res[[2]]
        if (new_best_value < best_value) {
            best_value <- new_best_value
            best_solution <- new_solution
        } else {
            break
        }
    }
    return(best_solution / weights_per_contract)
}

## Dynamic portfolio buffering.  Adapted from "Advanced futures trading strategies (2022)".
# capital : your money in your account currency
# optimized_positions : vector of optimized positions returned from the function "dynamic_portfolio"
# previous_position : vector of the previous optimal positions. All zeroes if not provided.
# notional_exposures : vector of the values in account currency of one contract (usually contract_size * price / FX_rate)
# cov_matrix : covariance matrix of instruments returns, usually calculated from the last 6 months of (daily or weekly) returns.
# target_volatility : your portfolio volatility target (e.g. 0.25)
# portfolio_buffering_level : the deviance representing the edges of the buffering. The highest this number the less frequent the portfolio updates.
#
# returned value: a list of: a vector of required positions updates to take (all zero if the adjustment factor is negative), 
#                            the tracking error of the portfolio 
#                            the adjustment factor (the percentage of position to adjust from the current to the optimized position)  
buffering_portfolio <- function(capital, optimized_positions, previous_positions, notional_exposures, cov_matrix, target_volatility, portfolio_buffering_level=0.1) {
    weights_per_contract <- notional_exposures / capital
    optimized_portfolio_weight <- optimized_positions * weights_per_contract 
    previous_portfolio_weight <- previous_positions * weights_per_contract 
    tracking_error_current_weight <- optimized_portfolio_weight - previous_portfolio_weight
    tracking_error <- as.numeric(sqrt(t(tracking_error_current_weight) %*% cov_matrix %*% tracking_error_current_weight))
    adjustment_factor <- max((tracking_error - portfolio_buffering_level/2 * target_volatility) / tracking_error, 0)
    required_trade <- adjustment_factor * (optimized_positions - previous_positions) 
    return(list(required_trade, tracking_error, adjustment_factor))
}

# EXAMPLE
run_example <- function() {
    
capital <- 500000
target_volatility <- 0.20
buffering_level <- 0.1
instrument_names <- c("US5y", "US10y", "SP500")
cov_matrix <- matrix(c(0.002704, 0.003838, -0.000889,
                       0.003838, 0.006724, -0.001402,
                       -0.000889, -0.001402, 0.029241
                        ), byrow = TRUE, ncol=3, dimnames = list(instrument_names, instrument_names))
previous_positions <- c(0, 0, 0)
optimal_positions <- c(0.4, 0.9, 3.1)
notional_exposures <- c(110000, 120000, 20000)

print("Previous positions:")
print(previous_positions)
print("Optimal positions:")
print(optimal_positions)
optimized_positions <- dynamic_portfolio(capital, optimal_positions=optimal_positions, notional_exposures=notional_exposures, 
                  cov_matrix=cov_matrix, previous_position=previous_positions)
print("Optimized positions:")
print(optimized_positions)
res <- buffering_portfolio(capital, optimized_positions = optimized_positions, previous_positions = previous_positions, 
                           notional_exposures = notional_exposures, cov_matrix = cov_matrix, 
                           target_volatility = target_volatility, portfolio_buffering_level = buffering_level)
print(paste("Tracking error:", round(res[[2]]*100,2), "%,", "Adjustment factor:", round(res[[3]]*100,2), "%"))
print("Required trades:")      
print(setNames(round(res[[1]]), instrument_names))
}

run_example()
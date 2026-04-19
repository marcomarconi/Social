library(quantmod)
library(tidyverse)
library(ggthemes)


# Month futures codes
month_map <- c(f=1,g=2,h=3,j=4,k=5,m=6,n=7,q=8,u=9,v=10,x=11,z=12)

# in dollars
max_drawdown <- function(equity) {
    equity <- na.omit(equity)
    running_max <- cummax(equity)
    drawdown <- equity - running_max
    mdd <- min(drawdown, na.rm = TRUE)
    dd_pct <- abs(mdd) / max(equity, na.rm = TRUE)
    list(
        max_drawdown = mdd,
        max_drawdown_pct = mdd_pct,
        drawdown_series = drawdown
    )
}

# Parse a single future contract
parse_contract <- function(filename) {
    base <- filename %>% basename() %>% tools::file_path_sans_ext()
    m <- str_match(base, "^..([fghjkmnquvxzFGHJKMNQUVXZ])([0-9]{2})$")
    #if (is.na(m[1,1])) stop(paste("Bad filename:", filename))
    month_chr <- tolower(m[1,2])
    yy <- as.integer(m[1,3])
    year_4 <- ifelse(yy >= 70, 1900 + yy, 2000 + yy)
    tibble(
        contract_name  = base,
        contract_month = month_map[[month_chr]],
        contract_year  = year_4
    )
}


# --- robust date parser ---
parse_time_column <- function(x) {
    # Try ymd first, then mdy; fall back to lubridate’s multiple formats
    parsed <- suppressWarnings(ymd(x))
    if (all(is.na(parsed))) parsed <- suppressWarnings(mdy(x))
    if (all(is.na(parsed))) parsed <- suppressWarnings(parse_date_time(x, orders = c("ymd", "mdy")))
    parsed
}

# --- read + compute per-contract returns ---
process_contracts <- function(files) {
    map_dfr(files, function(f) {
        meta <- parse_contract(f)
        read_csv(f, show_col_types = FALSE) %>%
            transmute(
                Date = parse_time_column(Time),
                Last = as.numeric(Last)
            ) %>%
            filter(!is.na(Date), !is.na(Last)) %>%
            arrange(Date) %>%
            mutate(
                contract_name  = meta$contract_name,
                contract_month = meta$contract_month,
                contract_year  = meta$contract_year,
                dLast = Last - dplyr::lag(Last),
                ret   = log(Last / dplyr::lag(Last)),
                prev  = dplyr::lag(Last),
                contract_key = contract_year * 100L + contract_month,
                date_key     = year(Date) * 100L + month(Date)
            )
    })
}

filter_contracts <- function(all_contracts, n = 2) {
    all_contracts %>% filter(contract_key >= date_key) %>%
        arrange(Date, contract_key) %>%
        group_by(Date) %>%
        slice_head(n = n) %>%
        mutate(rank = row_number()) %>%
        ungroup()
}

contracts_to_wide <- function(all_contracts) {
    all_contracts %>%
        transmute(
            Date,
            rank,
            C  = Last,
            dC = dLast,
            rC = ret,
            prevC = prev,
            name = contract_name
        ) %>%
        pivot_wider(
            names_from = rank,
            values_from = c(C, dC, rC, prevC, name),
            names_sep = ""
        ) %>% arrange(Date)
}



dir <- "/home/marco/trading/HistoricalData/Barchart/Futures/"
CMC_selection <- c("ZN","G","GG","CC","CA","KC","RM","HG","ZC","CT","CL","RB","HO", "LF", "PL","PA", 
                    "SI", "GC","HE","GF", "LE","LS","NG","ZO", "OJ","ZR","ZS","ZL","ZC","SW","ZM",
                    "ES","ZW")
instrument_list <- read_csv("/home/marco/trading/HistoricalData/Barchart/Futures/Instrument_List.csv", show_col_types = FALSE)
setwd(dir)
future_contracts <- list()
for(symbol in CMC_selection) {
    name <- instrument_list %>% dplyr::filter(Symbol == symbol) %>% pull(Name)
    dir_symbol <- paste0(dir, name)
    files <- list.files(dir_symbol,pattern = "^..[FfGgHhJjKkMmNnQqUuVvXxZz][0-9]{2}\\.csv$")
    stopifnot(length(files) > 0)
    # Process the contracts to calculare returns
    all_contracts <- process_contracts(paste0(dir_symbol,"/", files))
    # For each Date, pick front two contracts by contract month/year >= date's month/year
    front_two <-  filter_contracts(all_contracts, 3) %>% filter(rank>1)
    # Pivot to wide for prices and changes
    panel <- contracts_to_wide(front_two)
    future_contracts[[symbol]] <- panel %>% mutate(Symbol = symbol)
}
future_contracts_all <- do.call(rbind, future_contracts)
future_contracts_all <- future_contracts_all %>% filter(Date>"2010-01-01") %>% group_by(Symbol) %>% mutate(
  spread = log(C2 / C3),
  ret = rC3 - rC2,
  volatility = calculate_volatility(ret),
  ret_lead = lead(ret) / volatility,
  signal_0 = sign(spread),
  signal_1 = runZscore(spread %>% na.locf(na.rm=F), 252) %>% cap_forecast(2),
  pnl_naive = ret_lead,
  pnl_0 = ret_lead * signal_0,
  pnl_1 = ret_lead * signal_1,
  eq_naive = cumsum(replace_na(pnl_naive,0)),
  eq_0 = cumsum(replace_na(pnl_0,0)),
  eq_1 = cumsum(replace_na(pnl_1,0))
)

future_contracts_long <- future_contracts_all %>% dplyr::select(Date, Symbol, eq_naive, eq_0, eq_1) %>% pivot_longer(-c(Date, Symbol))
future_contracts_long %>% ggplot(aes(Date, value, color=Symbol)) + geom_line() + facet_wrap(~name)

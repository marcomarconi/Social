# Just backtest the VIX calendar strategy, short front long second, coded with chatgpt
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(quantmod)
library(tsibble)

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


# Download VIX index from Yahoo Finance
getSymbols("^VIX", src = "yahoo", from = "2004-01-01", to = Sys.Date())
vix_df <- data.frame(Date = index(VIX), coredata(VIX))



multiplier <- 100
vix_threshold <- 35

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


# detect all VIX futures files
dir <- "/home/marco/trading/HistoricalData/Barchart/Futures/VIX"
setwd(dir)
files <- list.files(dir,pattern = "^..[FfGgHhJjKkMmNnQqUuVvXxZz][0-9]{2}\\.csv$")
stopifnot(length(files) > 0)
# Process the contracts to calculare returns
all_contracts <- process_contracts(files)
# For each Date, pick front two contracts by contract month/year >= date's month/year
front_two <-  filter_contracts(all_contracts)
# Pivot to wide for prices and changes
panel <- contracts_to_wide(front_two)
# PnL: leg-by-leg using precomputed per-contract changes
# On a roll day, the newly-entered contract has dC = NA (no prior day) -> treated as 0
bt <- panel %>%
    mutate(
        pnl_short_C1 = -coalesce(dC1, 0) * multiplier,
        pnl_long_C2  =  coalesce(dC2, 0) * multiplier,
        pnl_total    = pnl_short_C1 + pnl_long_C2,
        eq_total_C1     = cumsum(replace_na(pnl_short_C1, 0)),
        eq_total_C2     = cumsum(replace_na(pnl_long_C2, 0)),
        eq_total     = cumsum(replace_na(pnl_total, 0)),
        pnl_log_short_C1     = -coalesce(rC1, 0),
        pnl_log_long_C2     = coalesce(rC2, 0),
        eq_log_total     = cumsum(replace_na(pnl_log_short_C1 + pnl_log_long_C2, 0)),
    )

bt_vix <- full_join(bt, vix_df %>% dplyr::select(Date, VIX.Close), by="Date") %>% arrange(Date)
bt_vix <- bt_vix %>% mutate(VIX_flag = if_else(lag(VIX.Close) > vix_threshold, 0, 1) %>% replace_na(0))

# Optional: show a compact result
bt_vix %>% select(Date, C1, C2, C1_name, C2_name, dC1, dC2, pnl_total, eq_total, pnl_log_short_C1, pnl_log_long_C2) %>% head()

plot.ts(bt_vix$eq_total);

# Monthly PnLs
bt_vix %>% group_by(M = yearmonth(Date)) %>% reframe(M_pnl = sum(pnl_total, na.rm=T)) %>% pull(M_pnl) %>% hist()

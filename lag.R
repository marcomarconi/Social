{
    suppressMessages(library(tidyverse))
    suppressMessages(library(moments))
    suppressMessages(library(TTR))
    suppressMessages(library(lubridate))
    suppressMessages(library(tsibble))
    suppressMessages(library(zoo))
    suppressMessages(library(moments))
    suppressMessages(library(ggthemes))
    suppressMessages(library(data.table))
    suppressMessages(library(Rfast))
    suppressMessages(library(optparse))
    suppressMessages(library(crayon))
    source("/home/marco/trading/Systems/Common/Common.R")
    source("/home/marco/trading/Systems/Common/Indicators.R")
    theme_set(theme_bw())
}
    
{
N <- 1000
p <- gbm_vec(1, N) %>% as.vector()
rsi <- RSI2(p, 20) %>% sign
signal <- rsi
ret <- c(0, diff(log(p)))
pnl <- signal * ret
cumpnl <- cumsum(replace_na(pnl,0))
plot.ts(cumpnl)
signal_ <- lag(rsi)
pnl_ <- signal_ * ret
cumpnl_ <- cumsum(replace_na(pnl_,0))
df <- data.frame(Lagged = c(rep("NO", N), rep("YES", N)), cumPnL = c(cumpnl, cumpnl_)) %>% mutate(x=rep(1:N, 2))
ggplot(df %>% filter(Lagged=="NO"), aes(x=x, y=cumPnL, color=Lagged)) + geom_line() + geom_point() + theme( text=element_text(size = 24), axis.text.x = element_blank()) + xlab("")
ggplot(df, aes(x=rep(1:N, 2), y=cumPnL, color=Lagged)) + geom_line() + geom_point() + theme( text=element_text(size = 24), axis.text.x = element_blank()) + xlab("")
}
{
    N <- nrow(res_1)
    df_res <- rbind(res_1, res_2, res_3, res_4, res_5) %>% mutate(Lag = c(rep(1, N), rep(2, N), rep(3, N), rep(4, N), rep(5, N))) %>% rename(cumPnL=PnL)
    ggplot(df_res, aes(x=as.Date(Date), y=cumPnL, color=Lag)) + geom_point() + theme( text=element_text(size = 24), axis.text.x = element_blank()) + xlab("")
}


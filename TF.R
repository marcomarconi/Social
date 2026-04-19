MM <- function(num_steps=50) {
    # Define states
    states <- c(-1, 0, 1)
    
    # Define transition matrix (example probabilities)
    transition_matrix <- matrix(c(
        0.95, 0.05, 0.0,  # From -1
        0.01, 0.98, 0.01,  # From  0
        0.0, 0.05, 0.95   # From  1
    ), nrow = 3, byrow = TRUE)
    
    # Initial state
    current_state <- sample(states, 1)
    
    # Store the state trajectory
    trajectory <- numeric(num_steps)
    trajectory[1] <- current_state
    
    # Run the simulation
    for (i in 2:num_steps) {
        current_index <- which(states == current_state)
        next_state <- sample(states, 1, prob = transition_matrix[current_index, ])
        trajectory[i] <- next_state
        current_state <- next_state
    }
    return(trajectory)
}
theme_set(theme_bw())
# RW vs Trendy
{
    N <- 100000
    lapse <- 40
    trend <- MM(N)
    trendy <- gbm_vec(nsim = 1, t = N, mu=trend[-1] * 1, sigma = 0.1)
    rwalk <- gbm_vec(nsim = 1, t = N, sigma = 0.1)
    trendy_ret <- c(0, diff(log(trendy)))
    rwalk_ret <- c(0, diff(log(rwalk)))
    par(mfrow = c(1, 3))
    trend %>% plot(type="l"); abline(h=0, lwd=3)
    trendy %>% plot(type="l")
    rwalk %>% plot(type="l")
    rwalk_frct <- RSI2(rwalk, lapse)
    trendy_frct  <- RSI2(trendy, lapse)
    rwalk_pnl <- lag(sign(rwalk_frct))*rwalk_ret; rwalk_pnl[is.na(rwalk_pnl)] <- 0;
    trendy_pnl <- lag(sign(trendy_frct))*trendy_ret; trendy_pnl[is.na(trendy_pnl)] <- 0;
    plot(density(abs(na.omit(rwalk_frct))), xlim=c(0, 1)); lines(density(abs(na.omit(trendy_frct))), col="red")
    df <- data.frame(Time=c(1:N, 1:N), Price=c(rwalk, trendy), Series=c(rep("RandomWalk", length(rwalk)), rep("Trendy", length(trendy))))
    ggplot(df, aes(x=Time, y=Price, color=Series)) + geom_line(linewidth=2) + theme(text = element_text(size=24), axis.text.x = element_blank(), axis.title.x = element_blank()) + scale_color_colorblind()
    df <- data.frame(Time=c(1:N, 1:N), CumPnL=c(cumsum(rwalk_pnl), cumsum(trendy_pnl)), Series=c(rep("RandomWalk", length(rwalk)), rep("Trendy", length(trendy))))
    ggplot(df, aes(x=Time, y=CumPnL, color=Series)) + geom_line(linewidth=2) + theme(text = element_text(size=24), axis.text.x = element_blank(), axis.title.x = element_blank()) + scale_color_colorblind()
    mean(trendy_pnl, na.rm=T)/sd(trendy_pnl, na.rm=T)*16
    df <- data.frame(PnL=trendy_pnl, Trade=lag(sign(trendy_frct)))
    rle_result <- rle(df$Trade)
    df$group_ids <- rep(seq_along(rle_result$values), times = rle_result$lengths)
    df <- df %>% group_by(group_ids) %>% reframe(Trade_PnL=sum(PnL))
    hist(df$Trade_PnL, 1000); abline(v=0, col="red", lwd=2)
    table(df$Trade_PnL>0)/length(df$Trade_PnL)
    df <- data.frame(Time=c(1:N, 1:N), RSI=c(rwalk_frct, trendy_frct), Series=c(rep("RandomWalk", length(rwalk)), rep("Trendy", length(trendy))))
    ggplot(df, aes(x=abs(RSI), color=Series)) + geom_density(linewidth=2) + theme(text = element_text(size=24), axis.text.x = element_blank(), axis.title.x = element_blank()) + scale_color_colorblind()
    
}


{
lapse <- 40
b <- BackAdj %>% lapply(., function(x) RSI2(x$AdjClose, lapse, maType = SMA)) %>% unlist %>% na.omit
a <- gbm_vec(100, t = 5000) %>% apply(., 2, function(x) RSI2(x, lapse, maType = SMA)) %>% as.vector %>% na.omit
Symbols %>% lapply(., function(x) RSI2(x$Close, lapse, maType = SMA)) %>% unlist %>% na.omit -> d
df <- data.frame(Time=c(1:length(a), 1:length(b)), RSI=c(a, b), Series=factor(c(rep("RandomWalk", length(a)), rep("Futures", length(b)))))
ggplot(df, aes(x=abs(RSI), color=Series)) + geom_density(linewidth=1) + 
    theme(text = element_text(size=24), axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = c(0.95, 0.95), legend.justification = c(1, 1) ) +
    scale_color_manual(values = c("blue", "black")) 

plot(density(abs(a)), col="blue"); lines(density(abs(b))); lines(density(abs(d)), col="red")

}

# Stats from real backtest
{
    hist(res$Symbols$`Sharpe ratio`, 30, main="Sharpe Ratio"); abline(v=0, lwd=2); abline(v=0.25, lwd=2, lty=2);
    all_forecasts <- do.call(rbind,forecasts)[,2] 
    all_results<- do.call(rbind,results)[,2]
    df <- data.frame(frct=all_forecasts, res=all_results)
    df$Trade  <- sign(df$frct)
    rle_result <- rle(df$Trade)
    df$group_ids <- rep(seq_along(rle_result$values), times = rle_result$lengths)
    df <- df %>% group_by(group_ids) %>% reframe(Trade_PnL=sum(res))
    hist(df$Trade_PnL, 1000, xlim=c(-1,1)); abline(v=0, col="red", lwd=2)
    table(df$Trade_PnL>0)/length(df$Trade_PnL)
}
# meh
{
    a <- read_delim("~/Downloads/Histórico(39).csv", delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."))
    a <- read_delim("~/Downloads/Histórico(39).csv")
    a$Ret <- (a$`CANTIDAD (EUR)`/a$`SALDO (EUR)`)  * 100
    hist(a$Ret, 100); abline(v=0, lwd=2)
}

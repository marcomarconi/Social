library(tsibble)
library(tidyverse)
library(lubridate)
library(TTR)
library(ggthemes)
theme_set(theme)
# Bonds
Bonds <- list()
for(symbol in names(Futures)) {
    if(!(symbol %in% c("ZN",  "ZF", "ZT", "UD", "ZB")))
        next
    print(symbol)
    Bonds[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Bonds[[symbol]]$Symbol <- symbol
}
df <- Bonds[c("ZN", "ZF", "ZT", "UD", "ZB")] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <-  mutate(df, dom=mday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(df, Trade = case_when(dom >= 25  ~ 1, dom <= 5 ~ -1, TRUE ~ 0), Cost=0) %>% 
    mutate(Cost = case_when(Symbol == "ZN" ~ 0.00008, Symbol == "ZF" ~ 0.00005, Symbol == "ZT" ~ 0.00003, Symbol == "UD" ~ 0.00007,  Symbol == "ZB" ~ 0.00012,TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade)) %>% 
    group_by(date, Symbol) %>% 
    summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    summarise(date=as.Date(date),PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost) %>% 
    mutate(Symbol = case_when(Symbol == "ZN" ~ "10y", Symbol == "ZF" ~ "5y", Symbol == "ZT" ~ "2y", 
                            Symbol == "ZB" ~ "30y",  Symbol == "UD" ~ "Ultra",TRUE ~ Symbol))
ggplot(a) + geom_line(aes(date, PnL, color=Symbol), linewidth=2) + scale_color_colorblind() + ylim(0, 1.5)+
    theme(text= element_text(size=40), legend.title = element_blank(), legend.text = element_text(size=48), legend.key.width = unit(5, "line"), axis.title.x = element_blank()) 

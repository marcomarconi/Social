# Oils
Oils <- list()
for(symbol in names(Futures)) {
    if(!(symbol %in% c("CB", "CL", "RB", "HO", "LF")))
        next
    print(symbol)
    Oils[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Oils[[symbol]]$Symbol <- symbol
}
df <- Oils[c("CB", "CL", "RB", "HO", "LF")] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <- mutate(df, dom=wday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Cost = case_when(Symbol == "CB" ~ 0.0005, Symbol == "CL" ~ 0.0005, Symbol == "RB" ~ 0.001, 
                            Symbol == "HO" ~ 0.001,  Symbol == "LF" ~ 0.0007,TRUE ~ 0)) %>% 
    mutate(Trade = case_when(dom <= 3 & Basis < 0 ~ -1, dom > 3 & Basis > 0 ~ 1, TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% 
    group_by(date, Symbol) %>% summarise(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    summarise(date=as.Date(date),PnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
    mutate(Symbol = case_when(Symbol == "CB" ~ "Brent", Symbol == "CL" ~ "WTI", Symbol == "RB" ~ "Gasoline", 
                              Symbol == "HO" ~ "Heating Oil",  Symbol == "LF" ~ "Gasoil",TRUE ~ Symbol))
ggplot(a) + geom_line(aes(date, PnL, color=Symbol), linewidth=2) + scale_color_colorblind() + ylim(0, 6)+
    theme(text= element_text(size=48), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
a %>% summarise(mean(Excess)/sd(Excess)*sqrt(52))

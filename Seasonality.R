library(tsibble)
library(tidyverse)
library(lubridate)
library(TTR)
library(ggthemes)
theme_set(theme_bw())

Futures <- read_rds("/home/marco/trading/HistoricalData/Barchart/Futures.RDS")
BackAdj <- read_rds("/home/marco/trading/HistoricalData/Barchart/BackAdj.RDS")

## Done fot RobotWealth
{
    all <- list()
    for(symbol in names(BackAdj)){
        print(symbol)
        df <- BackAdj[[symbol]] %>% filter(year(Date) >= 2000)
        name <-  BackAdj[[symbol]]$Name[1]
        df$Volatility <- calculate_volatility(df$Return)
        df$Return <- df$Return / df$Volatility * sqrt(252) # Seasonality adjusted, comment it if unwanted
        all[[name]] <- df
    }
    df <- do.call(rbind, all)
    {
        setwd("/tmp")
        for(type in unique(df$Class)){
            a <- group_by(df, Name, wday=lubridate::wday(Date), year=year(Date)) %>%filter(Class==type & wday %in% 2:6) %>% 
                reframe(Mean=mean(Return, na.rm=T))
            b <- group_by(a, Name, wday) %>% na.omit %>% summarize(M=mean(Mean), SD=2*sd(Mean)/sqrt(n()))
            p <- ggplot(a) + #geom_point(aes(x=jitter(wday), y=Mean), fill="grey90", color="gray20", pch=21, size=2) +  
                scale_x_continuous(labels=c("Mon","Tue","Wed","Thu","Fri")) +
                geom_errorbar(data=b, aes(x=wday, ymin=M-SD, ymax=M+SD), width=0.25, linewidth=2) +  geom_hline(yintercept = 0)  + facet_wrap(~Name)+
                theme(legend.position = "None", axis.title = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size=18), plot.title = element_text(hjust = 0.5, size=24, face = "bold"),panel.grid.minor = element_line(linewidth = 0.5), panel.grid.major = element_line(linewidth = 0.5), strip.text = element_text(size=18, margin = margin(0.1,0,0.1,0, "cm"))) + ggtitle(type ) 
                #ylim(c(-0.5, 0.5))
            ggsave(filename = paste0(type, "_week.png"), p, width=16, height=12,dpi=300)
            a <- mutate(df, Name, dom = mday(Date), year=year(Date)) %>%filter(Class==type)  %>% mutate(W = case_when(dom <= 7 ~ 1, dom > 7 & dom <= 14 ~ 2, dom > 14 & dom <= 21 ~ 3, dom > 21 & dom <= 31 ~ 4, TRUE ~ 0)) %>% 
                group_by(Name, W, year) %>% reframe( Mean=mean(Return, na.rm=T)) %>% na.omit
            b <- group_by(a, Name, W) %>% na.omit %>% reframe(M=mean(Mean), SD=2*sd(Mean)/sqrt(n()))
            p <- ggplot(a) +  #geom_point(aes(x=jitter(W), y=Mean), fill="grey90", color="gray20", pch=21, size=2)  + 
                facet_wrap(~Name)+
                scale_x_continuous(labels=c("1st","2nd","3rd","4th")) +
                geom_errorbar(data=b, aes(x=W, ymin=M-SD, ymax=M+SD), width=0.25, linewidth=2) + geom_hline(yintercept = 0)  + 
                theme(legend.position = "None", axis.title = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size=18), plot.title = element_text(hjust = 0.5, size=24, face = "bold"),panel.grid.minor = element_line(linewidth = 0.5), panel.grid.major = element_line(linewidth = 0.5), strip.text = element_text(size=18, margin = margin(0.1,0,0.1,0, "cm"))) + ggtitle(type )
                #ylim(c(-0.5, 0.5))
            ggsave(filename = paste0(type, "_month.png"), p, width=16, height=12,dpi=300)
         }
    }
}


# Bonds
Bonds <- list()
symbols <- c("CG","G","GG","GX","HF","HR","TJ","ZB","ZF","ZN","ZT")
for(symbol in names(Futures)) {
    if(!(symbol %in% symbols))
        next
    print(symbol)
    Bonds[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Bonds[[symbol]]$Symbol <- symbol
}
df <- Bonds[symbols] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <-  mutate(df, dom=mday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Trade = case_when(dom >= 25  ~ 1, dom <= 5 ~ -1*0, TRUE ~ 0), Cost=0) %>% 
    mutate(Cost = case_when(Symbol == "ZN" ~ 0.00008, Symbol == "ZF" ~ 0.00005, Symbol == "ZT" ~ 0.00003, Symbol == "UD" ~ 0.00007,  Symbol == "ZB" ~ 0.00012,TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Position*Return*Trade)) %>% mutate(Cost=0) %>% #filter(year(date)>=2023)%>% 
    group_by(date, Symbol) %>% 
    reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost) %>% 
    mutate(Symbol = case_when(Symbol == "ZN" ~ "10y", Symbol == "ZF" ~ "5y", Symbol == "ZT" ~ "2y", 
                              Symbol == "ZB" ~ "30y",  Symbol == "UD" ~ "Ultra", Symbol == "CG" ~ "Canada10Y", 
                              Symbol == "G" ~ "Gilt", Symbol == "GG" ~ "Bund", Symbol == "GX" ~ "Buxl", 
                              Symbol == "HF" ~ "Schatz", Symbol == "HR" ~ "Bobl", Symbol == "TJ" ~ "JGB10Y", 
                              TRUE ~ Symbol))
ggplot(a) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + scale_color_viridis_d()+ 
    theme(text= element_text(size=24), legend.title = element_blank(), legend.text = element_text(size=24), legend.key.width = unit(5, "line"), axis.title.x = element_blank()) 
a %>% group_by(Symbol)%>% reframe(mean(Excess)/sd(Excess)*sqrt(52))

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
    mutate(Cost = case_when(Symbol == "CB" ~ 0.03/100, Symbol == "CL" ~ 0.03/100, Symbol == "RB" ~ 0.04/100, 
                            Symbol == "HO" ~ 0.03/100,  Symbol == "LF" ~ 0.04/100,TRUE ~ 0)) %>% 
    mutate(Trade = case_when(dom == 2  ~ -1, dom == 6  ~ 1, TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% mutate(Cost=0) %>% #filter(year(date)==2024)%>% 
    group_by(date, Symbol) %>% reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
    mutate(Symbol = case_when(Symbol == "CB" ~ "Brent", Symbol == "CL" ~ "WTI", Symbol == "RB" ~ "Gasoline", 
                              Symbol == "HO" ~ "Heating Oil",  Symbol == "LF" ~ "Gasoil",TRUE ~ Symbol))
ggplot(a ) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + scale_color_colorblind() + 
    theme(text= element_text(size=32), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
a %>% group_by(Symbol)%>% reframe(mean(Excess)/sd(Excess)*sqrt(52))

# Metals
Metals <- list()
symbols <- c("GC", "SI", "HG", "R0", "Q0", "O0")
for(symbol in names(Futures)) {
    if(!(symbol %in% symbols))
        next
    print(symbol)
    Metals[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    Metals[[symbol]]$Symbol <- symbol
}
df <- Metals[symbols] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <- mutate(df, dom=wday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Cost = case_when(Symbol == "GC" ~ 0.02/100, Symbol == "HG" ~ 0.03/100, Symbol == "SI" ~ 0.02/100, 
                            Symbol == "R0" ~ 0.04/100,  Symbol == "Q0" ~ 0.04/100, Symbol == "O0" ~ 0.04/100,TRUE ~ 0)) %>% 
    mutate(Trade = case_when(dom == 2  ~ -1*0, dom == 6  ~ 1, TRUE ~ 0)) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% mutate(Cost=0) %>% #filter(year(date)==2024)%>% 
    group_by(date, Symbol) %>% reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
    mutate(Symbol = case_when(Symbol == "GC" ~ "Gold", Symbol == "SI" ~ "Silver", Symbol == "HG" ~ "Copper", 
                              Symbol == "R0" ~ "Lead",  Symbol == "Q0" ~ "Nickel", Symbol == "O0" ~ "Zinc",TRUE ~ Symbol))
ggplot(a ) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + scale_color_colorblind() + 
    theme(text= element_text(size=32), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
a %>% group_by(Symbol)%>% reframe(mean(Excess)/sd(Excess)*sqrt(52))


# Lumber/Hogs
{
    df <- rbind(BackAdj[["LS"]],BackAdj[["HE"]]) %>% filter(year(Date) > 2000)
    a <- mutate(df, dom=wday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
        mutate(Cost = case_when(Symbol == "GC" ~ 0.02/100, Symbol == "HG" ~ 0.03/100, Symbol == "SI" ~ 0.02/100, 
                                Symbol == "R0" ~ 0.04/100,  Symbol == "Q0" ~ 0.04/100, Symbol == "O0" ~ 0.04/100,TRUE ~ 0)) %>% 
        mutate(Trade = case_when(dom == 2  ~ -1, dom == 6  ~ 1, TRUE ~ 0)) %>% 
        mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
        mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% mutate(Cost=0) %>% #filter(year(date)==2024)%>% 
        group_by(date, Symbol) %>% reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
        reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
        mutate(Symbol = case_when(Symbol == "LS" ~ "Lumber", Symbol == "HE" ~ "LeanHogs",TRUE ~ Symbol))
    ggplot(a ) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + scale_color_colorblind() + 
        theme(text= element_text(size=32), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
    
}


# FX
FX <- list()
symbols <- c("GN", "MB", "PV", "PW", "UN", "UR")
for(symbol in names(Futures)) {
    if(!(symbol %in% symbols))
        next
    print(symbol)
    FX[[symbol]] <- backadjust_future(Futures[[symbol]], N=5)
    FX[[symbol]]$Symbol <- symbol
}
df <- FX[symbols] %>% do.call(rbind, .) %>% filter(year(Date) > 2000)
a <- mutate(df, dom=mday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
    mutate(Cost = 0) %>% 
    mutate(Trade = case_when(dom <= 7 ~ -1, TRUE ~ 0), Cost=0) %>% 
    mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
    mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% mutate(Cost=0) %>% #filter(year(date)==2024)%>% 
    group_by(date, Symbol) %>% reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
    reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
    mutate(Symbol = case_when(Symbol == "GN" ~ "GBPNZD", Symbol == "MB" ~ "GBPUSD", Symbol == "PV" ~ "GBPAUD", 
                              Symbol == "PW" ~ "GBPCAD",  Symbol == "UN" ~ "GBPCHF", Symbol == "UR" ~ "GBPJPY",TRUE ~ Symbol))
ggplot(a ) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + scale_color_colorblind() + 
    theme(text= element_text(size=32), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 
a %>% group_by(Symbol)%>% reframe(mean(Excess)/sd(Excess)*sqrt(52))


# Bond weekly
{
    symbols <- c("CG","G","GG","GX","HF","HR","TJ","ZB","ZF","ZN","ZT")

    df <- BackAdj[symbols]%>% do.call(rbind, .) %>% filter(year(Date) > 2000)
    a <- mutate(df, dom=wday(Date), date=yearweek(Date), Symbol=factor(Symbol)) %>% 
        mutate(Cost = case_when(Symbol == "GC" ~ 0.02/100, Symbol == "HG" ~ 0.03/100, Symbol == "SI" ~ 0.02/100, 
                                Symbol == "R0" ~ 0.04/100,  Symbol == "Q0" ~ 0.04/100, Symbol == "O0" ~ 0.04/100,TRUE ~ 0)) %>% 
        mutate(Trade = case_when(dom == 6  ~ 1, TRUE ~ 0)) %>% 
        mutate(Position = 0.25 / calculate_volatility(Return)) %>% 
        mutate(Excess = ifelse(is.na(Return), 0, Return*Trade*Position)) %>% mutate(Cost=0) %>% #filter(year(date)==2024)%>% 
        group_by(date, Symbol) %>% reframe(Excess=sum(Excess, na.rm=TRUE), Trades=first(length(rle(Trade[Trade!=0]))), Cost=first(Cost*Trades)) %>% group_by(Symbol) %>% 
        reframe(date=as.Date(date),cumPnL=cumsum(Excess-Cost),Excess=Excess-Cost, Cost=Cost, Symbol=as.character(Symbol)) %>% 
        mutate(Symbol = case_when(Symbol == "LS" ~ "Lumber", Symbol == "HE" ~ "LeanHogs",TRUE ~ Symbol))
    ggplot(a ) + geom_line(aes(date, cumPnL, color=Symbol), linewidth=2) + 
        theme(text= element_text(size=32), legend.title = element_blank(), legend.key.width = unit(2, "line"), axis.title.x = element_blank()) 

       
}

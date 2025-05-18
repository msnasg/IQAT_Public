
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.



# Forex

CandlePlotly_Func <- function(df){
  dff <- data.frame(Date = index(zoo(df)), coredata(zoo(df)))
  fig <- dff %>% plot_ly(x = ~Date, type="candlestick",
                         open = ~Open, close = ~Close,
                         high = ~High, low = ~Low) %>% 
    layout(title = "Candlestick Chart")
  return(fig)
} # CandlePlotly_Func

Doji_Func <- function(df = Table_D1[[1]], delta = 0.1){

} # Doji_Func

# ---------------------------------------------------------------------------- #
# Test code
if(MT5.Ping() && F){
  
  #AllSymbols <- MT5.Marketwatch()
  # print(AllSymbols)
  
  AllSymbols <- c("EURUSD",	"USDJPY", "GBPUSD", "EURGBP", "USDCHF", "AUDUSD", 
                  "USDCAD", "NZDUSD", "EURCHF", "EURJPY", "GBPJPY",
                  "AUDJPY")
  Standard_Lots <- c(100000, 1000, 100000, 100000, 100000,100000,
                     100000,100000,100000,1000,1000,1000)
  

  Table <- list()
  irows = 2500
  itf = 1440 # D1 = 1440, H1 = 60, H4 = 240
  
  for(i in 1:length(AllSymbols)){
    Table[[i]] = MT5.GetSymbol(AllSymbols[i], iTF = itf, iRows = irows, xts = T) 
    print(i)
  } # i
  
  names(Table) <- AllSymbols
  

  df = data.frame(date=index(as.zoo(Table[[1]])))
  for(i in 1:length(AllSymbols)){
    df <- cbind.data.frame(df, Table[[i]]$Close)
  }
  colnames(df) <- c("date", AllSymbols)
  df.rcorr = rcorr(as.matrix(df[,-1]))
  df.rcorr
  
  library(corrplot)
  corrplot(df.rcorr$r)
  corrplot(df.rcorr$r, method="number")

  
  MT5.AccountInfo()

    MT5.SingleOrder(sSymbol = "EURUSD",
    iCmd = 0,
    fVol = 0.1,
    fPrice = 1.0918,
    fStop = 1.0918 - 0.05,
    fGain = 1.0918 + 0.05,
    iFillType = 0
  )
  
  # -------------------------------------------------------------------------- #
  # Test Strategy
  # -------------------------------------------------------------------------- #

  # --------------------------- #
  # Data
  # --------------------------- #
   Table = list(MT5.GetSymbol(AllSymbols[1], iTF = 1440, iRows = 1 * 250, xts = T))
   names(Table) <- AllSymbols[1]
   # --------------------------- #
   Table = Table_D1[1]
   # --------------------------- #
   
   res <- data.frame()
   Trade_df <- list()
   
   for(l in 1:length(Table)){ # 
     
     df = Table[[l]]
     # Doji
     df = Doji_Func (df = Table[[l]], delta = 0.1)
     # df$MA20 <- rollapply(df$Close, 25, mean, align = "right", fill = NA)
     # View(df)
     
     OP <- as.numeric(Op(df))
     LO <- as.numeric(Lo(df))
     HI <- as.numeric(Hi(df))
     CL <- as.numeric(Cl(df))
     N <- nrow(df)
     
     M <-(OP+CL)/2 # median of a candle stick
     US <- HI - pmax(OP,CL) # upper shadow is line between high and real body 
     LS <- pmin(OP,CL) - LO # lower shadow is line between low and real body
     WC <- HI - LO # Whole candle length
     BL <- abs(OP - CL) # Body length
     
     Tru = Fals = 0
     Profits <- Losses <- Trade_Fals <- Trade_Tru <- c()
     Profit = Profit_pip = Loss_pip = 0
     lot = 0.1
     Standard_Lot = Standard_Lots[l] # 100000
     St = 0.1 # Body Length
     TP = 0.25 # Body Length
     df$BuySignal <- NA
     df$SellSignal <- NA
     
     for(i in ifrom:eval(parse(text = ito)) ){
       
       # Bulish
       if( eval(parse(text = Bullish_Tru_Strategy)) ){
         Tru = Tru + 1
         Profit_pip = Profit_pip + TP * BL[i+1]
         Profits[Tru] = (lot * Standard_Lot) * (TP * BL[i+1])
         Trade_Tru[Tru] <- "buy"
         df[i,"BuySignal"] <- "bT"
       }
       
       if(eval(parse(text = Bullish_Fals_Strategy)) ){
         Fals = Fals + 1
         Loss_pip = Loss_pip - St * BL[i+1]
         Losses[Fals] = -(lot * Standard_Lot) * (St * BL[i+1])
         Trade_Fals[Fals] <- "buy"
         df[i,"BuySignal"] <- "bF"
       }
       # Bearish
       if( eval(parse(text = Bearish_Tru_Strategy)) ){
         Tru = Tru + 1
         Profit_pip = Profit_pip + TP * BL[i+1]
         Profits[Tru] = (lot * Standard_Lot) * (TP * BL[i+1])
         Trade_Tru[Tru] <- "sell"
         df[i,"SellSignal"] <- "sT"
       }
       if(eval(parse(text = Bearish_Fals_Strategy)) ){
         Fals = Fals + 1
         Loss_pip = Loss_pip - St * BL[i+1]
         Losses[Fals] = -(lot * Standard_Lot) * (St * BL[i+1])
         Trade_Fals[Fals] <- "sell"
         df[i,"SellSignal"] <- "sF"
       }
     } # for i
    res = rbind.data.frame(res, data.frame(Symbol = names(Table)[l],
                                           N = N,
                                           Perc = Tru / (Tru + Fals), 
                                           TrueCases = Tru, 
                                           FalseCases = Fals,
                                           Profit_pip = Profit_pip,
                                           Loss_pip = Loss_pip, 
                                           Diff = Profit_pip + Loss_pip))
    
    Trade_df[[l]] <- df
    
    cat(l)
   } # for l
  
   res$Profit = lot * Standard_Lots * res$Diff
   sum(res$Profit)
   View(res)
   # 250 Days: 341.546
   # 500 Days: 651.3565
   # 750 Days: 831.556
   # 2500 Days: 2526.659
   
  dff <- Trade_df[[1]]
   
   dff <- data.frame(Date = index(zoo(df)), coredata(zoo(df)))
   
   fig <- dff %>% plot_ly(x = ~Date, type="candlestick",
                          text = dff$BuySignal,
                          open = ~Open, close = ~Close,
                          high = ~High, low = ~Low,
                          hoverinfo='text') %>%
     layout(title = "...",
            xaxis = list(rangeslider = list(visible = F)), 
            yaxis = list(title = "Price"))
   
   fig %>% add_text(text = dff$BuySignal, x = dff$Date, y = as.numeric(dff$Low),
                    textposition = "bottom center",
            showlegend = F) %>% 
     add_text(text = dff$SellSignal, x = dff$Date, y = as.numeric(dff$High),
                textposition = "top center",
                showlegend = F) 
   

   
   # Density Plot
   d1 = data.frame(Item = "Profits", Trade = Trade_Tru, Value = Profits)
   d2 = data.frame(Item = "Losses", Trade = Trade_Fals, Value = Losses)
   d = rbind.data.frame(d1,d2)
   ggplot(d, aes(x = Value, color = Item )) + geom_density(alpha=0.8)
   
   #

  # -------------------------------------------------------------------------- #
  df = xts(df)
  ret = (df$Close/lag(df$Close)) - 1
  
  # hist(df$Close)
  # df %>%
  #   ggplot( aes(x=Close)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
  ret %>%
    ggplot( aes(x=Close)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
   
  # -------------------------------------------------------------------------- #
  # Test if a green candle, how many next one is also green  # -------------------------------------------------------------------------- #
  # Daily iTF = 1440
  # H1 iTF = 60
  df1 = MT5.GetSymbol(AllSymbols[1], iTF = 240, iRows = 2000, xts = T)
  
  df = df1
  # 2000 Days 54%
  # 4000 Days: 50%
  # 10000 Days: 50%
  # 2000 H1 45%
  # 2000 H4 49%
  
  # Moving Average 
  x = 1:8
  rollapply(x, 6, mean, align = "right", fill = NA)
  rollapply(x, 6, toString, align = "right", fill = NA)
  
  # Time Series Trend
  dt = dff[(nrow(dff)-15):nrow(dff),]
  
  angle = atan(abs((m2-m1)/(1+m1*m2)))
  
  tmp = coef( lm(Close~Date, data = dt) )
  
  
  plot(x = dt$Date,                # True values on x-axis
       y = dt$Close,               # fitted values on y-axis
       xlab = "Date",
       ylab = "...",
       main = "Regression")
  abline(b = tmp[2], a =  tmp[1])  
  
  
  # -------------------------------------------------------------------------- #
  # if daily candel change from 2 reds to green
  # prob to have green next
  # -------------------------------------------------------------------------- #
  k = p = 0
  for(i in 3:(nrow(df)-1)){
    if(df[i-2,"Close"] < df[i-2,"Open"] && 
       df[i-1,"Close"] < df[i-1,"Open"] &&
       df[i,"Close"] > df[i,"Open"] && 
       df[i+1,"Close"] > df[i+1,"Open"]){
      k = k + 1
    }
    
    if(df[i-2,"Close"] < df[i-2,"Open"] && 
       df[i-1,"Close"] < df[i-1,"Open"] &&
       df[i,"Close"] > df[i,"Open"] &&
       df[i+1,"Close"] < df[i+1,"Open"]){
      p = p + 1
    }
  }
  k / (k+p) # 61%
  

  
}


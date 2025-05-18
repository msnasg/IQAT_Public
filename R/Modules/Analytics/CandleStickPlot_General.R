
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.



CandleStickPlot_General <- function(dat = RefTbl, Sy = "AKHABER", TF = "D", Windw = "All", 
                               LastPr = 200, Param = c(12,26,9), Type = "ggplot", Indicator = "MACD"){

  # dat = RefTbl; Sy = "SHETEHRAN"; TF = "D"; Windw = "All"; 
  # LastPr = 200; Param = c(12,26,9); Type = "ggplot"; Indicator = "MACD"
  
  PltMain_gg = NULL; PltIndctr_gg = NULL; PltMain_ly = NULL; PltIndctr_ly = NULL
  df = NULL

  
  if(!is.null(Sy)){
    LastPr = as.numeric(LastPr)
    # Pr = GetDataDB (dat, Sy , TF , Windw = Windw)$SeriesP
    Pr = dat
    OHLC = zoo(Pr)
    if("Volume" %in% colnames(OHLC)){
      OHLC <- data.frame(Date=index(OHLC), Open=OHLC$Open, High=OHLC$High, Low=OHLC$Low, Close=OHLC$Close, Volume = OHLC$Volume)  
    }else{
      OHLC <- data.frame(Date=index(OHLC), Open=OHLC$Open, High=OHLC$High, Low=OHLC$Low, Close=OHLC$Close)  
    }
    
    OHLCPlt <- data.frame(Date=OHLC$Date, Open=OHLC$Open, High=OHLC$High, Low=OHLC$Low, Close=OHLC$Close )
    OHLCPlt <- transform(OHLCPlt, Date = as.POSIXct(OHLCPlt$Date))  # is important for CandleStick plot
    
    OHLCPlt <- data.frame(OHLCPlt, chg=0, width=0, flat_bar=0)
    OHLCPlt$chg <- ifelse(Cl(OHLCPlt) > Op(OHLCPlt), "up", "dn")
    OHLCPlt$width <- as.numeric(periodicity(OHLC)[1])
    OHLCPlt$flat_bar <- OHLCPlt[, "High"] == OHLCPlt[, "Low"]
    
    if(nrow(OHLC) >  LastPr){
      OHLC <- tail(OHLC, LastPr)
      Pr = tail(Pr, LastPr)
      OHLCPlt <- tail(OHLCPlt, LastPr)
    }else{LastPr = nrow(OHLC)}
    
    df = data.frame(OHLC)
    
  } #  if is.null(OHLC)
  
  # +------------------------------------------------------------------+
  # | ggplot                                                           |
  # +------------------------------------------------------------------+
  if(Type == "ggplot"){

    if(!is.null(Indicator)){
      ## ---
      if(Indicator == "MACD"){
        Fast_P = Param[1]
        Slow_P = Param[2] 
        Signal_P = Param[3]
        macd  <- MACD( OHLC[,"Close"], Fast_P, Slow_P, Signal_P, maType="EMA" )
        data <- data.frame(OHLCPlt, MACD = macd, MACD.hist = macd[,"macd"] - macd[,"signal"])
        data <- na.omit(data)
        df = data
        if(nrow(data) > 50){
          PltIndctr_gg <- ggplot(data, aes(x=Date))  + 
            geom_bar(aes(y = MACD.hist), stat="identity", position=position_dodge(), colour="black") +
            geom_line(aes(y = MACD.macd), color='blue', size=1) +
            geom_line(aes(y = MACD.signal), color='red', size=1) +
            scale_y_continuous(position = "right") +
            ylab("MACD (Signal: Blue)") + theme_bw() 
        }
       
      } # if Indicator == "MACD"
      ## ---
    } # if !is.null(Indicator)
    
    if(nrow(data) > 50){
    PltMain_gg <- ggplot(data, aes(x=Date)) + geom_linerange(aes(ymin=Low, ymax=High)) + theme_bw() +
      labs(title = Sy) + geom_rect(aes(xmin = Date - width/2 * 0.9, xmax = Date + width/2 * 0.9, ymin = pmin(Open, Close),
                                       ymax = pmax(Open, Close), fill = chg)) + guides(fill = FALSE, colour = FALSE) + 
      scale_fill_manual(values = c("dn" = "darkred", "up" = "darkgreen")) + scale_color_discrete(na.value=NA) + 
      scale_y_continuous(position = "right")
    }
    
  } # if Type == "ggplot"
  
  
  
  
  # +------------------------------------------------------------------+
  # | plotly                                                           |
  # +------------------------------------------------------------------+
if(Type == "plotly"){
  library('plotly'); library('tidyverse'); library('alphavantager')

  ## ---
    if(!is.null(Indicator)){
      ## ---
      if(Indicator == "MACD"){
        Fast_P = Param[1]
        Slow_P = Param[2] 
        Signal_P = Param[3]
        macd  <- MACD( OHLC[,"Close"], Fast_P, Slow_P, Signal_P, maType="EMA" )
        data <- data.frame(OHLC, MACD = macd, MACD.hist = macd[,"macd"] - macd[,"signal"])
        data <- na.omit(data)
        df = data
        
        PltIndctr_ly <- plot_ly(data) %>%   
          add_trace(type = 'bar', x = ~Date, y = ~MACD.hist, name = 'MACD Histogram', marker = list(color = 'gray'),
                  yaxis = 'y2', legendgroup = 'two') %>% 
          add_trace(type = 'scatter', mode = 'lines', marker = NULL, x = ~Date, y = ~MACD.macd, name = 'MACD',
                    line = list(color = 'red'), yaxis = 'y2', legendgroup = 'two') %>% 
          add_trace(type = 'scatter', mode = 'lines', marker = NULL, x = ~Date, y = ~MACD.signal, name = 'Signal',
                    line = list(color = 'plum'), yaxis = 'y2', legendgroup = 'two')
      } # if Indicator == "MACD"
      ## ---
    } # if !is.null(Indicator)
  

  df = data
  PltMain_ly <- plot_ly(data, height = 700) %>% 
    add_trace(type = 'candlestick', 
              name = 'OHLC', x = ~Date,
              open = ~Open, high = ~High, low = ~Low, close = ~Close,
              increasing = list(line = list(color='rgba(52,169,102,1)', width=1), fillcolor = 'rgba(0,0,0,0)'), 
              decreasing = list(line = list(color='rgba(220,68,59,1)', width=1), fillcolor = 'rgba(0,0,0,0)'), 
              legendgroup = 'one')   %>%
    layout(yaxis = list(domain = c(0.62, 1), fixedrange = FALSE),
           yaxis2 = list(domain = c(0.32, 0.58), fixedrange = FALSE),
           yaxis3 = list(domain = c(0., 0.28), fixedrange = FALSE),
           title = Sy, xaxis = list(rangeslider = list(visible = F))) 
  
  # plot1
  # candleChart(zoo(Pr), TA=c(addMACD()), theme = chartTheme('white', up.col='green',dn.col='red'),
  #             show.grid = TRUE, major.ticks='auto', minor.ticks=TRUE)
  # https://rstudio-pubs-static.s3.amazonaws.com/495609_f8072f1c666043649730e8987ddfde77.html 
  
} # if Type == "plotly"
  
  
  
  # +------------------------------------------------------------------+
  # | data                                                             |
  # +------------------------------------------------------------------+

  return(list(
    df = df,
    PltMain_gg = PltMain_gg,
    PltIndctr_gg = PltIndctr_gg,
    # 
    PltMain_ly = PltMain_ly,
    PltIndctr_ly = PltIndctr_ly
    ))
} # Function CandleStickPlot_General

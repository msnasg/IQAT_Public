

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.




MACD_Div_Conv <- function(dat , Sy = "AKHABER", TF = "D", Windw = "All", LastPr0 = 200,
                          OHLC = NULL, Fast = 12, Slow = 26, Signal = 9, HowFar = 0.1){
  
  # Fast = 12; Slow = 26; Signal = 9; HowFar = 0.25
  # dat = RefTbl; Sy = "SHETEHRAN"; TF = "D"; Windw = "All"; LastPr0 = 200  ; Sy = SymbAll[513] 
  
  LastSignal = NULL; Plt = NULL; cndlplt = macdplt = NULL
  
  if(is.null(OHLC)){
    LastPr = as.numeric(LastPr0)
    # Pr = GetDataDB (dat, Sy , TF , Windw = Windw)$SeriesP
    Pr = dat
    OHLC = zoo(Pr)
    if("Volume" %in% colnames(OHLC)){
      OHLC <- data.frame(Date=index(OHLC), Open=OHLC$Open, High=OHLC$High, Low=OHLC$Low, Close=OHLC$Close, Volume = OHLC$Volume)  
    }else{
      OHLC <- data.frame(Date=index(OHLC), Open=OHLC$Open, High=OHLC$High, Low=OHLC$Low, Close=OHLC$Close)  
    }
    OHLC <- transform(OHLC, Date = as.POSIXct(OHLC$Date))  # is important for CandleStick plot
    if(nrow(OHLC) >  LastPr){OHLC <- tail(OHLC, LastPr); Pr = tail(Pr, LastPr)}else{LastPr = nrow(OHLC)}
  } #  if is.null(OHLC)

  # +------------------------------------------------------------------+
  # | MACD Series and Plot                                             |
  # +------------------------------------------------------------------+
  # macd  <- MACD( OHLC[,"Close"], Fast, Slow, Signal, maType="EMA" )
  # nrow(macd) == nrow(OHLC)
  # 
  # data <- data.frame(OHLC, MACD = macd, MACD.hist = macd[,"macd"] - macd[,"signal"])
  # data <- na.omit(data)
  
  if(nrow(OHLC) > 50){
    Fplt = CandleStickPlot_General(dat, Sy = Sy, TF = "D", Windw = "All", 
                                   LastPr = 200, Param = c(12,26,9), Type = "ggplot", Indicator = "MACD")
    cndlplt = Fplt$PltMain_gg 
  }

  if(!is.null(cndlplt)){
    macdplt = Fplt$PltIndctr_gg
    data = Fplt$df
    MACD_mcd = data$MACD.macd
    MACD_sgn = data$MACD.signal
    MACD_Hst = data$MACD.hist
    Low = data$Low
    High = data$High
    Date_S = data$Date
    LastSignal = NULL
    flag = TRUE
  }

  # +------------------------------------------------------------------+
  # | Functions to Search Last Indicator Peak                          |
  # +------------------------------------------------------------------+
  # https://www.mql5.com/en/code/viewcode/1806/128359/macd_divergence.mq5
  GetLastPeaks <- function(i){
    
  } # GetLastPeaks
  # +------------------------------------------------------------------+
  # | Functions to Search Last Indicator Trough                        |
  # +------------------------------------------------------------------+
  GetLastTrough <- function(i){
    
  } # GetLastTrough
  
  # +------------------------------------------------------------------+
  # | Find Bulish / Bearish Div-Conv                                   |
  # +------------------------------------------------------------------+
  N = nrow(data) - 2
if(!is.null(cndlplt)){
  for(i in N:5){ # Dates
    
    ## --- Catch Bullish Divergence
    isBullishDivergence = FALSE
    ## ---
    if(MACD_mcd[i] <= MACD_mcd[i-1] && MACD_mcd[i] < MACD_mcd[i-2] && MACD_mcd[i] < MACD_mcd[i+1]){
      # if current macd main is a bottom (lower than 2 previous and 1 next)
      currentExtremum = i
      lastExtremum = GetLastTrough(i)
      ## ---
      if(!is.null(lastExtremum)){
        if(MACD_mcd[currentExtremum] > MACD_mcd[lastExtremum] && Low[currentExtremum] < Low[lastExtremum]){
          isBullishDivergence = TRUE
          divergenceMsg = paste("Classical Bullish Divergence on: ", Date_S[i]) # Classic_Bullish
          Ind_Pr_x = c(Date_S[lastExtremum], Date_S[currentExtremum])
          Ind_y = c(MACD_mcd[lastExtremum], MACD_mcd[currentExtremum])
          Pr_y = c(Low[lastExtremum], Low[currentExtremum])
          
          if(currentExtremum > N *(1-HowFar) && flag){LastSignal = "Classic_Bullish"; flag = FALSE}
        }
        ## --- 
        if(MACD_mcd[currentExtremum] < MACD_mcd[lastExtremum] && Low[currentExtremum] > Low[lastExtremum]){
          isBullishDivergence = TRUE
          divergenceMsg = paste("Reverse Bullish Divergence on: ", Date_S[i]) # Reverse_Bullish 
          Ind_Pr_x = c(Date_S[lastExtremum], Date_S[currentExtremum])
          Ind_y = c(MACD_mcd[lastExtremum], MACD_mcd[currentExtremum])
          Pr_y = c(Low[lastExtremum], Low[currentExtremum])
          if(currentExtremum > N *(1-HowFar) && flag){LastSignal = "Reverse_Bullish"; flag = FALSE}
        }
        ## --- Bullish divergence is found, add plot or save lines
        if(isBullishDivergence) { 
          macdplt = macdplt + 
            geom_segment(aes_(x = Ind_Pr_x[1], y = Ind_y[1], xend = Ind_Pr_x[2], yend = Ind_y[2]))
          ## --- 
          cndlplt = cndlplt + 
            geom_segment(aes_(x = Ind_Pr_x[1], y = Pr_y[1], xend = Ind_Pr_x[2], yend = Pr_y[2]))
          }
      } # !is.null(lastExtremum)
  
      
    } # if Catch Bullish Divergence
    
    ## --- Catch Bearish Divergence
    isBearishDivergence = FALSE
    
    if(MACD_mcd[i] >= MACD_mcd[i-1] && MACD_mcd[i] > MACD_mcd[i-2] && MACD_mcd[i] > MACD_mcd[i+1]){
      ## --- if current macd main is a top (higher than 2 previous and 1 next)
      currentExtremum = i 
      lastExtremum = GetLastPeaks(i)
      ## ---  
      if(!is.null(lastExtremum)){
        if(MACD_mcd[currentExtremum] < MACD_mcd[lastExtremum] && High[currentExtremum] > High[lastExtremum]){
          isBearishDivergence = TRUE
          divergenceMsg = paste("Classical Bearish Divergence on: ", Date_S[i]) # Classic_Bearish
          Ind_Pr_x = c(Date_S[lastExtremum], Date_S[currentExtremum])
          Ind_y = c(MACD_mcd[lastExtremum], MACD_mcd[currentExtremum])
          Pr_y = c(High[lastExtremum], High[currentExtremum])
          if(currentExtremum > N *(1-HowFar) && flag){LastSignal = "Classic_Bearish"; flag = FALSE}
        }
        ## --- 
        if(MACD_mcd[currentExtremum] > MACD_mcd[lastExtremum] && High[currentExtremum] < High[lastExtremum]){
          isBearishDivergence = TRUE
          divergenceMsg = paste("Reverse Bearish Divergence on: ", Date_S[i]) # Reverse_Bearish
          Ind_Pr_x = c(Date_S[lastExtremum], Date_S[currentExtremum])
          Ind_y = c(MACD_mcd[lastExtremum], MACD_mcd[currentExtremum])
          Pr_y = c(High[lastExtremum], High[currentExtremum])
          if(currentExtremum > N *(1-HowFar) && flag){LastSignal = "Reverse_Bearish"; flag = FALSE}
        }
        ## -- Bearish divergence is found
        if(isBearishDivergence){
          macdplt = macdplt + 
            geom_segment(aes_(x = Ind_Pr_x[1], y = Ind_y[1], xend = Ind_Pr_x[2], yend = Ind_y[2]))
          ## --- 
          cndlplt = cndlplt + 
              geom_segment(aes_(x = Ind_Pr_x[1], y = Pr_y[1], xend = Ind_Pr_x[2], yend = Pr_y[2]))
        }
      } # !is.null(lastExtremum)
    } # if Catch Bearish Divergence
    
  } # For i
   
  cndlplt = cndlplt + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
# grid.arrange(cndlplt, macdplt,  nrow = 2, heights=c(3,2))
  
library(cowplot)
Plt = plot_grid(cndlplt, macdplt, nrow = 2, align="v", rel_heights = c(6,3))

} # if !isnull(cndlplt)

  return(list(LastSignal = LastSignal, Plt = Plt))
} # MACD_Div_Conv

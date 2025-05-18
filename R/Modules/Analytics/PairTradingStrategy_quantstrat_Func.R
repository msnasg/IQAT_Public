
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


PairTradingStrategy_quantstrat_Func <- function(Symb_1 = "TSLA", 
                                           Symb_2 = "AMZN", 
                                           TF = "D", 
                                           Windw = "All", 
                                           fromdate = "2021-01-01",
                                           InitCap = 1000,
                                           MaxPos = 1000,
                                           Model = "Basic",
                                           Currency = "USD"){
  
  {
    rm(list =ls(envir=.blotter), envir=.blotter)
    
    envir <<- .blotter <<- new.env()
    portfolio1.st <- 'MyPair'
    account.st <- 'pairs'
    rm.strat(portfolio1.st)
    env = parent.frame()
    
    # Parameters 
    lvls <- 3  
    
    # Data 
    # +-------------------------------------------+ Read DB
    
    Stock_A = Func_GetData(symbol = Symb_1, from = fromdate)
    Stock_B = Func_GetData(symbol = Symb_2, from = fromdate)
    
    Symbols = c(Symb_1, Symb_2)
    
    # f = GetDataDB(dat, Sys = Sys , TF = TF , Windw = Windw)
    # PriceList = f$SeriesP
    # ReturnList = f$SeriesR
    
    
    assign(Symb_1, Stock_A, envir = .GlobalEnv) 
    assign(Symb_2, Stock_B, envir = .GlobalEnv)
    
    
    startDate <- as.Date(xts::first(zoo::index(zoo::zoo(Stock_A))))
    endDate <- as.Date(xts::last(zoo::index(zoo::zoo(Stock_A))))
    
    FinancialInstrument::currency(Currency)
    
    FinancialInstrument::stock(Symb_1, currency = Currency, multiplier = 1)
    FinancialInstrument::stock(Symb_2, currency = Currency, multiplier = 1)
    
    # Initialize Portfolio, Account, and Orders
    blotter::initPortf(name = portfolio1.st, symbols = Symbols)
    
    blotter::initAcct(account.st, portfolios = portfolio1.st, initEq = InitCap)
    
    quantstrat::initOrders(portfolio = portfolio1.st)
    
    # osFUN need to know which symbol is leg 1 and which is leg 2 as well as values of MaxPos and lvls. 
    # So, create a slot in portfolio to hold this info.
    pair <- c(1, 2, MaxPos, lvls)
    
    names(pair) <- c(Symb_1, Symb_2, "MaxPos", "lvls")
    
    .blotter[[paste('portfolio', portfolio1.st, sep = '.')]]$pair <- pair
    .blotter[[paste('portfolio', portfolio1.st, sep='.')]]$symbol <- Symbols
    
    # ------------------------------------------------------------------------ #
    
    # Create initial position limits and levels by symbol, allow 3 entries for long and short if lvls = 3.
    addPosLimit(portfolio = portfolio1.st, 
                timestamp = startDate, 
                symbol = Symb_1, 
                maxpos = MaxPos, 
                longlevels = lvls, 
                minpos = -MaxPos, 
                shortlevels = lvls)
    
    addPosLimit(portfolio = portfolio1.st, 
                timestamp = startDate, 
                symbol = Symb_2, 
                maxpos = MaxPos, 
                longlevels = lvls,
                minpos = -MaxPos, 
                shortlevels = lvls)
    
    # Create a strategy object 
    PairStrat <- quantstrat::strategy('PairStrat')
    
    # ------------------------------------------------------------------------ #
    # Indicator function
    calcRatio <- function(x) { 

    }
    # Indicator used for determining entry/exits
    Ratio <- calcRatio(c(Symb_1, Symb_2))  
    
    # Store hedge ratio in portfolio so that it's available for order sizing function. 
    # In this example, the hedge ratio happens to be the same as the Ratio indicator.
    .blotter[[paste('portfolio', portfolio1.st, sep = '.')]]$HedgeRatio <- Ratio
    
    # and make a function to get the most recent HedgeRatio
    getHedgeRatio <- function(portfolio, timestamp) {
    }
    
    ##################################################
    # Create an indicator - BBands on the Ratio
    PairStrat <- quantstrat::add.indicator(strategy = PairStrat, 
                               name = "calcRatio", 
                               arguments = list(x = c(Symb_1, Symb_2)))
    
    SD <- 2 #  BBands SMA ?
    N <- 20 # ?
    PairStrat <- quantstrat::add.indicator(strategy = PairStrat, 
                                           name = "BBands", 
                                           arguments = list(HLC = quote(Ratio), 
                                                            sd = 2, 
                                                            n = 20, 
                                                            maType = 'SMA'))

    PairStrat <- quantstrat::add.signal(strategy = PairStrat, 
                            name = "sigCrossover", 
                            arguments = list(columns = c("Ratio", "up"), 
                                             relationship = "lt"), 
                            label = "cross.up")
    
    PairStrat <- quantstrat::add.signal(strategy = PairStrat, 
                                        name = "sigCrossover", 
                            arguments = list(columns = c("Ratio", "dn"),  
                                             relationship = "gt"),  
                            label = "cross.dn")
    
    PairStrat <- quantstrat::add.signal(strategy = PairStrat, 
                                        name = "sigCrossover", 
                            arguments = list(columns = c("Ratio","mavg"), 
                                             relationship = "lt"), 
                            label = "cross.mid.fa")
    
    PairStrat <- quantstrat::add.signal(strategy = PairStrat, 
                                        name = "sigCrossover", 
                            arguments = list(columns = c("Ratio", "mavg"), 
                                             relationship = "gt"),  
                            label = "cross.mid.fb")
    
    #######################_ORDER SIZING FUNCTION_##############################
    # make an order sizing function, check to see which stock it is. 
    # If it's the second stock, reverse orderqty and orderside
    
    osSpreadMaxPos <- function (data, timestamp, orderqty, ordertype, orderside, 
                                 portfolio, symbol, ruletype, ..., orderprice) {

    }
    
    ################################################################################
    # Create entry and exit rules for longs and for shorts. Both symbols will get the same buy/sell signals, 
    # but osMaxPos will reverse those for the second symbol.
    # orderqty's are bigger than PosLimits allow. osMaxPos will adjust the orderqty down to 1/3 the max allowed. 
    # (1/3 is because we are using 3 levels in PosLimit)
    PairStrat <- quantstrat::add.rule(strategy = PairStrat, 
                          name = 'ruleSignal', 
                          arguments = list(sigcol = "cross.dn", 
                                           sigval = TRUE, 
                                           orderqty = 1e6, 
                                           ordertype = 'market', 
                                           orderside = NULL, 
                                           osFUN = 'osSpreadMaxPos'), 
                          type = 'enter')
    
    PairStrat <- quantstrat::add.rule(strategy = PairStrat, 
                          name = 'ruleSignal', 
                          arguments = list(sigcol = "cross.up", 
                                           sigval = TRUE, 
                                           orderqty = -1e6, 
                                           ordertype = 'market', 
                                           orderside = NULL, 
                                           osFUN = 'osSpreadMaxPos'), 
                          type = 'enter')
    
    PairStrat <- quantstrat::add.rule(strategy = PairStrat, 
                                      name = 'ruleSignal', 
                                      arguments = list(sigcol = "cross.mid.fb", 
                                                       sigval = TRUE, 
                                                       orderqty = 'all', 
                                                       ordertype = 'market',
                                                       orderside = NULL), 
                                      type = 'exit')
    
    PairStrat <- quantstrat::add.rule(strategy = PairStrat, 
                                      name = 'ruleSignal', 
                                      arguments = list(sigcol = "cross.mid.fa", 
                                                       sigval = TRUE, 
                                                       orderqty = 'all', 
                                                       ordertype = 'market', 
                                                       orderside = NULL), 
                                      type = 'exit')
    
  }
  #####################################################################
  ## for debugging
  # applySignals(strategy = PairStrat, mktdata = applyIndicators(strategy = PairStrat, mktdata = get(Sy1)))
  # applySignals(strategy = PairStrat, mktdata = applyIndicators(strategy = PairStrat, mktdata = get(Sy2)))
  
  out1 <- quantstrat::applyStrategy(strategy = PairStrat, portfolios = portfolio1.st)
  
  blotter::updatePortf(Portfolio = portfolio1.st, Dates = paste("::", as.Date(Sys.time()), sep = ''))
  
  blotter::updateAcct(account.st, Dates = paste(startDate, endDate, sep ="::"))
  
  blotter::updateEndEq(account.st, Dates=paste(startDate, endDate, sep="::"))
  # getEndEq(account.st, Sys.time())
  
  #dev.new()
  # plt1 = chart.Posn(Portfolio = portfolio1.st, Symbol = Sy1)
  #dev.new()
  # plt2 = chart.Posn(Portfolio = portfolio1.st, Symbol = Sy2)
  #dev.new()
  # plt1 = chartSeries(Cl(get(Sy1))/Cl(get(Sy2)), TA="addBBands(n=20,sd=2)", name = paste("Spread: ", Sy1 ," &", Sy2, sep=" "),
  #                    theme = chartTheme("white"))
  
  #######################################
  ret1 <- blotter::PortfReturns(account.st)
  ret1$total <- base::rowSums(ret1)
  
  Totalret = ret1$total
  colnames(ret1) = c(paste("Ret.A",sep=""), paste("Ret.B",sep=""), "Pair")  # Sy1 Sy2
  
  # charts.PerformanceSummary(Totalret, geometric=F, colorset = bluefocus, wealth.index=T, main = "Pairtrading Performance")
  
  mtcorr = cbind(quantmod::Cl(Stock_A), quantmod::Cl(Stock_B), ret1)
  colnames(mtcorr) <- c(paste("Cl.A", sep=""), paste("Cl.B",sep=""), colnames(ret1))
  corr <- round(stats::cor(mtcorr), 2)
  corrplt <- ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE) +  labs(title = "Corelation Plot")
  

  x <- data.frame(Date = zoo::index(zoo::zoo(ret1)), 
                  v1=ret1[,1]*100,
                  v2=ret1[,2]*100,
                  v3=ret1[,3]*100)
  
  colnames(x) <- c("Date", Symb_1, Symb_2, "PairTrading")
  
  data <-  reshape2::melt(x, id.vars = "Date")
  
  # ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
  # ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
  
  boxplt <-  ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot(position=position_dodge(1)) +
    scale_color_grey() + theme_classic() + ggtitle("Returns Box plot")
  
  violinplt <-  ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_violin(position=position_dodge(1)) +
    scale_color_grey() + theme_classic() + ggtitle("Returns Violin plot")
  
  # ggplot(data, aes(x=variable, y=value)) +  geom_violin()
  
  # x <- data.frame(Date = index(zoo(ret1)), v3=ret1[,3]*100)
  # colnames(x) <- c("Date", "PairTrading")
  # data <-  reshape2::melt(x, id.vars = "Date")
  
  densityplt <- ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25) +
    scale_color_grey() + theme_classic() + ggtitle("Returns Density plot")
  
  # Me
  # trade statistics
  tStats <- blotter::tradeStats(Portfolios = portfolio1.st, 
                       use = "trades", 
                       inclZeroDays = FALSE)
  
  tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
  tStats = tStats[,-(1:2)]
  
  # print(data.frame(t(tStats[,-c(1,2)])))
  #SumTble = matrix(0, nrow=4, ncol = 1, dimnames = list(NULL, "Strategy"))
  
  aggPF <- (sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses) - 1 ) * 100
  
  
  aggCorrect <- round(mean(tStats$Percent.Positive),2)
  
  numTrades <- sum(tStats$Num.Trades)
  
  meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE)
  
  # daily and duration statistics
  dStats <- dailyStats(Portfolios = portfolio1.st, use = "Equity")
  
  rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
  
  # print(data.frame(t(dStats)))
  
  durStats <- IKTrading::durationStatistics(Portfolio = portfolio1.st, 
                                            Symbols = sort(c(Symb_1, Symb_2)))
  
  indivDurStats <- IKTrading::durationStatistics(Portfolio = portfolio1.st, 
                                                 Symbols = sort(c(Symb_1, Symb_2)), 
                                                 aggregate=FALSE)
  # print(t(durStats))
  # print(t(indivDurStats))
  
  # market exposure
  tmp <- list()
  length(tmp) <- length(c(Symb_1, Symb_2))
  
  for(i in 1:nrow(dStats)) {
    # totalDays <- nrow(get(rownames(dStats)[i]))
    totalDays <- dStats[i, "Total.Days"]
    mktExposure <- dStats$Total.Days[i]/totalDays
    tmp[[i]] <- c(rownames(dStats)[i], round(mktExposure, 3))
  }
  
  mktExposure <- data.frame(do.call(rbind, tmp))
  colnames(mktExposure) <- c("Symbol", "MktExposure")
  # print(mktExposure)
  # print(mean(as.numeric(as.character(mktExposure$MktExposure))))
  
  # portfolio cash PL
  portString <- paste0("portfolio.", portfolio1.st)
  
  portPL <- .blotter[[portString]]$summary$Net.Trading.PL
  
  # Cash Sharpe
  SharpeRatio.Y <- PerformanceAnalytics::SharpeRatio.annualized(portPL, geometric=FALSE)
  
  # Portfolio comparisons 
  instRets <- blotter::PortfReturns(account.st)
  
  # Correlations
  instCors <- stats::cor(instRets)
  diag(instRets) <- NA
  corMeans <- base::rowMeans(instCors, na.rm=TRUE)
  names(corMeans) <- gsub(".DailyEndEq", "", names(corMeans))
  # print(round(corMeans,3))
  # mean(corMeans)
  
  portfRets <- xts::xts(base::rowMeans(instRets)* ncol(instRets), 
                        order.by = zoo::index(zoo::zoo(instRets))) # or rowSums(instRets)
  
  portfRets <- portfRets[!is.na(portfRets)]
  cumPortfRets <- cumprod(1 + portfRets)
  firstNonZeroDay <- as.character(zoo::index(zoo::zoo(portfRets))[min(which(portfRets!=0))])
  
  # getSymbols("SPY", from=firstNonZeroDay, to=endDate)
  Stock_A_rets <- base::diff(log(quantmod::Cl(Stock_A)))[-1]
  
  cum_A_rets <- cumprod(1 + Stock_A_rets)
  comparison <- cbind(cumPortfRets, cum_A_rets)
  colnames(comparison)  <- c("strategy", Symb_1)
  
  PerformanceAnalytics::SharpeRatio.annualized(portfRets)
  PerformanceAnalytics::Return.annualized(portfRets)
  PerformanceAnalytics::maxDrawdown(portfRets)
  
  dailyRetComparison <- cbind(portfRets, Stock_A_rets)
  colnames(dailyRetComparison)  <- c("strategy", Symb_1)
  # round(apply.yearly(dailyRetComparison, Return.cumulative),3)
  # round(apply.yearly(dailyRetComparison, SharpeRatio.annualized),3)
  # round(apply.yearly(dailyRetComparison, maxDrawdown),3)
  
  ## Other plots like totalret
  final_acct <- getAccount(account.st)
  end_eq <- final_acct$summary$End.Eq
  returns <- PerformanceAnalytics::Return.calculate(end_eq, method="log")
  
  # pltperf <- PerformanceAnalytics::charts.PerformanceSummary(returns,
  #                                                 colorset = bluefocus,
  #                                                 main = "Strategy Performance")
  
  returns_2 <- PortfReturns(account.st)
  colnames(returns_2) <- Symbols
  returns_2 <- na.omit(cbind(returns_2,Return.calculate(end_eq)))
  names(returns_2)[length(names(returns_2))] <- "PairTrading"
  returnsCum <- returns_2[,c("PairTrading", Symbols)]
  # round(tail(returnsCum,5),6)
  
  pltRCum <-  PerformanceAnalytics::chart.CumReturns(returnsCum, 
                               colorset = rich10equal, 
                               legend.loc = "topleft", 
                             main = "Strategy Cumulative Returns")
  
  # https://statsmaths.github.io/stat395-f17/assets/final_project/amarnani.html
  
  # boxplt = 
  PerformanceAnalytics::chart.Boxplot(returnsCum, main = "Strategy Returns", colorset = rich10equal)
  
  
  ##
  Tbl = data.frame(t(tStats))
  Hold_Ret = ( getEndEq(account.st, Sys.time()) / InitCap - 1 ) * 100
  
  Tbl1 = Tbl[which(rownames(Tbl)  %in% c("Percent.Positive", "Percent.Negative", "Profit.Factor",
                                         "Ann.Sharpe", "Profit.To.Max.Draw", "Avg.WinLoss.Ratio")),]
  
  
  Tbl1 = formattable::formattable(Tbl1)
  Tbl1 = formattable::formattable(Tbl1, align = c("l", rep("r", NCOL(Tbl1) - 1)))
  
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  Tbl2 = Tbl[which(rownames(Tbl) %!in% c("Percent.Positive", "Percent.Negative", "Profit.Factor",
                                         "Ann.Sharpe", "Profit.To.Max.Draw", "Avg.WinLoss.Ratio")),]
  ### removed
  Tbl2 = Tbl2[-which(rownames(Tbl2) %in% c("Med.WinLoss.Ratio", "Std.Dev.Trade.PL", "Std.Err.Trade.PL",
                                           "Med.Win.Trade","Med.Losing.Trade","Med.Daily.PL","Std.Dev.Daily.PL",
                                           "Std.Err.Daily.PL", "Med.Trade.PL")),]
  
  Tbl2  = formattable(Tbl2, align = c("l", rep("r", NCOL(Tbl2) - 1)))
  
  TotalTable = Tbl[-which(rownames(Tbl) %in% c("Med.WinLoss.Ratio", "Std.Dev.Trade.PL", "Std.Err.Trade.PL",
                                               "Med.Win.Trade","Med.Losing.Trade","Med.Daily.PL","Std.Dev.Daily.PL",
                                               "Std.Err.Daily.PL", "Med.Trade.PL", "Max.Equity", "Min.Equity",
                                               "End.Equity", "Profit.Factor") ),]
  
  
  ##############################################################################

  SumTbl = NULL; plt1 = NULL; plt2 = NULL; P_Cum_Ret = NULL; FindPairTbl = NULL
  
  SyInds1 = "" # unique( as.character(ExclID$IDEn_IndustrySymbol[which(ExclID$IDEn_Symbol == Sy1)]))
  SyInds2 = "" # unique( as.character(ExclID$IDEn_IndustrySymbol[which(ExclID$IDEn_Symbol == Sy2)]))  
  
  # +-------------------------------------------+ Read DB
  Sys = c(Symb_1, Symb_2)
  
  PricePair <- xts::cbind.xts(Symb_1 = Stock_A[, grep("close", tolower(colnames(Stock_A)))], 
                         Symb_2 = Stock_B[, grep("close", tolower(colnames(Stock_B)))])
  
  # Estimate parameters & plot spread
  reg <- PairTrading::EstimateParameters(PricePair, method = lm)
  
  df <- data.frame(Date = zoo::index(zoo::zoo(PricePair)), 
                   Symb_1 = log(PricePair[,1]), 
                   Symb_2 = log(PricePair[,2]),  
                   Spread = reg$spread, 
                   l2 = NA, 
                   u2= NA,  
                   T1 = NA, 
                   T2 = NA, 
                   OpCL = NA, 
                   OffSet = NA,
                   P1 = PricePair[,1], 
                   P2 = PricePair[,2], 
                   Cap = NA, 
                   Ret = NA)
  
  colnames(df) = c("Date", Symb_1, Symb_2, 
                   "Spread",  "l2", "u2", "Trade1", "Trade2", "OpCL", "OffSet", "P1", "P2", "Cap", "Ret")
  
  # 1 and 2 sd
  u1 = mean(reg$spread) + 1 * sd(reg$spread) 
  l1 = mean(reg$spread) - 1 * sd(reg$spread)
  u2 = mean(reg$spread) + 1.5 * sd(reg$spread) 
  l2 = mean(reg$spread) - 1.5 * sd(reg$spread)
  
  bn = 50 # round(0.03 * nrow(df))
  typOpCL = Tradetype = NA
  
  for(kk in (bn+1):nrow(df)){
    if(kk < (bn+1)){
      # df[158,"Date"]
      # spr  = reg$spread[1:bn]
      # u2 = mean(spr) + 1.5 * sd(spr) 
      # l2 = mean(spr) - 1.5 * sd(spr)
      # df$Trade1[1:bn] <- ifelse(df$Spread[1:bn] > u2, "B", ifelse(df$Spread[1:bn] < l2, "S", NA))
      # df$Trade2[1:bn] <- ifelse(df$Spread[1:bn] > u2, "S", ifelse(df$Spread[1:bn] < l2, "B", NA))
    }else{
      spr  = reg$spread[1:(kk-1)]
      df$u2[kk] = u2 = mean(spr) + 2 * sd(spr) 
      df$l2[kk] = l2 = mean(spr) - 2 * sd(spr)
      # df$Trade1[kk] <- ifelse((df$Spread[kk-1] > u2 && df$Spread[kk] < u2), "B",
      #                         ifelse((df$Spread[kk-1] < l2 && df$Spread[kk] > l2), "S", NA))
      # df$Trade2[kk] <- ifelse((df$Spread[kk-1] > u2 && df$Spread[kk] < u2), "S",
      #                         ifelse((df$Spread[kk-1] < l2 && df$Spread[kk] > l2), "B", NA))
      
      if(df$Spread[kk-1] > u2 && df$Spread[kk] < u2 && is.na(Tradetype)){df$Trade1[kk] <- "B"; Tradetype = 1}
      if(df$Spread[kk-1] < l2 && df$Spread[kk] > l2 && is.na(Tradetype)){df$Trade1[kk] <- "S"; Tradetype = 2}
      if(df$Spread[kk-1] > u2 && df$Spread[kk] < u2 ){df$Trade2[kk] <- "S"}
      if(df$Spread[kk-1] < l2 && df$Spread[kk] > l2 ){df$Trade2[kk] <- "B"}
      
      ## Fixed
      # df$u2[kk] = u2 
      # df$l2[kk] = l2 
      # df$Trade1[kk] <- ifelse(df$Spread[kk] > u2, "B", ifelse(df$Spread[kk] < l2, "S", NA))
      # df$Trade2[kk] <- ifelse(df$Spread[kk] > u2, "S", ifelse(df$Spread[kk] < l2, "B", NA))
      
      if(df$Spread[kk-1] > 0 && df$Spread[kk] < 0){df$OffSet[kk] = "Off";  typOpCL = "Closed"}
      if(df$Spread[kk-1] < 0 && df$Spread[kk] > 0){df$OffSet[kk] = "Off";  typOpCL = "Closed"}
      
      if(!is.na(Tradetype) && !is.na(df$Trade2[kk])){ 
        if(Tradetype == 1 && df$Trade2[kk] == "B"){df$OffSet[kk] = "Off";  typOpCL = "Closed"}
        if(Tradetype == 2 && df$Trade2[kk] == "S"){df$OffSet[kk] = "Off";  typOpCL = "Closed"}
        # if(df$Trade1[kk] == "B"){Tradetype = 1}
        # if(df$Trade1[kk] == "S"){Tradetype = 2}
      }
      
      if(!is.na(df$Trade1[kk])){typOpCL = "Open"}
      if(!is.na(df$OffSet[kk]) && df$OffSet[kk] == "Off"){Tradetype = NA}
      df$OpCL[[kk]] = typOpCL
      
    }
  }
  
  # just for chart
  u1 = mean(reg$spread) + 1 * sd(reg$spread) 
  l1 = mean(reg$spread) - 1 * sd(reg$spread)
  u2 = mean(reg$spread) + 2 * sd(reg$spread) 
  l2 = mean(reg$spread) - 2 * sd(reg$spread)
  
  
  for(ii in 1:(nrow(df)-1)){
    if(df$Spread[ii] > 0 && df$Spread[ii + 1] < 0){df$OffSet[ii+1] = "Off"}
    if(df$Spread[ii] < 0 && df$Spread[ii + 1] > 0){df$OffSet[ii+1] = "Off"}
  }
  
  nr = which(df$Trade1 == "S" | df$Trade1 == "B")
  if(length(nr) >= 2){
    for(ii in 1:(length(nr)-1)){
      if(nr[ii+1] == nr[ii] + 1){ df$Trade1[nr[ii+1]] = df$Trade2[nr[ii+1]] = NA  }
    }  }
  
  
  nr = which(df$OffSet == "Off")
  
  if(length(nr) >= 2){
    for(ii in 1:(length(nr)-1)){
      if(nr[ii+1] == nr[ii] + 1){ df$OffSet[nr[ii+1]] = df$OffSet[nr[ii+1]] = NA  }
    } }
  
  df1 = reshape2::melt(df[,c(1:4)], id.vars = "Date")
  df1 = df1[which(df1$variable != "Spread"),]
  #
  Tradeplt = plt1 = ggplot(data=df1, aes(x=Date, y=value, colour=variable)) + geom_line() + theme_bw()+
    theme(legend.position = "top", legend.box = "horizontal", legend.title = element_blank()) + xlab("") + 
    ylab("Log Price") + 
    #  geom_label_repel(aes(y = df[,2], label = df$Trade1),  data = df,   col = "#619CFF", size = 3.5)
    geom_text(aes(y = df[,2], label = df$Trade1), data = df, col = "black", vjust = -1.5) +
    geom_text(aes(y = df[,3], label = df$Trade2), data = df,  col = "black", vjust = -1.5) +
    # geom_text(aes(y = df[,3] + 1, label = df$OffSet), data = df,  col = "black", vjust = -1.5) +
    theme(plot.margin = margin(.5, .5, .5, .5, "cm"))
  
  # , axis.text.x=element_blank()
  Spreadplt = plt2 = ggplot(df) + 
    geom_col(aes(x = Date, y = Spread), size = 1, color = "darkblue", fill = "white") + theme_bw() + 
    geom_hline(yintercept = u1, linetype="dashed", color = "green", size=0.5) +
    geom_hline(yintercept = u2, linetype="dashed", color = "darkgreen", size=1) +
    geom_hline(yintercept = l1, linetype="dashed", color = "red", size=0.5) +
    geom_hline(yintercept = l2, linetype="dashed", color = "darkred", size=1) + 
    theme(plot.margin = margin(.5, .5, .5, .5, "cm")) + xlab("")
  
  
  ##########################################################################################
  # Trade Cum Returns
  Fees <- data.frame(B = (1 + 0.00464), S = (1-0.00575), T = (1 + 0.01039))
  Buy = Sell = vector()
  OrderInd = OffInd = AvrInd = Cap = vector()
  m = mm = Trd = 1
  Flag = "Off" 
  Type = "" 
  for(ii in 1:nrow(df)){ # df[91,"Date"]
    # Open Order
    if(!is.na(df$Trade1[ii])){
      if(df$Trade1[ii] == "S" && df$Trade2[ii] == "B" && Flag == "Off"){
        if(df[ii,"P1"] * Fees$S >= df[ii,"P2"] * Fees$B){
          ratio = ceiling(df[ii,"P1"] * Fees$S / df[ii,"P2"] * Fees$B)
          Cap[m] = (ratio * df[ii,"P2"] * Fees$B) - df[ii,"P1"] * Fees$S
          df[ii,"Cap"] = Cap[m]
        }
        if(df[ii,"P1"] * Fees$S < df[ii,"P2"] * Fees$B){
          ratio = floor(df[ii,"P2"] * Fees$B / df[ii,"P1"] * Fees$S)
          Cap[m] = df[ii,"P2"] * Fees$B - (ratio * df[ii,"P1"] * Fees$S)
          df[ii,"Cap"] = Cap[m]
        }
        
        Sell[m] = df[ii,"P1"] * Fees$S
        Buy[m] = df[ii,"P2"] * Fees$B
        OrderInd[m] = ii
        Flag = "Trade"
        Type = "Lower"
        m = m + 1
        Trd = Trd + 1
      }
      
      if(df$Trade1[ii] == "B" && df$Trade2[ii] == "S" && Flag == "Off"){
        if(df[ii,"P1"] * Fees$S >= df[ii,"P2"] * Fees$B){
          ratio = ceiling(df[ii,"P1"] * Fees$S / df[ii,"P2"] * Fees$B)
          Cap[m] = (ratio * df[ii,"P2"] * Fees$B) - df[ii,"P1"] * Fees$S
          df[ii,"Cap"] = Cap[m]
        }
        if(df[ii,"P1"] * Fees$S < df[ii,"P2"] * Fees$B){
          ratio = floor(df[ii,"P2"] * Fees$B / df[ii,"P1"] * Fees$S)
          Cap[m] = df[ii,"P2"] * Fees$B - (ratio * df[ii,"P1"] * Fees$S)
          df[ii,"Cap"] = Cap[m]
        }
        Sell[m] = df[ii,"P2"] * Fees$S
        Buy[m] = df[ii,"P1"] * Fees$B
        OrderInd[m] = ii
        Flag = "Trade"
        Type = "Upper"
        m = m + 1
        Trd = Trd + 1
      }
      # Average Transaction
      # if(df$Trade1[ii] == "S" && df$Trade2[ii] == "B"  && Flag != "Off"){
      #   Buy[m-1] = (Buy[m-1] + (df[ii,"P2"] * Fees$B) ) / 2
      #   Sell[m-1] = (Sell[m-1] + (df[ii,"P1"] * Fees$S))/2
      #   AvrInd[m-1] = ii
      #   Trd = Trd + 1
      # }
      # if(df$Trade1[ii] == "B" && df$Trade2[ii] == "S" && Flag != "Off"){
      #   Buy[m-1] = (Buy[m-1] + (df[ii,"P1"] * Fees$B) ) / 2
      #   Sell[m-1] = (Sell[m-1] + (df[ii,"P2"] * Fees$S))/2
      #   AvrInd[m-1] = ii
      #   Trd = Trd + 1
      # }
    } # !is.na
    
    if(ii == nrow(df)) {df$OffSet[ii] = "Off"}
    # Offset
    if(!is.na(df$OffSet[ii]) && Flag == "Trade" && is.na(df$Trade1[ii]) ){
      if(Type == "Lower"){
        df[ii,"Cap"] = Cap[m-1]
        df$Ret[ii] = ( (Sell[m-1] - df[ii,"P1"] * Fees$B) + (df[ii,"P2"] * Fees$S - Buy[m-1]) ) / abs(Cap[m-1]) # abs(Sell[m-1] - Buy[m-1])
        Flag = "Off" 
        OffInd[mm] = ii
        mm = mm + 1
      }
      if(Type == "Upper"){
        df[ii,"Cap"] = Cap[m-1]
        df$Ret[ii] = ( (df[ii,"P1"] * Fees$B - Buy[m-1] ) + (Sell[m-1] - df[ii,"P2"] * Fees$B ) ) / abs(Sell[m-1] - Buy[m-1])
        Flag = "Off" 
        OffInd[mm] = ii
        mm = mm + 1
      }
      
    } # if Offset
    
    
  }
  # Buy;Sell;OrderInd;AvrInd; OffInd
  
  RetXts = na.omit( xts(df$Ret, order.by = df$Date) )
  Retdf = na.omit(data.frame(Date = df$Date, Returns = df$Ret))
  
  P_Cum_Ret = ggplot(Retdf, aes(x = Date, y = cumsum(Returns))) + geom_line(colour = "blue") + geom_point(colour = "red") + 
    xlab("Date") + ylab("Cumulative Returns") + theme_bw() + #ggtitle(paste("BackTest Cumulative Returns")) 
    geom_text(label =  as.character( paste(round(cumsum(RetXts) *100, 2), " %" ) ), 
              nudge_x = 1, nudge_y = 0.01, check_overlap = T, colour = "green4" ) + theme(plot.margin = margin(.5, .5, .5, .5, "cm"))
  
  #pltmyStr = grid.arrange(plt1, plt2 , P_Cum_Ret, nrow = 3, heights=c(4,2,3))
  pltmyStr = list(plt1, plt2 , P_Cum_Ret)
  
  
  ###################################################################################
  SumTbl = data.frame( matrix(0, nrow = 8, ncol = 1, 
                              dimnames = list(c("Trades", "offSets", "Avr.Ret", "Cum.Ret", 
                                                "Annual.Ret", "MaxDrawDown", "SharpeRatio" , "IsStationary"),
                                              c("Values" ) ) ) )
  SumTbl["Trades",1] <- Trd
  SumTbl["offSets",1] <- mm - 1
  SumTbl["Avr.Ret",1] <- round(mean(RetXts)* 100, 2)
  SumTbl["Cum.Ret",1] <- round(Return.cumulative(RetXts, geometric = F) * 100, 2)
  SumTbl["Annual.Ret",1] <- round(Return.annualized(RetXts)* 100, 2)
  SumTbl["MaxDrawDown",1] <- round(maxDrawdown(RetXts), 2)
  SumTbl["SharpeRatio",1] <- round(SharpeRatio.annualized(RetXts, Rf = 0.2), 2)
  test = IsStationary(reg$spread, 0.1)
  if(any(test)){SumTbl["IsStationary",1] <- "TRUE"}else{SumTbl["IsStationary",1] <- "FALSE"}
  
  
  TotalTable = data.frame(Items = rownames(TotalTable), TotalTable)
  rownames(TotalTable) <- NULL
  #####################################################################################################
  return(list(TotalTable = TotalTable, Tbl1 = Tbl1, Tbl2 = Tbl2, portfolio1.st = portfolio1.st, pltRCum = pltRCum,
              Totalret = Totalret, corrplt = corrplt, boxplt = boxplt, violinplt = violinplt,
              densityplt = densityplt, # pltmyStr = pltmyStr, 
              P_Cum_Ret = P_Cum_Ret,
              Tradeplt = Tradeplt, Spreadplt = Spreadplt,
              SumTbl = SumTbl))
}  # PairTradingStrategy_quantstrat_Func

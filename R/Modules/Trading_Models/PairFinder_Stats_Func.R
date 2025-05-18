

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


PairFinder_Stats_Func <- function(input, output, session,
                                  L = NULL,
                                  Symb = NULL){
  
  cat("PairFinder_Stats_Func ...")

  output$id_TradingModels_PairFinder_Stats_UI <- renderUI({
    fluidRow(
      col_12(
        plotlyOutput("id_TradingModels_PairFinder_Stats_Price_lineplotly", height = "400px", width = "100%")
      ),
      col_6(
        plotlyOutput("id_TradingModels_PairFinder_Stats_MonthlyPrice_lineplotly", height = "400px", width = "100%")
      ),
      col_6(
        plotlyOutput("id_TradingModels_PairFinder_Stats_MonthlyReturn_lineplotly", height = "400px", width = "100%")
      ),
      col_6(
        plotlyOutput("id_TradingModels_PairFinder_Stats_Price_distplotly", height = "400px", width = "100%")
      ),
      col_6(
        plotlyOutput("id_TradingModels_PairFinder_Stats_Return_distplotly", height = "400px", width = "100%")
      ),
      col_12(
        plotlyOutput("id_TradingModels_PairFinder_Stats_Return_lineplotly", height = "400px", width = "100%")
      )
    )
  })
  
  # print(Symb)
  # Symb = "Akhaber"
  
  # -------------------------------------------------------------------------- #
  # Server
  # -------------------------------------------------------------------------- #
 
  if(length(L) >= 1){
    
    price_df <- L[["price_df_all"]][, c("date", Symb)]
    price_xts <- L[["price_xts_all"]][, Symb]
    colnames(price_df) <-  c("date", "price")
    colnames(price_xts) <- c("price")
    
    ret_df <- L[["ret_df_all"]][, c("date", Symb)]
    ret_xts <- L[["ret_xts_all"]][, Symb]
    colnames(ret_df) <- c("date", "return")
    colnames(ret_xts) <- c("return")
  
    # ------------------------------------- #
    output$id_TradingModels_PairFinder_Stats_Price_lineplotly <- renderPlotly({
      
      plot_ly(price_df, x = ~date, y = ~price, type = 'scatter', 
              mode = 'lines') %>% layout(title = Symb)
      }) # plot_ly price
    
    # ------------------------------------- #
    output$id_TradingModels_PairFinder_Stats_Return_lineplotly <- renderPlotly({
      
      plot_ly(ret_df, x = ~date, y = ~return, type = 'scatter', 
              mode = 'lines') # %>% layout(title = Symb)
      }) # plot_ly return
    
    # ------------------------------------- #
    
    output$id_TradingModels_PairFinder_Stats_Price_distplotly <- renderPlotly({
      base::suppressWarnings({
        ggplotly(ggplot(price_df, aes(x = price)) + geom_density(alpha=.1) + 
                   theme_minimal())
        })
      })
  
    # ------------------------------------- #
    output$id_TradingModels_PairFinder_Stats_Return_distplotly <- renderPlotly({
      base::suppressWarnings({
        ggplotly(ggplot(ret_df, aes(x = return)) + geom_density(alpha=.1) + 
                   theme_minimal() )
        })
      })
    
    # ------------------------------------- #
    output$id_TradingModels_PairFinder_Stats_MonthlyPrice_lineplotly <- renderPlotly({
      base::suppressWarnings({
        dfmelt <- MonthlyChart_Func(price_xts, type = "log")
        
        ggplotly(ggplot(dfmelt, aes(x = variable, y = value, group = Vars, color = Vars)) + geom_point() +
                   geom_path() + theme_bw() + ggtitle(paste("Monthly Price Developments of ", Symb)) +
                   xlab("Months") + ylab("Log value") + scale_color_discrete(name = "Years"))
      })
    })
    
    # ------------------------------------- #
    output$id_TradingModels_PairFinder_Stats_MonthlyReturn_lineplotly <- renderPlotly({
      base::suppressWarnings({
        
        dfmelt <- MonthlyChart_Func(ret_xts, type = "return")
        
        ggplotly(ggplot(dfmelt, aes(x = variable, y = value, group = Vars, color = Vars)) + geom_point() +
                   geom_path() + theme_bw() + ggtitle(paste("Monthly Return Developments of ", Symb)) +
                   xlab("Months") + ylab("value") + scale_color_discrete(name = "Years"))
      })
    })
    
    
    # 
  } # if
  
   cat(" Done. \n")
  
} # PairFinder_Stats_Func



RMSE <- function(sim, obs){

}


Jal2Greg_Func <- function(d){

  
} # jal2greg_Func



Imputation_MissingValues_Func <- function(df, 
                                          ret_method = "log",
                                          type = "Aggregated"){
  
  
  if(grepl("aggregat", type, ignore.case = T)){    

  } # if aggregated
  
  if(grepl("Most recent", type, ignore.case = T)){

  } # if Most recent
  
  if(grepl("Next observation", type, ignore.case = T)){

  } # if NOCB
  
  if(grepl("Linear interpolation", type, ignore.case = T)){
    
  } # if Linear interpolation
  
  if(grepl("Seasonal", type, ignore.case = T)){
    
  } # if  Kalman filter
  
  # -------------------------------------------------------------------------- #
  base::suppressPackageStartupMessages({ 
    base::suppressWarnings({
      price_df_all = data.frame(date = zoo::index(zoo(price_xts_all)), coredata(price_xts_all))
    })
  })
  
  # -------------------------------------------------------------------------- #
  if(grepl("tsrobprep", type, ignore.case = T) ){
   
    autoclean <- auto_data_cleaning(
      data = price_df[,-1], 
      S = c(36, 5*36),
      no.of.last.indices.to.fix = nrow(price_df),
      model.missing.pars = list(consider.as.missing = 0, 
                                min.val = 0)
    )
    
    # autoclean$replaced.indices
    tic("Run ")
    system.time(
    model.miss <- tsrobprep::model_missing_data(
      data = price_xts, 
      S = c(36, 5*36),
      # tau = 0.5,
      no.of.last.indices.to.fix = nrow(price_df), 
      # ndices.to.fix = seq_len(nrow(price_df)),
      consider.as.missing = 0,
      min.val = 0
    )
    ) # system.time
    toc()
   
    model.miss$estimated.models
    model.miss$replaced.indices
    
    # Impute missing values
    data.imputed <- impute_modelled_data(model.miss)
    
    #Detect outliers
    system.time(
      o.ident <- detect_outliers(data = data.imputed, S = c(36, 5*36))
    )
    
    # Plot of identified outliers in time series
    outlier.vector <- rep(F,length(data.imputed))
    
    outlier.vector[o.ident$outlier.pos] <- T
    
    
    plot(price_df, type = "o", col = 1,
         pch = 1, cex = 0.3, main = "Akhaber")
    
    plot(data.imputed, type = "o", col = 1 + 1 * outlier.vector,
         pch = 1 + 18 * outlier.vector, cex= 0.3, main = "Akhaber")
    

    df1 <- data.frame(o.ident$outlier.pos.raw,unlist(o.ident$outlier.probs)[o.ident$outlier.pos.raw])
    
    colnames(df1) <- c("Outlier position", "Probability of being outlying data")
    
    View(df1)
    
    # Plot of feature matrix
    plot.ts(o.ident$features, type = "o",
            col = 1 + outlier.vector,
            pch = 1 + 1 * outlier.vector,
            cex = 0.3, main = "Akhaber")
    

    # Detect outliers with feat.int = T
    system.time(
      o.ident <- detect_outliers(data = data.imputed, S = c(36, 5*36), feat.inf = T)
    )
    
    feature.imp <- unlist(lapply(o.ident$inf.feature.combinations,
                                 function(x) paste(o.ident$feature.inf.tab[x], collapse = " | ")))
    
    df2 <- data.frame(o.ident$outlier.pos.raw,o.ident$outlier.probs[o.ident$outlier.pos.raw],
                     feature.imp[as.numeric(names(feature.imp)) %in% o.ident$outlier.pos.raw])
    colnames(df2) <- c("Outlier position", "Probability being outlying data", "Responsible features")
    
    View(df2)
    
  }
  
  # -------------------------------------------------------------------------- #
  ret_xts_all <- PerformanceAnalytics::Return.calculate(prices = price_xts_all,
                      method = tolower(ret_method))
  
  base::suppressPackageStartupMessages({ 
    base::suppressWarnings({
      ret_df_all = data.frame(date = zoo::index(zoo(ret_xts_all)), coredata(ret_xts_all))
    })
  })
  
  # -------------------------------------------------------------------------- #
  cat(" Done. \n")
  return( list(price_xts_all = price_xts_all, price_df_all = price_df_all, ret_xts_all = ret_xts_all, ret_df_all = ret_df_all))
} # Imputation_MissingValues_Func


# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


Funda_Charts_func <- function(dfL = Porfolio_Tbl, 
                              charttype = "ReturnBeta"){
  
  
  # -------------------------------------------------------------------------- #
  # ReturnBeta
  # -------------------------------------------------------------------------- #
  # dfL <- Porfolio_Tbl
  dff <<- dfL$df_Funda
  if(charttype == "ReturnBeta" && ("Beta" %in% colnames(dff)) &&
     ("ReturnOnAssetsTTM" %in% colnames(dff)) ){
    
    df <- dff[, c("Symbol", "MarketCapitalization", "ReturnOnAssetsTTM", "Beta")]
   
    df <- df %>% mutate_at(c("MarketCapitalization", 
                             "ReturnOnAssetsTTM", "Beta"), as.numeric)
    
    gplt <- ggplot(df, show_guide = F,
                    aes(x = Beta, y = ReturnOnAssetsTTM , colour = Symbol)) +
               #geom_abline(intercept = rf.rate, slope = slope, size = 1.25, colour='black') +
               geom_point(aes(size = MarketCapitalization )) +
               geom_text(show.legend = FALSE, label = df$Symbol, vjust = 0, 
                         nudge_y = (max(df$ReturnOnAssetsTTM) - min(df$ReturnOnAssetsTTM)) / 100) +
               xlab('Beta') + ylab('ReturnOnAssetsTTM') + ggtitle('Market Line') +
               theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
     
    return(list(gplt = gplt, df = df))
    
  } else{ return(NULL) } # charttype == "ReturnBeta"
  # -------------------------------------------------------------------------- #
  
} # Funda_Charts_func


# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.



#' HTML Screener widget tool from TradingView.com for Shiny App 
#' @param Shinyinputs c(input, output, session)
#' @return Screener
#' @export

Screener_Dash_Func <- function(input, output, session){

  # Exchange <- "USA Indices"
 
  Exchange <- input$Mrkt_Screner_Exchange_Pikr 
  
  # cat(Exchange)
  
  if(Exchange %in% c("USA", "Germany", "UK")){
    
    ExchangeLink <- '<div class="tradingview-widget-copyright"><a href="https://www.tradingview.com/screener/" rel="noopener" target="_blank"><span class="blue-text"> </span></a> </div>
  <script type="text/javascript" src="https://s3.tradingview.com/external-embedding/embed-widget-screener.js" async>'
  
    # market = "america"
    # 
    # if(Exchange == "America"){market = "america"}
    # if(Exchange == "Germany"){market = "germany"}
    # if(Exchange == "UK"){market = "uk"}
    Screen = "most_capitalized"
    
   } else{
      
   ExchangeLink <- '<div class="tradingview-widget-copyright"><a href="https://www.tradingview.com/forex-screener/" rel="noopener" target="_blank"><span class="blue-text"> </span></a> </div>
   <script type="text/javascript" src="https://s3.tradingview.com/external-embedding/embed-widget-screener.js" async>'
    
   # if(Exchange == "Forex"){market = "forex"}
   # if(Exchange == "Crypto"){market = "crypto"}
   Screen = "general"
 }
 
  # ------------------------
  HTMLjs_Script <- paste0('
  <html>
  <!-- TradingView Widget BEGIN -->
  <div class="tradingview-widget-container">
  <div class="tradingview-widget-container__widget"></div> 
  ', ExchangeLink, '
  {
  "width": "100%",
  "height": 900,
  "defaultColumn": "overview",
  "defaultScreen": "', Screen, '",
  "market": "', tolower(Exchange), '" ,
  "showToolbar": true,
  "colorTheme": "light",
  "locale": "en"
  }
  
  </script>
  </div>
  <!-- TradingView Widget END --> 
  
  </html> ', sep = "")
    
  
  return(htmlTemplate( text_ = HTMLjs_Script, document_ = TRUE))
    

} # Screener_Dash_Func
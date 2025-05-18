
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.

#' HTML Stocks widget tool from TradingView.com for Shiny App 
#' @param Shinyinputs c(input, output, session)
#' @return HTML widgets
#' @export
#' @examples
#'
#'
Stocks_Dash_Func <- function(input, output, session, type = ""){
  # Exchange <- "US"
  
  # ------------------------------------------------------------------------ # 
  # UI
  # ------------------------------------------------------------------------ # 
  
  
  # ------------------------------------------------------------------------ # 
  # Server
  # ------------------------------------------------------------------------ # 
  
  if(type == "Top5"){
  

  return(htmlTemplate( text_ = HTMLjs_Script_top5, document_ = TRUE))
  } # Top5


# ------------------------------------------------------------------------ # 
# # MarketNews 
# ------------------------------------------------------------------------ #
 if(type == "MarketNews"){
   

  return(htmlTemplate( text_ = HTMLjs_Script_MarketNews, document_ = TRUE))
 } # MarketNews

  # ------------------------------------------------------------------------ # 
  # # Symbol Info Widget
  # ------------------------------------------------------------------------ #
  
  if(type == "SymbolInfo"){
    

  return(htmlTemplate( text_ = HTMLjs_Script_SymbolInfo, document_ = TRUE))
  } # SymbolInfo
  

# ------------------------------------------------------------------------ # 
# # ChartOnly Widget
# ------------------------------------------------------------------------ #

if(type == "ChartOnly"){
  
  return(htmlTemplate( text_ = HTMLjs_Script_ChartOnly, document_ = TRUE))
}


# ------------------------------------------------------------------------ # 
# # Fundamental Data Widget
# ------------------------------------------------------------------------ #

if(type == "Fundamental"){
  
  return(htmlTemplate( text_ = HTMLjs_Script_Fundamental, document_ = TRUE))
}

# ------------------------------------------------------------------------ # 
# # SymbolNews Widget
# ------------------------------------------------------------------------ #

if(type == "SymbolNews"){
  
  return(htmlTemplate( text_ = HTMLjs_Script_SymbolNews, document_ = TRUE))
}


# ------------------------------------------------------------------------ # 
# # Company Profile Widget
# ------------------------------------------------------------------------ #

if(type == "CompanyProfile"){
  
  return(htmlTemplate( text_ = HTMLjs_Script_CompanyProfile, document_ = TRUE))
}

} # Stocks_Dash_Func
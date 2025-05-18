

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


Func_GetData <- function(symbol = NULL, from = "2021-01-01"){

  df_symbol <- tryCatch({
    quantmod::getSymbols(as.character( symbol ), 
                         src = 'yahoo', 
                         from = from, 
                         auto.assign = FALSE)
  },
  error = function(e) {NULL})
  
  if(!is.null(df_symbol) && nrow(df_symbol) >= 1){
    return(df_symbol)
  } else{ return(NULL)} # if
  
} # Func_GetData
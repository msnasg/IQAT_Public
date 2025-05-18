

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.


PairFinder_Excel_Smry_Func <- function(df){

  cln = colnames(df)
  
  symbs = cln[!grepl("date", cln, ignore.case = T) &
          !grepl("time", cln, ignore.case = T)]
  
  Smry <- data.frame(Symbol = symbs, Missing_Date = unname(colSums(is.na(df[,-1]))), 
                     Missing_Rate = round(100 * unname(colSums(is.na(df[,-1]))) / nrow(df), 2), 
                     Min = apply(df[,-1], 2, function(x) {min(x[!is.na(x)])}), 
                     Max = apply(df[,-1], 2, function(x) {max(x[!is.na(x)])}),
                     Mean = round(apply(df[,-1], 2, function(x) {mean(x[!is.na(x)])}), 2),
                     Sd = round(apply(df[,-1], 2, function(x) {sd(x[!is.na(x)])}),2),
                     Start =  unname(apply(df[,-1], 2, function(x){ as.character(df[min(which(!is.na(x))) ,1]) })), 
                     End =  unname(apply(df[,-1], 2, function(x){ as.character(df[max(which(!is.na(x))) ,1]) }))
                     )
  

  return(Smry)
  
  
  } # PairFinder_Excel_Smry_Func

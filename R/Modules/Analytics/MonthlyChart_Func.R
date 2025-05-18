
# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.





MonthlyChart_Func <- function(x = price_xts, type = "log"){
  

  cat("MonthlyChart_Func ...")
  # x[which(grepl("2012-03-24", x$date)  )]
  
  y = xts(x)
  
  if(type == "log"){
    Cl <- log(y[ ,1])
  }else{
    Cl <- y[ ,1]
  }
  
  
  dat <- as.data.frame(Cl)
  base::suppressWarnings({
    date <- zoo::index(zoo(Cl))
  })
  
  AllMonths <- split(Cl, f = "months")
  df2 <- data.frame()
  
  for (ii in 1:length(AllMonths)){
    tmp <- AllMonths[[ii]]
    n <- nrow(tmp)
    df2 <- rbind.data.frame(df2, tmp[n,])
  }
  
  df2 <- data.frame(Date = rownames(df2), df2)
  
  df2$Date <- as.Date(df2$Date)
  
  res <- reshape2::dcast(transform(df2, 
                                   month = format(Date, format="%m"),
                                   year = format(Date, "%Y")),
                         year ~ month, 
                         value.var = colnames(y)[1])
  
  rownames(res) <- res$year
  res <- res[,-1]
  names(res) <- toupper(month.abb[as.numeric(names(res))])
  
  
  replace_na_with_last <- function(x,a=!is.na(x)){
    x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
  }
  
  dfmelt <- melt(as.data.table(res, keep.rownames = "Vars"), id.vars = "Vars")
  
  # print(type)
  
  cat(" Done. \n")
  return(dfmelt)
} # MonthlyChart_Func

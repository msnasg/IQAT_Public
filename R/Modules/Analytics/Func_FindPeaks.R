
# FIND Peaks and Valleys                             
# Last Update: 17 August 2019

########################################## Function ####################################### 

Func_FindPeaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(!is.na(all(x[c(z : i, (i + 2) : w)] <= x[i + 1]))) {return(i + 1)} else {return(numeric(0))}
  })
  pks <- unlist(pks)
  pks
}

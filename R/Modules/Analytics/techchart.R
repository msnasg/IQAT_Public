

# --- Partial Code for Demonstration Purposes Only ---
# This code is a simplified or incomplete version, shared solely to illustrate the project structure and layout.
# Core functionality and sensitive logic have been omitted intentionally.
# For inquiries or collaboration, please contact the developer.





# ---------------------------------------------------------------------------- #
# https://github.com/prodipta/techchart/blob/master/R/RcppExports.R
# ---------------------------------------------------------------------------- #
cpt_trend_R <- function(x, y, Q, minseglen, penalty) {
  # .Call('techchart_cpt_trend', PACKAGE = 'techchart', x, y, Q, minseglen, penalty)
  cpt_trend(x, y, Q, minseglen, penalty)
}

houghtransform_R <- function(x1, y1, flag, rbucket, abucket, s) {
  # .Call('techchart_houghtransform', PACKAGE = 'techchart', x1, y1, flag, rbucket, abucket, s)
  houghtransform(x1, y1, flag, rbucket, abucket, s)
}

timesTwo_R <- function(x) {
  # .Call('techchart_timesTwo', PACKAGE = 'techchart', x)
  timesTwo(x)
}

findminima_R <- function(xmin, xmax, threshold) {
  # .Call('techchart_findminima', PACKAGE = 'techchart', xmin, xmax, threshold)
  # .Call('techchart_findminima', xmin, xmax, threshold)
  findminima(xmin, xmax, threshold)
  #.Call('techchart_findminima', xmin, xmax, threshold)
}

findmaxima_R <- function(xmin, xmax, threshold) {
  # .Call('techchart_findmaxima', PACKAGE = 'techchart', xmin, xmax, threshold)
  findmaxima(xmin, xmax, threshold)
}

sortoptimaposition_R <- function(pos, sign, value) {
  # .Call('techchart_sortoptimaposition', PACKAGE = 'techchart', pos, sign, value)
  sortoptimaposition(pos, sign, value)
}

sortoptimasign_R <- function(pos, sign, value) {
  #.Call('techchart_sortoptimasign', PACKAGE = 'techchart', pos, sign, value)
  sortoptimasign(pos, sign, value)
}

checkoptimasign_R <- function(sign) {
  # .Call('techchart_checkoptimasign', PACKAGE = 'techchart', sign)
  checkoptimasign(sign)
}

checkoptimapos_R <- function(pos) {
  #.Call('techchart_checkoptimapos', PACKAGE = 'techchart', pos)
  checkoptimapos(pos)
}

# ---------------------------------------------------------------------------- #
# cpt trend functions
#'@importFrom Rcpp evalCpp
#'@useDynLib techchart

#' @export
print.cpttrend <- function(x,...){
  print(x$cpts, ...)
}

#' @export
summary.cpttrend <- function(object, ...){
  x <- object
  n <- NROW(x$cpts)+1
  seg.len <- rep(0,n)
  seg.return <- rep(0,n)
  seg.offset <- rep(0,n)
  for(i in 1:n){
    seg.len[i] <- NROW(x$segments[[i]])
    seg.return[i] <- annualize(x$segments[[i]])
    if(i>1){
      start <- as.numeric(x$segments[[i]][1])
      end <- as.numeric(x$segments[[i-1]][NROW(x$segments[[i-1]])])
      seg.offset[i] <- start/end -1
    }
  }
  y <- list(cpts=x$cpts, seg.len=seg.len, seg.return=seg.return,
            seg.offset=seg.offset)
  class(y) <- "summary.cpttrend"
  return(y)
}

#' @export
print.summary.cpttrend <- function(x,...){
  cat("change points:\n")
  print(x$cpts)
  cat("segments length summary:\n")
  print(summary(x$seg.len))
  cat("segments returns summary:\n")
  cat(x$seg.return); cat("\n")
  cat("segments offset summary:\n")
  cat(x$seg.offset)
}

#' @export
plot.cpttrend <- function(x,...){
  n <- NROW(x$cpts)+1
  if(xts::is.xts(x$data)){
    if(quantmod::is.OHLC(x$data)){
      plot(quantmod::Cl(x$data), main = "trend change points", xlab="x", ylab="y")
    }else{
      plot(x$data[,1], main = "trend change points", xlab="x", ylab="y")
    }
    for(i in 1:n){
      lines(xts::as.xts(x$segments[[i]]), ...)
    }
  } else{
    plot(x$data, type = "n", main="trend change points", xlab="x", ylab="y")
    lines(x$data)
    for(i in 1:n){
      lines(x$segments[[i]], ...)
    }
  }
  Sys.sleep(0)
}

#' change point analysis using binary segmentation
#' @param x xts, dataframe, matrix or vector, representing a time series
#' @param Q Maximum number of change points required
#' @param minseglen Minimum length of a trend segment
#' @param penalty Penalty value, increasing it reduces number of segments
#' @return change points data (object of class cpttrends)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2015/"]
#' cpts <- cpt.trend(x,50,minseglen = 20, penalty = 5)
#' summary(cpts)
#' quantmod::chart_Series(x)
#' quantmod::add_TA(cpts$segments[[NROW(cpts$segments)]],on=1,lty=3)
#' @seealso \code{\link[changepoint]{cpt.mean}}
#' @export
cpt.trend <- function(x, Q=10, minseglen=10, penalty=1){
  
  if.xts <- F
  if(xts::is.xts(x)){
    if(quantmod::is.OHLC(x)){
      y <- data.frame(x=seq(1:NROW(x)),y=as.numeric(zoo::coredata(quantmod::Cl(x))))
    } else{
      y <- data.frame(x=seq(1:NROW(x)),y=as.numeric(zoo::coredata(x[,1])))
    }
    
    if.xts <- T
  } else if(NCOL(x)==1){
    y <- data.frame(x=seq(1:NROW(x)), y=as.numeric(x))
  } else{
    y <- data.frame(x=as.numeric(x[,1]), y=as.numeric(x[,2]))
  }
  
  z <- cpt_trend_R(y$x, y$y, Q, minseglen, penalty)
  z <- z[z!=0]
  
  if(NROW(z)<1){
    warning("No change point found, try relaxing parameters")
    cpts <- list()
    cpts$data <- x
    cpts$cpts <- NULL
    cpts$segments <- NULL
    class(cpts) <- "cpttrend"
    return(cpts)
  }
  if(NROW(z)==Q){
    warning("number of change points found same as requested,
            consider increasing the value of Q or penalty")
  }
  
  z <- sort(z)
  n <- NROW(z)+1
  segments <- list()
  
  for(i in 1:n){
    a <- ifelse(i==1,1,z[i-1]+1)
    b <- ifelse(i==n,NROW(y),z[i])
    m <- stats::lm(y$y[a:b]~y$x[a:b])
    fit <- predict(m)
    if(if.xts){
      segments[[i]] <- xts::as.xts(fit,zoo::index(zoo(x))[a:b])
    } else{
      segments[[i]] <- data.frame(x=y$x[a:b],y=fit)
    }
  }
  
  cpts <- list()
  cpts$data <- x
  cpts$cpts <- z
  cpts$segments <- segments
  class(cpts) <- "cpttrend"
  return(cpts)
}

# ---------------------------------------------------------------------------- #
# https://github.com/prodipta/techchart/blob/master/R/findlines.R
# ---------------------------------------------------------------------------- #
# cpt trend functions
#'@importFrom Rcpp evalCpp
#'@importFrom stats approx cutree dist hclust na.omit predict sd
#'@useDynLib techchart

range01 <- function(x){
  zeroshift <- min(x)
  x <- x - min(x)
  x <- x/max(x)
  return(x)
}
cluster_grouping <-function(d,cluster,tolerance, FUN="max", is.rounded=TRUE){
  n <- NROW(d);
  all_idx <- (1:n)
  groups <- rep(0,n)
  for(i in 1:n){
    if(NROW(which(!is.na(all_idx)))<1)break
    cut <- stats::cutree(cluster,i)
    for(j in 1:i){
      idx <- which(cut==j)
      idx <- idx[which(idx %in% all_idx)]
      x <- d[idx]
      if(NROW(x)<1)next
      if(NROW(x)==1){
        groups[idx] <- ifelse(is.rounded,round(x),x)
        all_idx[idx] <- NA
        next
      }
      if(max(x)-min(x)<tolerance){
        y <- do.call(FUN,list(x))
        groups[idx] <- ifelse(is.rounded,round(y),y)
        all_idx[idx] <- NA
      }
    }
  }
  return(groups)
}
merge_lines <- function(xlines, tolerance=2){
  
  #return if only one member
  if(NROW(xlines)<2)return(xlines)
  # cluster along start and end lines
  d <- na.omit(unique(xlines$end));
  # if less than two, no clustering
  if(NROW(d)<2)return(xlines)
  
  # run clutering
  cluster <- hclust(dist(d))
  d1 <- cluster_grouping(d,cluster,2*tolerance, FUN="max",FALSE)
  endtag <- d1[match(xlines$end,d)]
  d <- unique(xlines$start)
  if(NROW(d)<2)return(xlines)
  cluster <- hclust(dist(d))
  d1 <- cluster_grouping(d,cluster,2*tolerance, FUN="min",FALSE)
  starttag <- d1[match(xlines$start,d)]
  
  #merge same start and end points based on score
  xlines$key <- paste(starttag,"-",endtag)
  keys <- unique(xlines$key)
  lapply(1:NROW(keys), FUN=function(i){
    cut <- xlines[which(xlines$key == keys[i]),]
    if(NROW(cut)>1){
      idx <- which(cut$score==min(cut$score))
      if(NROW(idx)>1)idx <- which(cut$fit==max(cut$fit))
      x <- cut[idx,]
      xlines$r[which(xlines$key==keys[i])] <<- 0
      xlines <<- rbind(xlines,x)
    }
  })
  xlines$key <- NULL
  xlines <- xlines[xlines$r != 0,]
  
  return(xlines)
}
filter_lines <- function(xlines, imppts, flag=1, ptheta=0.25, pscore=0.3,
                         pfit=0.95, nsize=10, nlines=10){
  
  # nothing to do if no lines found
  if(NROW(xlines)<1)return(xlines)
  
  # compute the slope...
  #if(flag==1){
  #xlines$xtheta <- (180/pi)*atan((as.numeric(imppts$maxima$value[xlines$end]) - as.numeric(imppts$maxima$value[xlines$start]))
  #                      /(as.numeric(imppts$maxima$pos[xlines$end]) - as.numeric(imppts$maxima$pos[xlines$start])))
  #} else{
  #xlines$xtheta <- (180/pi)*atan((as.numeric(imppts$minima$value[xlines$end]) - as.numeric(imppts$minima$value[xlines$start]))
  #                      /(as.numeric(imppts$minima$pos[xlines$end]) - as.numeric(imppts$minima$pos[xlines$start])))
  #}
  xlines$xtheta <- xlines$theta - 90
  # ... and eliminate near-vertical lines
  if(!is.na(ptheta)){
    xlines <- xlines[abs(cos(xlines$xtheta*pi/180))>ptheta,]
  }
  
  # return if too few lines left
  if(NROW(xlines)<1)return(xlines)
  if(NROW(xlines)<nlines){
    xlines <- xlines[with(xlines, order(-end,score,-fit)),]
    return(xlines)
  }
  
  # check if envelop score is acceptable
  if(!is.na(pscore)){
    cutoffscore <- as.numeric(stats::quantile(unique(xlines$score), pscore))
    tmplines <- xlines[xlines$score < pscore,]
    if(NROW(tmplines)<1)return(xlines)
    xlines <- tmplines
  }
  # return if too few lines left
  if(NROW(xlines)<1)return(xlines)
  if(NROW(xlines)<nlines){
    xlines <- xlines[with(xlines, order(-end,score,-fit)),]
    return(xlines)
  }
  
  # check if fit score is acceptable
  if(!is.na(pfit)){
    tmplines <- xlines[xlines$fit > pfit,]
    if(NROW(tmplines)<1)return(xlines)
    xlines <- tmplines
  }
  # return if too few lines left
  if(NROW(xlines)<1)return(xlines)
  if(NROW(xlines)<nlines){
    xlines <- xlines[with(xlines, order(-end,score,-fit)),]
    return(xlines)
  }
  
  # finally filter based on size
  if(!is.na(nsize)){
    s1 <- as.numeric(imppts$maxima$pos)[xlines$end]
    s2 <- as.numeric(imppts$maxima$pos)[xlines$start]
    if(flag==-1){
      s1 <- as.numeric(imppts$minima$pos)[xlines$end]
      s2 <- as.numeric(imppts$minima$pos)[xlines$start]
    }
    tmplines <- xlines
    tmplines$size <- s1 - s2
    tmplines <- tmplines[tmplines$size > nsize,]
    if(NROW(tmplines)<1)return(xlines)
    xlines <- tmplines
    xlines$size <- NULL
  }
  # return if too few lines left
  if(NROW(xlines)<1)return(xlines)
  if(NROW(xlines)<nlines){
    xlines <- xlines[with(xlines, order(-end,score,-fit)),]
    return(xlines)
  }
  
  # enough lines left, sort, cut and return
  xlines <- xlines[with(xlines, order(-end,score,-fit)),]
  xlines <- xlines[1:nlines,]
  
  return(xlines)
}
hough_lines <- function(x, flag, r.tol=0.02, theta.tol=1, s=2){
  
  rbucket <- seq(-1.42,1.42,r.tol); abucket <- seq(1,180,theta.tol)
  xlines <- houghtransform_R(range01(x$x),range01(x$y),flag,rbucket, abucket,s)
  xlines$r <- rbucket[xlines$r+1]; xlines$theta <- abucket[xlines$theta+1]
  xlines <- xlines[xlines$r != 0,]
  xlines$start <- xlines$start + 1
  xlines$end <- xlines$end + 1
  xlines <- xlines[which(xlines$end != xlines$start),]
  return(xlines)
}


#' Find enveloping lines of a given time series
#' @param x xts object representing a time series
#' @param tolerance tolerance specification for important points
#' @param nlines max number of lines to return
#' @param pscore envelope score, default value is NA (no filtering done)
#' @param pfit fit socre, default value is NA (no filtering done)
#' @param r.tol r (polar) coordinate tolerance in unit square
#' @param theta.tol theta coordinate tolerance in degrees
#' @param s minimum number less than the # of colinear points on the line
#' @param merge.tol tolerance for merging lines with similar start or end points
#' @seealso \code{\link[techchart]{find.tchannel}}
#' @export
find_lines <- function(x, tolerance=2, nlines=10, pfit=NA, pscore=NA,
                       r.tol=0.02, theta.tol=2, s=2, merge.tol=2){
  if.xts <- F
  if(xts::is.xts(x)){
    if.xts <- T
  }
  
  # find the important points and switch to 1 unit square map
  imppts <- find.imppoints(x,tolerance)
  xmin <- imppts$minima; xmax <- imppts$maxima
  xmin <- data.frame(x=as.numeric(zoo::coredata(xmin$pos)),y=as.numeric(zoo::coredata(xmin$value)))
  xmax <- data.frame(x=as.numeric(zoo::coredata(xmax$pos)),y=as.numeric(zoo::coredata(xmax$value)))
  
  # start with min points, find lines and merge and filter
  minlines <- hough_lines(xmin,-1,r.tol,theta.tol,s)
  if(NROW(minlines)>1){
    minlines <- merge_lines(minlines,merge.tol)
  }
  if(NROW(minlines)>1){
    minlines <- merge_lines(minlines,merge.tol)
  }
  if(NROW(minlines)>1){
    minlines <- filter_lines(minlines,imppts, -1,nlines=nlines, pfit = pfit, pscore = pscore)
  }
  
  # now same with max points
  maxlines <- hough_lines(xmax,1,r.tol,theta.tol,s)
  if(NROW(maxlines)>1){
    maxlines <- merge_lines(maxlines,merge.tol)
  }
  if(NROW(maxlines)>1){
    maxlines <- merge_lines(maxlines,merge.tol)
  }
  if(NROW(maxlines)>1){
    maxlines <- filter_lines(maxlines,imppts, 1,nlines=nlines, pfit = pfit, pscore = pscore)
  }
  
  # build the return object
  xlines <- list()
  xlines$imppts <- imppts
  xlines$maxlist <- maxlines
  xlines$minlist <- minlines
  
  # compute the lines for plot for minima points...
  minlist <- list()
  
  if(NROW(minlines)>0){
    for(i in 1:NROW(minlines)){
      r <- minlines$r[i]
      theta <- minlines$theta[i]
      start <- minlines$start[i]; end <- minlines$end[i]
      s2 <- as.numeric(imppts$minima$pos)[minlines$end[i]]
      s1 <- as.numeric(imppts$minima$pos)[minlines$start[i]]
      y2 <- as.numeric(quantmod::Lo(x)[s2]);y1 <- as.numeric(quantmod::Lo(x)[s1])
      idx1 <- zoo::index(zoo(x))[s1:s2]
      idx <- zoo::index(zoo(x))[s1:NROW(x)]
      z <- Hmisc::approxExtrap(c(1,NROW(idx1)),c(y1,y2),seq(1:NROW(idx)),n=NROW(idx))
      lastval <- as.numeric(z$y[NROW(idx)]); lastx <- as.numeric(quantmod::Cl(x)[NROW(x)])
      maxx <- max(as.numeric(quantmod::Cl(x))); minx <- min(as.numeric(quantmod::Cl(x)))
      b_out_range <- lastval > maxx | lastval < minx
      b_no_extrapolate <- lastval < 0.9*lastx | lastval > 1.1*lastx
      if(b_out_range & b_no_extrapolate){
        z <- approx(c(1,NROW(idx1)),c(y1,y2),seq(1:NROW(idx)),n=NROW(idx))
      }
      minlist[[i]] <- na.omit(xts::as.xts(z$y,idx))
    }
  }
  
  # and for maxima points...
  maxlist <- list()
  if(NROW(maxlines)>0){
    for(i in 1:NROW(maxlines)){
      r <- maxlines$r[i]
      theta <- maxlines$theta[i]
      start <- maxlines$start[i]; end <- maxlines$end[i]
      s2 <- as.numeric(imppts$maxima$pos)[maxlines$end[i]]
      s1 <- as.numeric(imppts$maxima$pos)[maxlines$start[i]]
      y2 <- as.numeric(quantmod::Hi(x)[s2]);y1 <- as.numeric(quantmod::Hi(x)[s1]);
      idx1 <- zoo::index(zoo(x))[s1:s2]
      idx <- zoo::index(zoo(x))[s1:NROW(x)]
      z <- Hmisc::approxExtrap(c(1,NROW(idx1)),c(y1,y2),seq(1:NROW(idx)),n=NROW(idx))
      lastval <- as.numeric(z$y[NROW(idx)]); lastx <- as.numeric(quantmod::Cl(x)[NROW(x)])
      maxx <- max(as.numeric(quantmod::Cl(x))); minx <- min(as.numeric(quantmod::Cl(x)))
      b_out_range <- lastval > maxx | lastval < minx
      b_no_extrapolate <- lastval < 0.9*lastx | lastval > 1.1*lastx
      if(b_out_range & b_no_extrapolate){
        z <- approx(c(1,NROW(idx1)),c(y1,y2),seq(1:NROW(idx)),n=NROW(idx))
      }
      maxlist[[i]] <- na.omit(xts::as.xts(z$y,idx))
    }
  }
  
  xlines$maxlines <- maxlist
  xlines$minlines <- minlist
  
  # we are done
  return(xlines)
}

find.lines <- function(x, tolerance=1.5, n=3, pscore=(0.05)^2, pfit=0.85,
                       force.one=!return.all, return.all=FALSE){
  
  last.idx <- zoo::index(zoo(x))[NROW(x)]
  
  # get the lines
  xlines <- find_lines(x,tolerance,n)
  if(return.all)return(xlines)
  
  # find the best fit max line
  if(NROW(xlines$maxlist)>0){
    nn <- NROW(xlines$maxlist)
    last.pos <- max(xlines$maxlist$end)
    scorecard <- data.frame(pos=rep(0,nn), score=rep(0,nn), valid=rep(0,nn),
                            mid=rep(0,nn))
    for(i in 1:nn){
      idx <- NROW(xlines$maxlines[[i]])
      b_ending <- zoo::index(zoo(xlines$maxlines[[i]]))[idx] == last.idx
      b_pos <- b_ending & abs(xlines$maxlist$end[i] - max(xlines$maxlist$end)) < 3
      b_score <- xlines$maxlist$score[i] == min(xlines$maxlist$score) |
        xlines$maxlist$score[i] < pscore
      b_fit <- xlines$maxlist$fit[i] == min(xlines$maxlist$fit) |
        xlines$maxlist$fit[i] > pfit
      scorecard$pos[i] <- b_pos
      scorecard$score[i] <- b_score
      scorecard$valid[i] <- b_pos & b_score & b_fit
      scorecard$mid[i] <- !b_ending & b_score
    }
    idx <- which(scorecard$valid==TRUE)
    if(NROW(idx)< 1 & force.one){
      idx <- which(scorecard$pos==TRUE & scorecard$score==TRUE)
      if(NROW(idx)< 1){
        idx <- which(scorecard$pos==TRUE)
      }
    }
    if(NROW(idx)>1){
      idx <- which(xlines$maxlist$score==min(xlines$maxlist$score))
      if(NROW(idx)>1){
        idx <- which(xlines$maxlist[idx,]$strength
                     ==max(xlines$maxlist[idx,]$strength))
      }
      
      if(NROW(idx)>1)idx <- idx[1]
    }
    idx2 <- which(scorecard$mid==TRUE)
    idx <- c(idx,idx2)
    if(NROW(idx)>0){
      xlines$maxlist <- xlines$maxlist[idx,]
      for(i in 1:nn){
        if(!(i%in%idx))xlines$maxlines[[i]] <- NULL
      }
    } else{
      xlines$maxlist <- xlines$maxlist[0,]
      xlines$maxlines <- list()
    }
  }
  
  # now repeat for minima
  if(NROW(xlines$minlist)>0){
    nn <- NROW(xlines$minlist)
    last.pos <- max(xlines$minlist$end)
    scorecard <- data.frame(pos=rep(0,nn), score=rep(0,nn), valid=rep(0,nn),
                            mid=rep(0,nn))
    for(i in 1:nn){
      idx <- NROW(xlines$minlines[[i]])
      b_ending <- zoo::index(zoo(xlines$minlines[[i]]))[idx] == last.idx
      b_pos <- b_ending & abs(xlines$minlist$end[i] - max(xlines$minlist$end)) < 2
      b_score <- xlines$minlist$score[i] == min(xlines$minlist$score) |
        xlines$minlist$score[i] < pscore
      b_fit <- xlines$minlist$fit[i] == min(xlines$minlist$fit) |
        xlines$minlist$fit[i] > pfit
      scorecard$pos[i] <- b_pos
      scorecard$score[i] <- b_score
      scorecard$valid[i] <- b_pos & b_score & b_fit
      scorecard$mid[i] <- !b_ending & b_score
    }
    idx <- which(scorecard$valid==TRUE)
    if(NROW(idx)<1 & force.one){
      idx <- which(scorecard$pos==TRUE & scorecard$score==TRUE)
      if(NROW(idx)< 1){
        idx <- which(scorecard$pos==TRUE)
      }
    }
    if(NROW(idx)>1){
      idx <- which(xlines$minlist$score==min(xlines$minlist$score))
      if(NROW(idx)>1){
        idx <- which(xlines$minlist[idx,]$strength==
                       max(xlines$minlist[idx,]$strength))
      }
      
      if(NROW(idx)>1)idx <- idx[1]
    }
    idx2 <- which(scorecard$mid==TRUE)
    idx <- c(idx,idx2)
    if(NROW(idx)>0){
      xlines$minlist <- xlines$minlist[idx,]
      for(i in 1:nn){
        if(!(i%in%idx))xlines$minlines[[i]] <- NULL
      }
    } else{
      xlines$minlist <- xlines$minlist[0,]
      xlines$minlines <- list()
    }
  }
  
  #done, return the lines
  return(xlines)
  
}

#' Find most current enveloping lines of a given time series
#' @param x xts object representing a time series
#' @param tolerance tolerance specification for important points, expressed as times the standard deviation
#' @param n number of lines to to choose the best ones from
#' @param pscore envelope score, defines how best the lines envelopes the time series
#' @param pfit fit socre, defines how much the lines deviates from the extreme points it connects
#' @return returns the trend channel object (of type class tchannel)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2016-01-01::2016-09-30"]
#' quantmod::chart_Series(x)
#' tchannel <- find.tchannel(x,1.1)
#' tchannel
#' quantmod::add_TA(tchannel$xlines$maxlines[[1]],on=1)
#' quantmod::add_TA(tchannel$xlines$minlines[[1]],on=1)
#' @seealso \code{\link[techchart]{find_lines}}
#' @export
find.tchannel <- function(x, tolerance=1.5, n=3, pscore=(0.05)^2,
                          pfit=0.85){
  
  if(!xts::is.xts(x))stop("expected xts object")
  if(!quantmod::is.OHLC(x)){
    x <- merge(x[,1],x[,1],x[,1],x[,1])
    colnames(x) <- c("open","high","low","close")
  }
  
  try(xlines <- find.lines(x, tolerance, pscore, pfit, force.one = TRUE))
  tchannel <- list()
  tchannel$xlines <- NA
  tchannel$name <- NA
  tchannel$dir <- NA
  tchannel$upperlimit <- NA
  tchannel$lowerlimit <- NA
  tchannel$duration <- NA
  tchannel$midlinemove <- NA
  tchannel$maxlinemove <-NA
  tchannel$minlinemove <- NA
  tchannel$length <- NA
  tchannel$aspectratio <- NA
  tchannel$score <- NA
  tchannel$strength <- NA
  tchannel$fit <- NA
  class(tchannel) <- "tchannel"
  
  if(NROW(xlines$maxlist)<1 | NROW(xlines$minlist)<1){
    warning("no envelopes found, try changing the tolerance, increasing n or pscore or reducing pfit")
    return(tchannel)
  }
  
  if(NROW(xlines$maxlines[[1]]) <2 |NROW(xlines$minlines[[1]]) <2 ){
    warning("no envelopes found, try changing the tolerance, increasing n or pscore or reducing pfit")
    return(tchannel)
  }
  
  last.maxday <- zoo::index(zoo(xlines$maxlines[[1]]))[NROW(xlines$maxlines[[1]])]
  last.minday <- zoo::index(zoo(xlines$minlines[[1]]))[NROW(xlines$minlines[[1]])]
  last.day <- zoo::index(zoo(x))[NROW(x)]
  if(last.maxday != last.minday | last.maxday != last.day){
    warning("no envelopes found, try changing the tolerance, increasing n or pscore or reducing pfit")
    return(tchannel)
  }
  
  maxx <- as.numeric(xlines$maxlines[[1]][NROW(xlines$maxlines[[1]])])
  minx <- as.numeric(xlines$minlines[[1]][NROW(xlines$minlines[[1]])])
  if(maxx < minx)return(tchannel)
  
  idx <- max(zoo::index(zoo(xlines$maxlines[[1]]))[1],zoo::index(zoo(xlines$minlines[[1]]))[1])
  max0 <- as.numeric(xlines$maxlines[[1]][idx])
  min0 <- as.numeric(xlines$minlines[[1]][idx])
  if(max0 < min0)return(tchannel)
  
  startdev <- 100*(max0/min0-1)
  enddev <- 100*(maxx/minx-1)
  vol <- sd(na.omit(TTR::ROC(quantmod::Cl(x)))); tol <- 0.25*vol
  duration <- min(NROW(xlines$maxlines[[1]]),NROW(xlines$minlines[[1]]))
  duration <- duration/(NROW(x))
  
  #find the channel type and direction
  if(startdev > enddev & abs(startdev-enddev)>(100*tol)){
    tchannel$name <- "triangle"
  } else if(startdev < enddev & abs(startdev-enddev)>(100*tol)){
    tchannel$name <- "megaphone"
  } else{
    tchannel$name <- "channel"
  }
  
  startmean <- 0.5*(min0+max0)
  endmean <- 0.5*(minx+maxx)
  
  if(startmean < endmean & abs(endmean/startmean-1)>tol ){
    tchannel$dir <- 1
  } else if(startmean > endmean & abs(endmean/startmean-1)>tol){
    tchannel$dir <- -1
  } else{
    tchannel$dir <- 0
  }
  
  # add the lines data
  tchannel$xlines <- xlines
  tchannel$midlinemove <- endmean/startmean
  tchannel$minlinemove <- minx/min0
  tchannel$maxlinemove <- maxx/max0
  
  #find the limiting points
  tchannel$upperlimit <- maxx
  tchannel$lowerlimit <- minx
  tchannel$duration <- duration
  
  #aesthetic parameters
  tchannel$length <- max(NROW(xlines$maxlines[[1]])/NROW(xlines$minlines[[1]]),
                         NROW(xlines$minlines[[1]])/NROW(xlines$maxlines[[1]]))
  tchannel$aspectratio <- max((maxx-minx)/(max0-min0),(max0-min0)/(maxx-minx))
  tchannel$score <- max(tchannel$xlines$maxlist$score[1],tchannel$xlines$minlist$score[1])
  tchannel$strength <- min(tchannel$xlines$maxlist$strength[1],tchannel$xlines$minlist$strength[1])
  tchannel$fit <- min(tchannel$xlines$maxlist$fit[1],tchannel$xlines$minlist$fit[1])
  
  return(tchannel)
}

#'@export
print.tchannel <- function(x,...){
  cat(paste("name:",x$name)); cat("\n")
  #cat(paste("type:",x$type)); cat("\n")
  cat(paste("direction:",x$dir)); cat("\n")
  cat(paste("upper limit:",round(x$upperlimit,3))); cat("\n")
  cat(paste("lower limit:",round(x$lowerlimit,3))); cat("\n")
  cat(paste("duration ratio:",round(x$duration,3))); cat("\n")
  cat(paste("aesthetics - aspect ratio:",round(x$aspectratio,2),"score:",round(x$score,2),
            "fit:",round(x$fit,2), "strength:", x$strength))
}

#'@export
summary.tchannel <- function(object, ...){
  x <- object
  x$xlines <- NULL
  return(x)
}

#'@export
print.summary.tchannel <- function(x,...){
  cat(paste("name:",x$name)); cat("\n")
  #cat(paste("type:",x$type)); cat("\n")
  cat(paste("direction:",x$dir)); cat("\n")
  cat(paste("upper limit:",round(x$upperlimit,3))); cat("\n")
  cat(paste("lower limit:",round(x$lowerlimit,3))); cat("\n")
  cat(paste("duration ratio:",round(x$duration,3))); cat("\n")
  cat(paste("aesthetics - aspect ratio:",round(x$aspectratio,2),"score:",round(x$score,2),
            "fit:",round(x$fit,2), "strength:", x$strength))
}


# ---------------------------------------------------------------------------- #
# https://github.com/prodipta/techchart/blob/master/R/helpers.R
# ---------------------------------------------------------------------------- #
annualize <- function(x){
  if(!xts::is.xts(x) | NROW(x)<2){
    stop("input is not in xts format or has less than 2 points")
  }
  period <- as.double(zoo::index(zoo(x))[NROW(x)] - zoo::index(zoo(x))[1])
  years <- period/365.25
  r <- as.numeric(x[NROW(x),1])/as.numeric(x[1,1])
  r <- (r)^(1/years) - 1
  return(r)
}


#' Find the top level trends in a time series
#' @param x xts, dataframe, matrix or vector, representing a time series
#' @param penalty starting penalty value, should be a BIG number (default 20)
#' @return change points data (object of class cpttrends)
#' @seealso \code{\link[techchart]{cpt.trend}}
#' @export
find.major.trends <- function(x,penalty=20){
  n <- penalty + 1
  cpts <- list(); class(cpts) <- "cpttrend"
  for(i in 1:n){
    tryCatch({
      cpts <- cpt.trend(x,50,20,n-i)
      if(NROW(cpts$cpts > 0)) break
    }, error=function(cond){
      message(cond)
      break
    }, warning=function(cod){
      # do nothing
    })
    
  }
  return(cpts)
}

#' Find the last best-fit enveloping lines of a given time series
#' @param x xts object representing a time series
#' @param tscore envelope score threshold
#' @return returns the trend channel object (of type class tchannel)
#' @seealso \code{\link[techchart]{find.tchannel}}
#' @export
find.trend.channel <- function(x, tscore=(0.25)^2){
  tolerance <- seq(1.1,1.5,0.05)
  error_threshold <- tscore
  score <- 999
  
  # set up the return object
  tchannel <- list()
  tchannel$xlines <- NA
  tchannel$name <- NA
  tchannel$dir <- NA
  tchannel$upperlimit <- NA
  tchannel$lowerlimit <- NA
  tchannel$duration <- NA
  tchannel$midlinemove <- NA
  tchannel$maxlinemove <-NA
  tchannel$minlinemove <- NA
  tchannel$aspectratio <- NA
  tchannel$score <- NA
  tchannel$strength <- NA
  tchannel$fit <- NA
  class(tchannel) <- "tchannel"
  na_tchannel <- tchannel
  
  for(tol in tolerance){
    tryCatch({
      new_tchannel <- find.tchannel(x,tol)
      if(!is.na(new_tchannel$name)){
        if(new_tchannel$score >= score){
          if(score < error_threshold)return(tchannel)
          next
        }
        tchannel <- new_tchannel
        score <- tchannel$score
      }
    }, error=function(cond){
      # do nothing
    }, warning=function(w){
      # do nothing
    })
  }
  
  if(!is.na(tchannel$score)){
    if(tchannel$score < (1.5*error_threshold))return(tchannel)
  }
  return(na_tchannel)
}



#' Find current and/ or forming technical patter
#' @param x xts, a time series in xts format
#' @param pattern a list returned from a call to pattern.db()
#' @param tolerance a list tolerance used for pattern matching definition (vol multiple)
#' @param pip.tolerance a list tolerance used for searching important points (vol multiple)
#' @return a list of pattern matches (object of type tpattern)
#' @seealso \code{\link[techchart]{pattern.db}}
#' @seealso \code{\link[techchart]{find.tpattern}}
#' @export
find.pattern <- function(x, pattern=pattern.db("HS")[[1]], tolerance=c(0.25,1),
                         pip.tolerance=c(1.1,1.5)){
  
  m <- NROW(pip.tolerance); n <- NROW(tolerance)
  
  for(i in 1:m){
    for(j in 1:n){
      tryCatch({
        # check if we have a pattern at the end
        tpattern <- find.tpattern(x, pattern,tolerance[j],pip.tolerance[i], find.all = FALSE)
        
        if(NROW(tpattern$matches) > 0) {return(tpattern)}
        
        #if not, check if one is forming
        last.extrema <- as.numeric(tpattern$imppts$results$sign[NROW(tpattern$imppts$results)])
        move <- tpattern$vol*pip.tolerance[i]
        x1 <- as.numeric(quantmod::Cl(x)[NROW(x)])*(1+last.extrema*move)
        x1 <- data.frame(t(rep(x1,NCOL(x)))); colnames(x1) <- colnames(x)
        x1 <- xts::as.xts(x1,zoo::index(zoo(x))[NROW(x)]+1)
        y <- rbind(x,x1);
        
        tryCatch({
          tpattern <- find.tpattern(y, pattern,tolerance[j],pip.tolerance[i], find.all = FALSE)
          if(NROW(tpattern$matches)>0){
            tpattern$matches[[1]]$type = "forming"
            return(tpattern)
          }
        }, error=function(e){}
        )
      }, error=function(e){}
      )
    }
  }
  
  # no pattern found
  return(NULL)
}

# ---------------------------------------------------------------------------- #
# https://github.com/prodipta/techchart/blob/master/R/patterns.R
# ---------------------------------------------------------------------------- #
# pattern search and find
#'@importFrom Rcpp evalCpp
#'@importFrom grDevices adjustcolor
#'@useDynLib techchart

#' @title Database of pattern definitions
#' @description definition of standard technical patterns
#' @param type string, any of "HS", "IHS", "BTOP", "BBOT" or "all"
#' @return a list if pattern definition
#' @export
pattern.db <- function(type="all"){
  patterns = list()
  
  #Head and Shoulder
  name = "Head and shoulder"
  call.expr <- parse(text="
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     (R[1] > R[2]) & (R[5] > R[4]) & (R[3] > R[1]) & (R[3] > R[5]) &
                     abs(R[1] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
                     abs(R[5] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
                     abs(R[2] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     abs(R[4] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2
                     ")
  # pattern$formula = expression(
  #   # E3 > E1, E3 > E5
  #   E3 > E1 &
  #     E3 > E5 &
  #     
  #     # E1 and E5 are within 1.5 percent of their average
  #     abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
  #     abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
  #     
  #     # E2 and E4 are within 1.5 percent of their average
  #     abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
  #     abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
  # )
  draw = expression({
    lines(c(T1,T2,T3,T4,T5), c(E1,E2,E3,E4,E5), lwd = 3, col="col")
    text(T3, E3, 'HS', adj = c(0.5,-0.5), xpd = TRUE)		
  })
  length <- 5
  start <- 1 # "max"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("red",alpha.f = 0.4),lwd=5)
  patterns$HS <- list(name=name, call=call.expr, length=length, start=start,
                      threshold=threshold, draw = draw, plot=plot, 
                      aspect.1=aspect.1, aspect.2=aspect.2)
  
  # Inverse Head and Shoulder
  name = "Inverse head and shoulder"
  call.expr <- parse(text="
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     (R[1] < R[2]) & (R[5] < R[4]) & (R[3] < R[1]) & (R[3] < R[5]) &
                     abs(R[1] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
                     abs(R[5] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
                     abs(R[2] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     abs(R[4] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2
                     ")
  # pattern$formula = expression(
  #   # E3 < E1, E3 < E5
  #   E3 < E1 &
  #     E3 < E5 &
  #     
  #     # E1 and E5 are within 1.5 percent of their average
  #     abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
  #     abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
  #     
  #     # E2 and E4 are within 1.5 percent of their average
  #     abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
  #     abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
  # )
  draw = expression({
     lines(c(T1,T2,T3,T4,T5), c(E1,E2,E3,E4,E5), lwd=3, col="blue")
    text(T3, E3, 'IHS', adj=c(0.5,1), xpd=TRUE)		
   })
  length <- 5
  start <- -1 # "min"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("blue",alpha.f = 0.4),lwd=5)
  patterns$IHS <- list(name=name, call=call.expr, length=length, start=start,
                       threshold=threshold, draw = draw, plot=plot, 
                       aspect.1=aspect.1, aspect.2=aspect.2)
  
  # Broadening Top (BTOP)
  name = "Broadening top"
  call.expr <- parse(text="
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     R[1] < R[3] & R[3] < R[5] & R[2] > R[4]")
  # pattern$formula = expression(
  #   # E1 < E3 < E5
  #   E1 < E3 &
  #     E3 < E5 &		
  #     E2 > E4
  # )
  # pattern$plot = expression({
  #   beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
  #   lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=3, col=col)	
  #   lines(c(d2,d4), c(E2,E4), lwd=3, col=col)
  #   text(d3, min(E2,E4), 'BTOP', adj=c(0.5,1), xpd=TRUE)
  # })
  length <- 5
  start <- 1 # "max"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("orange",alpha.f = 0.4),lwd=5)
  patterns$BTOP <- list(name=name, call=call.expr, length=length, start=start,
                        threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)
  
  # Broadening Bottom (BBOT)
  name = "Broadening bottom"
  call.expr <- parse(text="
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     R[1] > R[3] & R[3] > R[5] & R[2] < R[4]")
  # pattern$formula = expression(
  #   # E1 > E3 > E5
  #   E1 > E3 &
  #     E3 > E5 &		
  #     E2 < E4
  # )		
  # pattern$plot = expression({
  #   beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
  #   lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=3, col=col)	
  #   lines(c(d2,d4), c(E2,E4), lwd=3, col=col)
  #   text(d3, max(E2,E4), 'BBOT', adj=c(0.5,0), xpd=TRUE)
  # })
  length <- 5
  start <- -1 # "min"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("brown",alpha.f = 0.4),lwd=5)
  patterns$BBOT <- list(name=name, call=call.expr, length=length, start=start,
                        threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)
  
 
  # Triangle tops (TTOP)
  name = "Triangle tops"
  call.expr <- parse(text=" 
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     R[1] > R[3] & R[3] > R[5] & R[2] < R[4] ")
  # pattern$formula = expression(
  #   # E1 > E3 > E5
  #   E1 > E3 &
  #     E3 > E5 &		
  #     E2 < E4
  # )
  # pattern$plot = expression({
  #   beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
  #   lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=3, col=col)
  #   lines(c(d2,d4), c(E2,E4), lwd=3, col=col)
  #   text(d3, min(E2,E4), 'TTOP', adj=c(0.5,1), xpd=TRUE)
  # })
  length <- 5
  start <- 1 # "max"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("green",alpha.f = 0.4),lwd=5)				
  patterns$TTOP = list(name=name, call=call.expr, length=length, start=start,
                       threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)	
  
  
  # Triangle bottoms (TBOT)
  name = "Triangle bottoms"
  call.expr <- parse(text=" 
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     R[1] < R[3] & R[3] < R[5] & R[2] > R[4] ")
  # pattern$formula = expression(
  #   # E1 < E3 < E5
  #   E1 < E3 &
  #     E3 < E5 &		
  #     E2 > E4
  # )
  # pattern$plot = expression({
  #   beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients		
  #   lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=3, col=col)
  #   lines(c(d2,d4), c(E2,E4), lwd=3, col=col)
  #   text(d3, max(E2,E4), 'TBOT', adj=c(0.5,0), xpd=TRUE)
  # })
  length <- 5
  start <- -1 # "min"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("yellow",alpha.f = 0.4),lwd=5)				
  patterns$TBOT = list(name=name, call=call.expr, length=length, start=start,
                       threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)
  
  
  # Rectangle tops (RTOP)
  name = "Rectangle tops"
  call.expr <- parse(text=" 
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     abs(R[1] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     abs(R[3] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     abs(R[5] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     abs(R[2] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     abs(R[4] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     (min(R[1],R[3],R[5]) > max(R[2],R[4]))")
  # pattern$formula = expression({
  #   avg.top = (E1+E3+E5)/3
  #   avg.bop = (E2+E4)/2
  #   
  #   # tops E1,E3,E5 are within 0.75 percent of their average
  #   abs(E1 - avg.top) < 0.75/100 * avg.top &
  #     abs(E3 - avg.top) < 0.75/100 * avg.top &
  #     abs(E5 - avg.top) < 0.75/100 * avg.top &
  #     
  #     # bottoms E2,E4 are within 0.75 percent of their average
  #     abs(E2 - avg.bop) < 0.75/100 * avg.bop &
  #     abs(E4 - avg.bop) < 0.75/100 * avg.bop &
  #     
  #     # lowest top > highest bottom
  #     min(E1,E3,E5) > max(E2,E4)
  # })
  # pattern$plot = expression({
  #   avg.top = (E1+E3+E5)/3
  #   avg.bop = (E2+E4)/2
  #   
  #   lines(c(d1,d3,d5), rep(avg.top,3), lwd=3, col=col)
  #   lines(c(d2,d4), rep(avg.bop,2), lwd=3, col=col)
  #   text(d3, min(E2,E4), 'RTOP', adj=c(0.5,-0.5), xpd=TRUE)
  # })
  length <- 5
  start <- 1 # "max"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color=adjustcolor("darkgreen",alpha.f = 0.4),lwd=5)				
  patterns$RTOP = list(name=name, call=call.expr, length=length, start=start,
                       threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)
  
  
  # Rectangle bottoms (RBOT)
  name = "Rectangle bottoms"
  call.expr <- parse(text=" 
                     fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5));
                     R <- fit$residuals; R <- R+E1;
                     if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)};
                     abs(R[2] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     abs(R[4] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
                     abs(R[1] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     abs(R[3] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     abs(R[5] - (R[1]+R[3]+R[5])/3) < tolerance * (R[1]+R[3]+R[5])/3 &
                     (min(R[2],R[4]) > max(R[1],R[3],R[5]))")
  
  # patterns$formula = expression({
  #   avg.top = (E2+E4)/2
  #   avg.bop = (E1+E3+E5)/3
  #   
  #   # tops E2,E4 are within 0.75 percent of their average
  #   abs(E2 - avg.top) < 0.75/100 * avg.top &
  #     abs(E4 - avg.top) < 0.75/100 * avg.top &
  #     
  #     # bottoms E1,E3,E5 are within 0.75 percent of their average		
  #     abs(E1 - avg.bop) < 0.75/100 * avg.bop &
  #     abs(E3 - avg.bop) < 0.75/100 * avg.bop &
  #     abs(E5 - avg.bop) < 0.75/100 * avg.bop &
  #     
  #     # lowest top > highest bottom
  #     min(E2,E4) > max(E1,E3,E5)
  # })
  length <- 5
  start <- -1 # "min"
  threshold <- quote((R[2]+R[4])/2)
  aspect.1 <- quote((R[5]-R[4])/(R[1]-R[2]))
  aspect.2 <- quote((R[5]-R[4])/(R[3]-R[2]))
  plot <- list(color = adjustcolor("darkgreen",alpha.f = 0.4), lwd=5)				
  patterns$RBOT = list(name=name, call=call.expr, length=length, start=start,
                       threshold=threshold, plot=plot, aspect.1=aspect.1, aspect.2=aspect.2)
  
  
  
  # Double tops (DTOP), note in E and t first one is excluded
  
  # Double bottoms (DBOT)
  
  
  
  #done, now check and return required patterns
  ret.patterns <- list()
  index <-1
  if(type[1] != "all"){
    for(i in 1:NROW(patterns)){
      if(names(patterns)[i] %in% type){
        ret.patterns[[index]] <- patterns[[i]]
        index <- index + 1
      }
    }
  } else {
    ret.patterns <- patterns
  }
  
  return(ret.patterns)
}

#' Find matching patterns in a time series
#' @param x xts, a time series in xts format
#' @param pattern a list returned from a call to pattern.db()
#' @param tolerance tolerance used for pattern matching definition (vol multiple)
#' @param pip.tolerance tolerance used for searching important points (vol multiple)
#' @param find.all boolean, whether to return all matches or the latest one
#' @param trend.adjusted boolean, whether to adjust for trends during matching
#' @return a list of pattern matches (object of type tpattern)
#' @seealso \code{\link[techchart]{pattern.db}}
#' @seealso \code{\link[techchart]{find.pattern}}
#' @export
find.tpattern <- function(x, pattern=pattern.db("HS")[[1]], tolerance=0.25,
                          pip.tolerance=2, find.all= TRUE, trend.adjusted=FALSE){
  # find.tpattern(x,pattern,tolerance = tolerance[j], pip.tolerance = pip.tolerance[i], find.all = FALSE)
 
   if(!xts::is.xts(x)){
    stop("require an xts time series object")
  }
  if(NROW(pattern)>1 && NROW(lengths(pattern))==1){
    warning("more than one patterns specified, first will be used")
    pattern <- pattern[[1]]
  }
  
  if(quantmod::is.OHLC(x)){
    ret <- na.omit(TTR::ROC(quantmod::Cl(x)))
  } else{
    ret <- na.omit(TTR::ROC(x[,1]))
  }
  vol <- sd(ret)
  tolerance <- vol*tolerance
  
  idx <- zoo::index(zoo(x))
  
  imppts <- find.imppoints(x, pip.tolerance)
 
  if(NROW(imppts$results)<pattern$length){
    stop("not enough points, time series too short")
  }
  z <- imppts$results
  
  k <- min(which(z$sign == pattern$start))
  
  if(!find.all){
    pattern.end <- ((-1)^(pattern$length-1))*pattern$start
    k <- max(which(z$sign==pattern.end)) - (pattern$length - 1)
  }
  l <- NROW(z)-pattern$length+1
  kl <- seq(k,l,2)
  
  pattern.env <- new.env(parent = globalenv())
  
  tpattern <- list(); tpattern$imppts <- imppts;
  tpattern$vol <- vol; class(tpattern) <- "tpattern"
  matches <- list(); n <- 0
  
  # loop through the important points
  for(k in kl){
    #assign optimas to pattern environment
    for(j in 1:pattern$length){
      assign(paste("E",j,sep=""),as.numeric(z$value[k+j-1]) ) #, envir=pattern.env)
      
      assign(paste("T",j,sep=""),as.numeric(z$pos[k+j-1]) ) # ,envir=pattern.env)
    }
    # assign control variables
    curren.point <- ifelse(quantmod::is.OHLC(x),quantmod::Cl(x)[NROW(x)],x[NROW(x),1])
    assign("tolerance",tolerance)# ,envir=pattern.env)
    assign("tchart.trend.adjusted",trend.adjusted)# ,envir=pattern.env)
    assign("current.point", as.numeric(curren.point))# ,envir = pattern.env)
    
    # E1 = pattern.env$E1; E2 = pattern.env$E2; E3 = pattern.env$E3; E4 = pattern.env$E4; E5 = pattern.env$E5
    # T1 = pattern.env$T1; T2 = pattern.env$T2; T3 = pattern.env$T3; T4 = pattern.env$T4; T5 = pattern.env$T5
    # current.point = pattern.env$current.point
    # fit = pattern.env$fit 
    # R = pattern.env$R
    # tchart.trend.adjusted = pattern.env$tchart.trend.adjusted
    # tolerance = pattern.env$tolerance
    
    # eval(expression(fit <- lm(c(E1,E2,E3,E4,E5)~c(T1,T2,T3,T4,T5)), R <- fit$residuals, 
    #            R <- R+E1, if(!tchart.trend.adjusted){R <- c(E1,E2,E3,E4,E5)}, 
    #            (R[1] > R[2]) & (R[5] > R[4]) & (R[3] > R[1]) & (R[3] > R[5]) &
    #              abs(R[1] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
    #              abs(R[5] - (R[1]+R[5])/2) < tolerance * (R[1]+R[5])/2 &
    #              abs(R[2] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2 &
    #              abs(R[4] - (R[2]+R[4])/2) < tolerance * (R[2]+R[4])/2))
    
    eval(pattern$call)
    
    #tryCatch({
     # if(eval(pattern$call, pattern.env)){
        x.in.idx <- idx[z$pos[k:(k+pattern$length-1)]]
        x.in <- z$pos[k:(k+pattern$length-1)]
        y.in <- z$value[k:(k+pattern$length-1)]
        x.out <- z$pos[k]:z$pos[k+pattern$length-1]
        x.out.idx <- idx[z$pos[k]:z$pos[k+pattern$length-1]]
        y.out <- approx(x.in,y.in,x.out, n=NROW(x.out))
        patterns.out <- xts::as.xts(y.out$y, x.out.idx)
        colnames(patterns.out) <- c("pattern")
        points.out <- xts::as.xts(y.in, x.in.idx)
        colnames(points.out) <- c("points")
        xdate <- zoo::index(zoo(patterns.out))[NROW(patterns.out)]
        threshold <- eval(pattern$threshold) #, pattern.env)
        aspect.1 <- eval(pattern$aspect.1) # , pattern.env)
        aspect.2 <- eval(pattern$aspect.2 ) #, pattern.env)
        name <- pattern$name
        move <- annualize(patterns.out)
        duration <- as.double(zoo::index(zoo(patterns.out))[NROW(patterns.out)]
                              - zoo::index(zoo(patterns.out))[1])
        type = "complete"
        n <- n+1; 
        matches[[n]] <- list(data=patterns.out,points=points.out,
                                       name=name, move=move, duration=duration,
                                       threshold=threshold, aspect.1=aspect.1,
                                       aspect.2=aspect.2, date=xdate, type=type)
     #}
    #}, error=function(e){
      #cat(paste("loop:",k,"\n"))
      #print(e)
      #print(zoo::index(x)[NROW(x)])
      #print(tail(z))
      #print(c(pattern.env$E1,pattern.env$E2,pattern.env$E3,pattern.env$E4))
    #}
    #)
    #k <- min(which(z$sign[(k+1):NROW(z)]==pattern$start)) + k
  }
  
  tpattern$matches <- matches
  return(tpattern)
}

#'@export
print.tpattern <- function(x,...){
  n <- NROW(x$matches)
  for(i in 1:n){
    y <- x$matches[[i]]
    cat(paste("------pattern matched on:",y$date,"--------")); cat("\n")
    cat(paste("name:",y$name)); cat("\n")
    cat(paste("type:",y$type)); cat("\n")
    cat(paste("move:",round(100*y$move,2)),"(percentage annualized)"); cat("\n")
    cat(paste("threshold:",round(y$threshold,2))); cat("\n")
    cat(paste("duration:",round(y$duration,2)),"(days)"); cat("\n")
  }
}

#'@export
summary.tpattern <- function(object, ...){
  x <- object
  return(x)
}

#'@export
print.summary.tpattern <- function(x,...){
  n <- NROW(x$matches)
  for(i in 1:n){
    y <- x$matches[[i]]
    cat(paste("------pattern matched on:",y$date,"--------")); cat("\n")
    cat(paste("name:",y$name)); cat("\n")
    cat(paste("type:",y$type)); cat("\n")
    cat(paste("move:",round(100*y$move,2)),"(percentage annualized)"); cat("\n")
    cat(paste("threshold:",round(y$threshold,2))); cat("\n")
    cat(paste("duration:",round(y$duration,2)),"(days)"); cat("\n")
  }
}


# ---------------------------------------------------------------------------- #
# https://github.com/prodipta/techchart/blob/master/R/supports.R
# ---------------------------------------------------------------------------- #
# support resistance and important points functions
#'@importFrom Rcpp evalCpp
#'@importFrom graphics lines plot points
#'@useDynLib techchart
#'
#'@export
print.imppoints <- function(x,...){
  print(x$results, ...)
}
#'@export
summary.imppoints <- function(object, ...){
  x <- object
  maxima <- x$maxima
  minima <- x$minima
  ret <- list(maxima=maxima, minima=minima)
  class(ret) <- "summary.imppoints"
  return(ret)
}
#' @export
print.summary.imppoints <- function(x,...){
  cat("extrement points:\n")
  print(paste("maxima:", NROW(x$maxima),"minima:",NROW(x$minima)))
  cat("Highs summary:\n")
  print(summary(x$maxima$value))
  cat("Lows summary:\n")
  print(summary(x$minima$value))
}
#'@export
plot.imppoints <- function(x, maxcol="green", mincol="red", ...){
  if(xts::is.xts(x$data)){
    if(quantmod::is.OHLC(x$data)){
      plot(quantmod::Cl(x$data), ...)
    }else{
      plot(x$data[,1], ...)
    }
    max.xy <- xts::as.xts(x$maxima[,2],zoo::index(x$data)[x$maxima[,1]])
    min.xy <- xts::as.xts(x$minima[,2],zoo::index(x$data)[x$minima[,1]])
    points(max.xy, col="black", pch = 24, bg=maxcol)
    points(min.xy, col="black", pch = 25, bg=mincol)
  } else{
    plot(x$data, type ="n", ...)
    lines(x$data)
    points(x$maxima[,1],x$maxima[,2], col="black", pch = 24, bg=maxcol)
    points(x$minima[,1],x$minima[,2], col="black", pch = 25, bg=mincol)
  }
  Sys.sleep(0)
}
#'@export
`[.imppoints` <- function(x,condition){
  if(!xts::is.xts(x$data))stop("subsetting not allowed on non-xts input")
  imppts <- list()
  imppts$data <- x$data[condition,]
  imppts$results <- x$results[condition,]
  imppts$maxima <- x$maxima[condition,]
  imppts$minima <- x$minima[condition,]
  class(imppts) <- "imppoints"
  return(imppts)
}

find.minima <- function(x, tolerance, lookback=20){
  n <- NROW(x)
  y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
  i.min <- i.y <- 1
  
  if(tolerance > 1 & quantmod::is.OHLC(x)){
    threshold <- TTR::ATR(quantmod::HLC(x),n=lookback)$atr/quantmod::Cl(x)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance > 1 & !(quantmod::is.OHLC(x))){
    threshold <- zoo::rollapply(TTR::ROC(x[,1]),lookback, sd)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance < 1){
    threshold <- rep(tolerance,NROW(x))
  } else{
    stop("tolerance is not valid for data")
  }
  
  threshold <- (1+threshold)
  if(quantmod::is.OHLC(x)){
    x.min <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,min)
    x.max <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,max)
  } else{
    x.min <- x.max <- as.matrix(x)[,1]
  }
  
  y <- data.frame(findminima_R(as.numeric(x.min),as.numeric(x.max),threshold))
  y <- y[which(y[,2]!=0),]
  
  colnames(y) <- c("pos","sign")
  y$pos <- y$pos+1
  y$value <- as.numeric(x.min)[y$pos]
  return(y)
}
find.maxima <- function(x, tolerance, lookback=20){
  n <- NROW(x)
  y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
  i.min <- i.y <- 1
  
  if(tolerance > 1 & quantmod::is.OHLC(x)){
    threshold <- TTR::ATR(quantmod::HLC(x),n=lookback)$atr/quantmod::Cl(x)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance > 1 & !(quantmod::is.OHLC(x))){
    threshold <- zoo::rollapply(TTR::ROC(x[,1]),lookback, sd)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance < 1){
    threshold <- rep(tolerance,NROW(x))
  } else{
    stop("tolerance is not valid for data. Variable tolerance allowed only for xts input")
  }
  
  threshold <- (1+threshold)
  if(quantmod::is.OHLC(x)){
    x.min <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,min)
    x.max <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,max)
  } else{
    x.min <- x.max <- as.matrix(x)[,1]
  }
  
  y <- data.frame(findmaxima_R(as.numeric(x.min),as.numeric(x.max),threshold))
  y <- y[which(y[,2]!=0),]
  colnames(y) <- c("pos","sign")
  y$pos <- y$pos+1
  y$value <- as.numeric(x.max)[y$pos]
  return(y)
}

#' time series extrema using important point algorithm
#' @param x xts object or vector, representing a time series
#' @param tolerance threshold for percentage change or vol multiple for extreme points
#' @param lookback Used for volatility dependent adaptive threshold
#' @return important points data object (object of class imppoints)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2015/"]
#' imppts <- find.imppoints(x,2)
#' quantmod::chart_Series(x)
#' points(as.numeric(imppts$maxima$pos),as.numeric(imppts$maxima$value),bg="green",pch=24,cex=1.25)
#' points(as.numeric(imppts$minima$pos),as.numeric(imppts$minima$value),bg="red",pch=25,cex=1.25)
#' @export
find.imppoints <- function(x, tolerance=0.02, lookback=20){
  z1 <- find.minima(x,tolerance, lookback)
  z2 <- find.maxima(x, tolerance, lookback)
  z <- rbind(z1,z2)
  z <- z[order(z$pos),]
  
  for(i in 1:5){
    if(!checkoptimapos_R(as.numeric(z$pos))){
      sign_x <- sortoptimaposition_R(as.numeric(z$pos),as.numeric(z$sign),
                                   as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign!=0),]
    }
    if(!checkoptimasign_R(as.numeric(z$sign))){
      sign_x <- sortoptimasign_R(as.numeric(z$pos),as.numeric(z$sign),
                               as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign!=0),]
    }
  }
  rownames(z) <- seq(1:NROW(z))
  pts <- list()
  pts$data <- x
  
  if(xts::is.xts(x)){
    z <- xts::as.xts(z,zoo::index(zoo(x))[z$pos])
    data <- data.frame(pos=z$pos[which(z$sign==1)],value=z$value[which(z$sign==1)])
    maxima <- xts::as.xts(data,zoo::index(zoo(x))[z$pos[which(z$sign==1)]])
    data <- data.frame(pos=z$pos[which(z$sign==-1)],value=z$value[which(z$sign==-1)])
    minima <- xts::as.xts(data,zoo::index(zoo(x))[z$pos[which(z$sign==-1)]])
  } else{
    maxima <- data.frame(pos=z$pos[which(z$sign==1)],value=z$value[which(z$sign==1)])
    minima <- data.frame(pos=z$pos[which(z$sign==-1)],value=z$value[which(z$sign==-1)])
  }
  pts$results <- z
  pts$maxima <- maxima
  pts$minima <- minima
  class(pts) <- "imppoints"
  return(pts)
}

#'@export
print.supports <- function(x,...){
  print(x$results, ...)
}
#'@export
summary.supports <- function(object, ...){
  x <- object
  resist<- x$results$value[which(x$results$value > x$lastpoint)]
  sups<- x$results$value[which(x$results$value < x$lastpoint)]
  resist <- sort(resist)
  sups <- rev(sort(sups))
  ret <- list(supports=sups, resistance=resist)
  class(ret) <- "summary.supports"
  return(ret)
}
#' @export
print.summary.supports <- function(x, n=3, ...){
  cat("supports and resistance:\n")
  n.s <- NROW(x$supports); n.r <- NROW(x$resistance)
  if(n.s <1){
    cat("no supports at curret levels")
  } else{
    cat(paste("next",n,"supports:"))
    if(n.s >n){
      cat(x$supports[1:n])
    } else{
      cat(x$supports)
    }
  }
  cat("\n")
  if(n.r <1){
    cat("no resistance at curret levels")
  } else{
    cat(paste("next",n,"resistance:"))
    if(n.r >n){
      cat(x$resistance[1:n])
    } else{
      cat(x$resistance)
    }
  }
}
#'@export
plot.supports <- function(x, ...){
  n <- NROW(x$results)
  if(xts::is.xts(x$data)){
    if(quantmod::is.OHLC(x$data)){
      plot(quantmod::Cl(x$data), main = "supports and resistance", xlab="x", ylab="y")
    }else{
      plot(x$data[,1], main = "supports and resistance", xlab="x", ylab="y")
    }
    for(i in 1:n){
      lines(xts::as.xts(x$lines[[i]]), ...)
    }
  } else{
    plot(x$data, type = "n", main="supports and resistance", xlab="x", ylab="y")
    lines(x$data)
    for(i in 1:n){
      lines(x$lines[[i]], ...)
    }
  }
  Sys.sleep(0)
}

merge_levels <- function(levels, clusters, tolerance=0.01, strength=3){
  n <- length(levels)
  supports <- data.frame(rep(0,n),rep(0,n))
  sups <- levels
  k <- 1
  for(i in 1:n){
    cut <- cutree(clusters,i)
    for(j in 1:i){
      s <- sups[which(cut==j)]
      s <- na.omit(s)
      if(length(s)<1){
        next
      }
      error <- (max(s) - min(s))/mean(s)
      if(error < tolerance){
        supports[k,1] <- mean(s)
        supports[k,2] <- length(s)
        sups[which(cut==j)] <- NA
        k <- k+1
      }
    }
  }
  for(i in 1:n){
    if(!is.na(sups[i])){
      supports[k,1] <- sups[i]
      supports[k,2] <- 1
      k <- k+1
    }
  }
  supports <- supports[supports[,2] >= strength,]
  colnames(supports) <- c("value","strength")
  return(supports)
}

find.supports <- function(x, tolerance=0.02, strength=3, maxline=10,lookback=20){
  optima <- find.imppoints(x, tolerance = tolerance, lookback = lookback)
  clusters <- stats::hclust(dist(optima$results$value))
  sups <- merge_levels(optima$results$value,clusters,tolerance,strength)
  if(NROW(sups)<1)stop("no supports found, try reducing strength parameter")
  
  if(xts::is.xts(x)){
    if(quantmod::is.OHLC(x)){
      lastval <-as.numeric(quantmod::Cl(x)[NROW(x)])
    }else{
      lastval <-as.numeric(x[NROW(x),1])
    }
  }else{
    lastval <- as.matrix(x)[NROW(x),1]
  }
  
  sups$dist <- abs(sups$value - lastval)
  if(NROW(sups) > maxline){
    sups <- sups[order(sups$dist),]
    sups <- sups[1:maxline,]
  }
  sups <- sups[,1:2]
  rownames(sups) <- seq(1:NROW(sups))
  
  suplines <- list()
  for(i in 1:NROW(sups)){
    if(xts::is.xts(x)){
      suplines[[i]] <- xts::as.xts(rep(sups$value[i],NROW(x)),zoo::index(zoo(x)))
    } else{
      suplines[[i]] <- data.frame(x=zoo::index(zoo(x)), y=rep(sups$value[i],NROW(x)))
    }
  }
  
  supports <- list()
  supports$lastpoint <- lastval
  supports$data <- x
  supports$results <- sups
  supports$lines <- suplines
  class(supports) <- "supports"
  
  return(supports)
}

#' Find supports and resitance for a time series
#' @param x xts object, or vector, representing a time series
#' @param type either FIB (Fibonacci) or SR. SR is based on best fit lines of multiple peaks and troughs
#' @param tolerance threshold for percentage change or vol multiple for extreme points
#' @param strength minimum number of extreme points defining a support
#' @param maxline maximum number of support/ resistance lines to return
#' @param lookback Used for volatility dependent adaptive threshold
#' @return support/ resistance object (object of class supports)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2015/"]
#' sups <- find.pivots(x, type = "FIB")
#' summary(sups)
#' sups <- find.pivots(x, type = "SR", strength = 5)
#' summary(sups)
#' quantmod::chart_Series(x)
#' quantmod::add_TA(sups$lines[[1]],on=1, lty=2)
#' @export
find.pivots <- function(x, type=c("SR","FIB"), tolerance=0.02, strength=3, maxline=10, lookback=20){
  if(type=="SR"){
    return(find.supports(x,tolerance,strength,maxline))
  }
  
  lastval <- 0
  if(xts::is.xts(x)){
    if(quantmod::is.OHLC(x)){
      lastval <-as.numeric(quantmod::Cl(x)[NROW(x)])
    }else{
      lastval <-as.numeric(x[NROW(x),1])
    }
  }else{
    lastval <- as.matrix(x)[NROW(x),1]
  }
  
  imppts <- find.imppoints(x,tolerance)
  xmax <- max(lastval,imppts$results$value)
  xmin <- min(lastval,imppts$results$value)
  xrange <- xmax - xmin
  levels <- rep(0,6)
  
  levels[1]<-xmin; levels[2]<-levels[1]+0.236*xrange; levels[3]<-levels[1]+0.382*xrange;
  levels[4]<-levels[1]+0.5*xrange; levels[5]<-levels[1]+0.618*xrange; levels[6]<-xmax;
  
  sups <- data.frame(levels, rep(1,6))
  colnames(sups) <- c("value","strength")
  
  suplines <- list()
  for(i in 1:6){
    if(xts::is.xts(x)){
      suplines[[i]] <- xts::as.xts(rep(levels[i],NROW(x)),zoo::index(zoo(x)))
    } else{
      suplines[[i]] <- data.frame(x=zoo::index(zoo(x)), y=rep(levels[i],NROW(x)))
    }
  }
  
  supports <- list()
  supports$lastpoint <- lastval
  supports$data <- x
  supports$results <- sups
  supports$lines <- suplines
  class(supports) <- "supports"
  
  return(supports)
}


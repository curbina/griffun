###############################################################################
# Collection of General Utilities by Drew Griffith
#
# For more information please visit my blog at http://drewgriffith15.tumblr.com/

###############################################################################
###############################################################################

#' Remove prefixes or suffixes
#' @param sep character separator, defaults to ","
#' @param side where to look, "left" (default) or "right"
#' @param greedy defaults to TRUE
#' @references #' @references http://www.r-bloggers.com/string-manipulations-on-full-names/
#' @examples 
#' ex = c("a", "a, b", "a, b, c", "a, b, c, d")
#' str_filter(ex, side = "left"  , greedy = TRUE)
#' str_filter(ex, side = "right" , greedy = TRUE)
#' str_filter(ex, side = "left"  , greedy = FALSE)
#' str_filter(ex, side = "right" , greedy = FALSE)
str_filter <- function(x, sep = ",", side = "left", greedy = TRUE) {
  gsub(switch(side,
              left  = c(ifelse(greedy, "", "?"), ")", sep, "\\s*"),
              right = c(ifelse(greedy, "?", ""), ")\\s*", sep)) %>%
         c("(.*", ., "(.*)") %>%
         paste0(collapse = ""),
       switch(side, left = "\\2", right = "\\1"), x)
}
###############################################################################

#' Detach prefixes or suffixes
#' @param ... arguments to \code{str_filter}
#' @references http://www.r-bloggers.com/string-manipulations-on-full-names/
#' @examples 
#' ex = c("a", "a, b", "a, b, c", "a, b, c, d")
#' str_detach(ex, side = "left"  , greedy = TRUE)
#' str_detach(ex, side = "right" , greedy = TRUE)
#' str_detach(ex, side = "left"  , greedy = FALSE)
#' str_detach(ex, side = "right" , greedy = FALSE)
###############################################################################
str_detach <- function(x, sep = ",", side = "left", greedy = TRUE) {
  y = str_filter(x, sep, side, greedy) %>% sapply(nchar)
  x[ y > 0 ] = lapply(x[ y > 0 ],
                      function(x, regex = str_filter(x, sep, side, greedy)) {
                        y = c(
                          switch(side,
                                 left = str_replace(x, regex, "") %>%
                                   str_replace(str_c(sep, "\\s*$"), ""),
                                 right = NULL),
                          regex,
                          switch(side,
                                 left = NULL,
                                 right = str_replace(x, regex, "") %>%
                                   str_replace(str_c("^", sep, "\\s*"), ""))
                        )
                        ifelse(!nchar(y), NA, y)
                      })
  sapply(x, function(x){ x[ switch(side, left = 1, right = 2 )]})
}

###############################################################################
#' StockCharts Technical Rank
#'
#' @param x vector or xts
#' @references
#' \url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:sctr}
#' @export
###############################################################################
SCTR <- function(x){
  
  library(TTR)
  library(quantmod)
  
  price <- x
  SMA200 <- 200
  ROC125 <- 125
  SMA50 <- 50
  ROC20 <- 20
  PPO.HIST <- 3
  RSI14 <- 14
  LT.WEIGHT <- .30
  MD.WEIGHT <- .15
  SH.WEIGHT <- .05
  
  #Long-Term Indicators (weighting)
  #  * Percent above/below 200-day SMA (30%)
  #  * 125-Day Rate-of-Change (30%)
  
  SM200 <- SMA(price, SMA200)
  LTSMA <- ((price - SM200) / ((price + SM200) / 2)) * 100
  LTROC <- ROC(price,ROC125) * 100
  LT <- (LTSMA + LTROC) * LT.WEIGHT
  
  #Medium-Term Indicators (weighting)
  #  * Percent above/below 50-day SMA  (15%)
  #  * 20-day Rate-of-Change (15%)
  
  SM50 <- SMA(price, SMA50)
  MDSMA <- ((price - SM50) / ((price + SM50) / 2)) * 100
  MDROC <- ROC(price,ROC20) * 100
  MD <- (MDSMA + MDROC) * MD.WEIGHT
  
  #Short-Term Indicators (weighting)
  #  * 3-day slope of PPO-Histogram (5%)
  #  * 14-day RSI (5%)
  
  EMA12 <- EMA(price,12)
  EMA26 <- EMA(price,26)
  PPO <- (EMA12-EMA26)/EMA26 * 100
  PPO.linear <-  6 * ( WMA(PPO,3) -  mean(last(PPO,3))) / (3 - 1) 
  
  # Mutliplier to get values between 0-100
  PPOlags <- lags(PPO.linear)
  PPOdiff <- PPOlags[,1] - PPOlags[,2]
  NetChgAvg = SMA(PPOdiff, 3);
  TotChgAvg = SMA(abs(PPOdiff), 3);
  ChgRatio = iif(TotChgAvg != 0, (NetChgAvg / TotChgAvg),0);
  
  SHPPO = round(50 * (ChgRatio + 1),3) / 100
  SHRSI <- RSI(price,14) 
  SH <- (SHPPO + SHRSI) * SH.WEIGHT
  
  output <- round(LT + MD + SH, 1)
  return(output)
}
###############################################################################
#' Set up a dataframe for regression
#'
#' This function, when supplied a vector, looks for matches based on correlation
#' or coefficient determination and returns a data frame preparing for a regression model.
#' The function also outputs "newdata" to be used in the forecast horizon if needed, and
#' the location in the data set where the match begins.
#'
#' @param data vector or times series
#' @param n.hist number of data points used for Y in a regression model
#' @param n.fore number of data points in the forecast horizon
#' @param n.match number of matches requested from data set
#' @param model linear, ves - variable elasticity,  or ces - constant elasticity
#' @param use.cd whether to use the coefficient determination or correlation
#' @export
###############################################################################
find.matches <- function(data, n.hist = 35, n.fore = 15, n.match=NULL,
                         model = c("linear","ves","ces"), use.cd = TRUE)
{
  library(xts)
  library(zoo)
  origdata = coredata(data)
  n.data = NROW(origdata)
  model = match.arg(model)
  if (model =="ces") {
    Y = round(log(origdata[((n.data-n.hist)+1):n.data]),4)
  } else { Y = origdata[((n.data-n.hist)+1):n.data]
  }
  if (is.null(n.match)) {
    n.match = floor(n.hist*.4)
  }
  if (model=="ves") {
    n.match = floor(n.match/2)
  }
  
  # correlation table
  correlation.table = rep(NA, n.data)
  for(i in 1:(n.data-(n.hist+n.fore))) {
    window = origdata[i:(n.hist+(i-1))]
    correlation.table[i] = cor(Y, window)
  }
  
  # CD table
  cd.table = round(abs(correlation.table)^2,6)
  
  # find matches
  max.indx = c()
  max.cor = c()
  
  if (use.cd==TRUE){temp = cd.table
  } else {temp = correlation.table}
  
  if (use.cd==TRUE){
    for(i in 1:n.match) {
      indx = which.max(temp)
      c = temp[indx]
      max.indx[i] = indx
      max.cor[i] = c
      #temp[max(0,indx):min(n.data,(indx +
      #  (n.fore+n.hist)))] = NA 12.10.13
      temp[max(0,indx)] = NA
    }} else {for(i in 1:n.match) {
      indx = which.max(temp)
      c = temp[indx]
      max.indx[i] = indx
      max.cor[i] = c
      temp[max(0,indx):min(n.data,(indx +
                                      (n.fore+n.hist)))] = NA}}
  
  # model
  n.match = NROW(max.indx)
  X = matrix(NA, nr=(n.match), nc=(n.hist))
  temp = origdata
  for(i in 1:n.match) {
    X[i,] = temp[max.indx[i]:(max.indx[i]+(n.hist-1))]
  }
  if (model=="ves") {
    Z = log(X)
    X = data.frame(t(rbind(X,Z)))
    df = cbind(data.frame(Y=Y),as.data.frame(X))
  } else if (model=="ces") {
    X = t(log(X))
    df = cbind(data.frame(Y=Y),data.frame(X))
  } else { X = t(X)
           df = cbind(data.frame(Y=Y),data.frame(X))
  }
  
  # newdata formation
  X = matrix(NA, nr=(n.match), nc=(n.fore))
  temp = origdata
  for(i in 1:n.match) {
    X[i,] = temp[(max.indx[i]+n.hist):((max.indx[i]+
                                           n.hist+n.fore)-1)]
  }
  if (model=="ves") {
    Z = log(X)
    newdf = data.frame(t(rbind(X,Z)))
  } else if (model=="ces") {
    X = t(log(X))
    newdf = data.frame(X)
  } else { X = t(X)
           newdf = data.frame(X)
  }
  
  out = list(df,newdf,max.indx)
  names(out)[1] = "rmodel"
  names(out)[2] = "fmodel"
  names(out)[3] = "matchindx"
  return(out)
}

###############################################################################
###############################################################################
#' Adding future dates to a forecast
#'
#' @param dates the dates of the historical values
#' @param forecast a vector of forecasted values
#'
#' @keywords forecast dates
#' @export
#' @examples
#' ##NULL
###############################################################################
extendForecast <- function(dates, forecast) {
  library(timeDate)
  library(xts)
  # TESTING FOR FREQ: 1-daily; 4-quarterly; 12-monthly
  # weekly is not supported by timeDate package
  freq <- frequency(as.timeDate(dates))
  if (freq == 1) {
    h <- NROW(forecast)
    new.dates <- as.Date(dates)
    new.dates <- seq(last(new.dates) + 1, last(new.dates) + 365, by = "day")
    new.dates <- timeDate(new.dates)
    new.dates <- as.Date(new.dates[isBizday(new.dates, holidayNYSE())])
    new.dates <- new.dates[1:h]
    return(as.xts(forecast, new.dates))
  }
  if (freq == 4) {
    h <- NROW(forecast)
    new.dates <- as.timeDate(dates)
    new.dates <- as.Date(alignQuarterly(timeSequence(from = last(dates) +
                                                       1, by = "quarter", length.out = h)))
    return(as.xts(forecast, new.dates))
  }
  if (freq == 12) {
    h <- NROW(forecast)
    new.dates <- as.timeDate(dates)
    new.dates <- as.Date(alignMonthly(timeSequence(from = last(dates) +
                                                     1, by = "month", length.out = h)))
    return(as.xts(forecast, new.dates))
  }
}

###############################################################################
###############################################################################
#' Remove Outliers from a dataset
#'
#' This function removes outliers from a dataset
#' credit for this source code goes to aL3xa on StackOverflow
#'
#' @param x object
#' @param na.rm boolean
#'
#' @return object
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' x <- c(-10, x, 10)
#' y <- remove_outliers(x)
#' par(mfrow = c(1, 2))
#' boxplot(x)
#' boxplot(y)
#' }
#' @export
###############################################################################

remove.outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  i <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - i)] <- NA
  y[x > (qnt[2] + i)] <- NA
  y
}

###############################################################################
###############################################################################
#' Excel data to R
#'
#' This function specifies that you are reading data from the clipboard,
#' that it is tab delimited, and that it has a header.
#'
#' @param header boolean
#'
#' @return object
#'
#' @examples
#' \dontrun{
#' dat=read.excel()
#' }
#' @export
###############################################################################

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

###############################################################################
###############################################################################
#' R object to Excel
#'
#' Exporting a R object to Excel via the clipboard
#'
#' @param x object
#' @param row.names boolean
#' @param col.names boolean
#'
#' @return object
#'
#' @examples
#' \dontrun{
#' write.excel(dat)
#' }
#' @export
###############################################################################

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

###############################################################################
###############################################################################
#' Reverse First and Last Name
#'
#' When there is a "last, first" name or "last first" name string, you can
#' reverse the string to look like "first last" name with this function.
#'
#' @param x string
#' @param p boolean
#'
#' @return string
#'
#' @examples
#' \dontrun{
#' x = "Ruth, Babe"
#' reverse_name(x)
#' }
#' @export
###############################################################################

reverse_name <- function(x, p="TRUE"){
  library(stringr); library(Hmisc)
  if (p == "TRUE"){
    #x = str_replace_all(x, "[^[:print:]]", " ")
    x = str_replace_all(x, "[^a-zA-Z.-]", " ") #keep periods and hyphens
    la = capitalize(substr(str_trim(x, side = "both"),1,
                           str_locate(x, " ")-1))
    fi = capitalize(substr(str_trim(x, side = "both"),
                           str_locate(x, " ")+1,str_length(x)))
    out = str_trim(paste(fi,la), side = "both")
  } else {
    x = str_replace_all(x, "[^a-zA-Z]", " ") #leave out all punctuation
    la = capitalize(substr(str_trim(x, side = "both"),1,
                           str_locate(x, " ")-1))
    fi = capitalize(substr(str_trim(x, side = "both"),
                           str_locate(x, " ")+1,str_length(x)))
    out = str_trim(paste(fi,la), side = "both")
  }
  return(out)
}

###############################################################################
###############################################################################
#' Summarize content
#'
#' This function takes the R object on which we need to obtain statistics (x),
#' how many entries should each summary contain (step, defaulting to 1000),
#' and the function we want to apply (fun, defaulting to mean).
#'
#' @param x object
#' @param step number of entries
#' @param fun function
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' dat<-data.frame(matrix(runif(100000,0,1),ncol=10))
#' summarize.by(dat)
#' }
#' @export
###############################################################################

summarize.by<-function(x,step=1000,fun="mean")
{
  
  if(is.data.frame(x))
  {
    group<-sort(rep(seq(1,ceiling(nrow(x)/step)),step)[1:nrow(x)])
  }
  if(is.vector(x))
  {
    group<-sort(rep(seq(1,ceiling(length(x)/step)),step)[1:length(x)])
  }
  x<-data.frame(group,x)
  x<-aggregate(x,by=list(x$group),FUN=fun)
  x<-x[,-c(1,2)]
  return(x)
}

###############################################################################
###############################################################################
#' Age Years
#'
#' This function for calculating age with two dates
#'
#' @param earlier first date
#' @param later second date
#'
#' @return integer
#'
#' @examples
#' \dontrun{
#' x <- as.Date("2000-02-29")
#' y <- as.Date("2004-02-28")
#' age.years(x, y)
#' }
#' @export
###############################################################################

age.years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}

###############################################################################
###############################################################################
#' Between
#'
#' This function mimics the SQL between clause and returns a logical indx
#'
#' @param x numeric
#' @param low numeric
#' @param high numeric
#' @param ineq boolean
#'
#' @return logical
#'
#' @examples
#' \dontrun{
#' btween( 1:10, 5.5, 6.5 )
#' }
#' @export
###############################################################################

btween <- function(x, low, high, ineq=F) {
  if (ineq) {
    x >= low & x <= high
  } else {
    x > low & x < high
  }
}

###############################################################################
###############################################################################
#' Buy/Sell Indicator
#'
#' This function will provide one buy and one sell indicator for the given
#' series
#'
#' @param x numeric
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' buy.sell(x)
#' }
#' @export
###############################################################################

buy.sell <- function(x){
  buy = x[which.min(x)]
  sell = x[which.max(x)]
  z=x
  for (i in 1:NROW(x)){
    if (i == which.min(x)) {z[i,1] = 100
    }
    if (i == which.max(x)) {z[i,1] = -100
    }
    if (i != which.min(x) & i != which.max(x)) {z[i,1] = 0
    }
  }
  out = list(z,buy,sell)
  names(out)[1] = "Buy.Sell"
  names(out)[2] = "Buy.Price"
  names(out)[3] = "Sell.Price"
  return(out)
}

###############################################################################
###############################################################################
#' Unadjusted R Squared
#'
#' This function will calculate unadjusted R squared value which explains
#' the model more accurately, because the intercept is factored back into the
#' model
#'
#' @param fit lm
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' unadj.rsquared(fit)
#' }
#' @export
###############################################################################

unadj.rsquared <- function(fit){
  rc = summary(fit)$r.squared
  n = NROW(fit$fitted.values)
  ybar = mean(fit$model[,1])
  n_ybar2 = n*ybar^2
  sumsq_y = sum(fit$model[,1]^2)
  k = n_ybar2/sumsq_y
  ru = rc*(1-k)+k
  out = list(ru)
  names(out)[1] = "unadj.rsquared"
  return(out)
}

###############################################################################
###############################################################################
#' Seasonal Averages
#'
#' This function will calculate the seasonal averages of a time series
#' based on the frequency (average of N rows)
#'
#' @param x dataset
#' @param frequency how often
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' seasonals(x,12)
#' }
#' @export
###############################################################################

seasonals <- function(x,frequency){
  return(rowMeans(matrix(x,frequency))/mean(x))
}

###############################################################################
###############################################################################
#' Normalization
#'
#' This function will normalize a vector
#'
#' @param x dataset
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' normalize(x)
#' }
#' @export
###############################################################################

normalize <- function(x){
  minV = min(x)
  maxV = max(x)
  for (i in 1:NROW(x)) {
    x[i] = ((x[i]-minV)/(maxV-minV))
  }
  return(x)
}

###############################################################################
###############################################################################
#' Absolute Average Accuracy
#'
#' This function will calculate the absolute average accuracy of compartive datasets
#'
#' @param y dataset 1
#' @param x dataset 2
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' acc(y,x)
#' }
#' @export
###############################################################################
acc <- function (y, x) {
  return(mean(1-abs((y-x)/y)));
}

###############################################################################
###############################################################################
#' Split string into tokens using delim
#'
#' This function will split given string into tokens using delim
#'
#' @param s input string
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return array of tokens
#'
#' @examples
#' \dontrun{
#' spl('a,b,c')
#' }
#' @export
###############################################################################
spl <- function
(
  s,  		# input string
  delim = ','	# delimiter
)
{
  return(unlist(strsplit(s,delim)));
}

###############################################################################
###############################################################################
#' Join vector of strings into one string using delim
#'
#' This function will join vector of strings into one string using delim
#'
#' @param v vector of strings
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{
#' join(c('a','b','c'), ',')
#' }
#' @export
###############################################################################
join <- function
(
  v, 			# vector of strings
  delim = ''	# delimiter
)
{
  return(paste(v,collapse=delim));
}

###############################################################################
###############################################################################
#' Shortcut for length function
#'
#' This function is a shortcut for length function
#'
#' @param x vector / string / matrix
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{
#' len(1:10)
#' }
#' @export
###############################################################################
len <- function
(
  x	# vector
)
{
  return(length(x))
}

###############################################################################
###############################################################################
#' Faster version of ifelse function
#'
#' This function is a faster version of ifelse function
#'
#' @param cond true / false condition
#' @param truepart resulting value(s) if condition is true
#' @param falsepart resulting value(s) if condition is false
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{
#' iif(1:10 > 5, 1, 1:10)
#' }
#' @export
###############################################################################
iif <- function
(
  cond,		# condition
  truepart,	# true part
  falsepart	# false part
)
{
  if(len(cond) == 1) { if(cond) truepart else falsepart }
  else {
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    
    if(length(truepart) == 1)
      falsepart[cond] = truepart
    else {
      cond = ifna(cond,F)
      falsepart[cond] = truepart[cond]
    }
    
    #falsepart[!is.na(cond)] = temp
    
    return(falsepart);
  }
}

###############################################################################
###############################################################################
#' Replace NA, NaN, Inf values
#'
#' This function will replace all NA, NaN, Inf with given values
#'
#' @param x data to check for NA, NaN, Inf
#' @param y values(s) to be used in place of NA, NaN, Inf
#'
#' @return updated data
#'
#' @examples
#' \dontrun{
#' ifna(c(1,NA,2,Inf,3), 4)
#' }
#' @export
###############################################################################
###############################################################################
ifna <- function
(
  x,	# check x for NA, NaN, Inf
  y	# if found replace with y
) {
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

###############################################################################
###############################################################################
#' Replace NULL values
#'
#' This function will replace all NULL with given value
#'
#' @param x data to check for NULL
#' @param y values to be used in place of NULL
#'
#' @return updated data
#'
#' @examples
#' \dontrun{
#' temp = list()
#' temp$val1 = ifnull(temp$val1, 4)
#' }
#' @export
###############################################################################
ifnull <- function
(
  x,	# check x for NULL
  y	# if found replace with y
) {
  return(iif(is.null(x), y, x))
}

###############################################################################
###############################################################################
#' Faster version of rep fucntion
#'
#' This function is a faster version of rep fucntion
#'
#' @param x data to be repeated
#' @param times number of times to repeat the data
#'
#' @return new data
#'
#' @examples
#' \dontrun{
#' fast.rep(c(1,NA,2,Inf,3), 4)
#' }
#' @export
###############################################################################
fast.rep <- function(x, times) {
  length(x) = times
  x[] = x[1]
  x
}

###############################################################################
###############################################################################
#' Count number of non NA elements
#'
#' This function will count number of non NA elements in the given matrix
#'
#' @param x data matrix
#' @param side margin along which to count
#'
#' @return counts
#'
#' @examples
#' \dontrun{
#' cnt(matrix(c(1,NA,2,3),2,2))
#' }
#' @export
###############################################################################
cnt <- function(
  x,			# matrix with data
  side = 2	# margin along which to count
)
{
  if( is.null(dim(x)) ) {
    sum( !is.na(x) )
  } else {
    apply(!is.na(x), side, sum)
  }
}

###############################################################################
###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{
#' date.dayofweek(Sys.Date())
#' }
#' @export
#' @rdname DateFunctions
###############################################################################
date.dayofweek <- function(dates)
{
  return(as.double(format(dates, '%w')))
}

#' @export
#' @rdname DateFunctions
date.day <- function(dates)
{
  return(as.double(format(dates, '%d')))
}

#' @export
#' @rdname DateFunctions
date.week <- function(dates)
{
  return(as.double(format(dates, '%U')))
}

#' @export
#' @rdname DateFunctions
date.month <- function(dates)
{
  return(as.double(format(dates, '%m')))
}

#' @export
#' @rdname DateFunctions
date.year <- function(dates)
{
  return(as.double(format(dates, '%Y')))
}

###############################################################################
###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week.
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{
#' third.friday.month(2012,1)
#' }
#' @export
###############################################################################
third.friday.month <- function(year, month)
{
  day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
  day = c(20,19,18,17,16,15,21)[1 + day]
  return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}

###############################################################################
###############################################################################
#' Load Packages that are available and install ones that are not available
#'
#' This function a convience wrapper for install.packages() function
#'
#' @param packages names of the packages separated by comma
#' @param repos default repository
#' @param dependencies type of dependencies to install
#' @param ... additional parameters for the \code{\link{install.packages}} function
#'
#' @return nothing
#'
#' @examples
#' \dontrun{
#' load.packages('forecast,quantmod')
#' }
#' @export
###############################################################################
load.packages <- function
(
  packages, 							# names of the packages separated by comma
  repos = "http://cran.r-project.org",# default repository
  dependencies = c("Depends", "Imports"),	# install dependencies
  ...									# other parameters to install.packages
)
{
  packages = spl(packages)
  for( ipackage in packages ) {
    if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
      install.packages(ipackage, repos=repos, dependencies=dependencies, ...)
      
      if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
        stop("package", sQuote(ipackage), 'is needed.  Stopping')
      }
    }
  }
}

###############################################################################
###############################################################################
#' Begin Timing
#'
#' @param identifier name for this timing session
#'
#' @return nothing
#'
#' @examples
#' \dontrun{
#' tic(1)
#' }
#' @export
#' @rdname TimingFunctions
###############################################################################
tic <- function
(
  identifier	# integer value
)
{
  assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}

###############################################################################
###############################################################################
#' End Timing and report elapsed time
#'
#' @param identifier name for this timing session
#'
#' @return elapsed time
#'
#' @examples
#' \dontrun{
#' toc(1)
#' }
#' @export
#' @rdname TimingFunctions
###############################################################################
toc <- function
(
  identifier	# integer value
)
{
  if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
    prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
    diffTimeSecs = proc.time()[3] - prevTime
    cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
  } else {
    cat('Toc error\n')
  }
  return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}

###############################################################################
###############################################################################
#' Lag matrix or vector
#'
#' This function shifts elemnts in a vector or a mtrix by a given lag.
#' For example: mlag(x,1) - use yesterday's values and
#'  mlag(x,-1) - use tomorrow's values
#'
#' @param x vector / matrix
#' @param nlag number of lags, \strong{defaults to 1}
#'
#' @return modified object
#'
#' @examples
#' \dontrun{
#' mlag(1:10)
#' }
#' @export
###############################################################################
mlag <- function
(
  m,			# matrix or vector
  nlag = 1	# number of lags
)
{
  # vector
  if( is.null(dim(m)) ) {
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
    
    # matrix
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    }
  }
  return(m);
}

###############################################################################
###############################################################################
#' Repeat Rows
#'
#' @param m vector (row)
#' @param nr number of copies along rows
#'
#' @return new matrix
#'
#' @examples
#' \dontrun{
#' matrix(1:3, nr=5, nc=3, byrow=T)
#' repRow(1:3, 5)
#' }
#' @export
###############################################################################
repRow <- function(n,nr){
  matrix(m, nr=nr, nc=len(m), byrow=T)
}


###############################################################################
###############################################################################
#' Repeat Columns
#'
#' @param m vector (column)
#' @param nc number of copies along columns
#'
#' @return new matrix
#'
#' @examples
#' \dontrun{
#' matrix(1:5, nr=5, nc=3, byrow=F)
#' repCol(1:5, 3)
#' }
#' @export
###############################################################################
repCol <- function(m,nc){
  matrix(m, nr=len(m), nc=nc, byrow=F)
}

###############################################################################
###############################################################################
#' Create \code{\link{xts}} object, faster version of \code{\link{xts}} fucntion
#'
#' @param x vector / matrix / data frame
#' @param order.by dates that correspond to rows of x
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
###############################################################################
make.xts <- function
(
  x,			# data
  order.by	# date
)
{
  #Sys.setenv(TZ = 'GMT')
  tzone = Sys.getenv('TZ')
  
  orderBy = class(order.by)
  indx = as.numeric(as.POSIXct(order.by, tz = tzone))
  
  # need to handle case for one row; i.e. len(orderBy) == 1
  if( is.null(dim(x)) ) {
    if( len(order.by) == 1 )
      x = t(as.matrix(x))
    else
      dim(x) = c(len(x), 1)
  }
  x = as.matrix(x)
  
  x = structure(.Data = x, 
                indx = structure(indx, tzone = tzone, tclass = orderBy), 
                class = c('xts', 'zoo'), .indxCLASS = orderBy, tclass=orderBy, .indxTZ = tzone, tzone=tzone)
  return( x )
}
###############################################################################
###############################################################################
#' Fast alternative to indx() function for \code{\link{xts}} object
#'
#' NOTE indx.xts is the same name as the indx function in the XTS package
#'
#' @param x \code{\link{xts}} object
#'
#' @return dates
#' 
#' @examples
#' \dontrun{ 
#' indx.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)))
#' }
#' @export 
###############################################################################
indx.xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'indx')
  class(temp) = c('POSIXct', 'POSIXt')
  
  type = attr(x, '.indxCLASS')[1]
  if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
    temp = as.Date(temp)
  return(temp)
}

# other variants that are not currently used
# this function is used in plota for X axis
indx4xts <- function
(
  x			# XTS object
)
{
  temp = attr(x, 'indx')
  class(temp)='POSIXct' 
  
  return(temp)
}

indx2date.time <- function(temp) {
  class(temp)='POSIXct' 
  
  if( attr(x, '.indxCLASS')[1] == 'Date') {	
    as.Date(temp)
  } else {
    as.POSIXct(temp, tz = Sys.getenv('TZ'))
  }
}

###############################################################################
###############################################################################
# Prepare backtest data
#' @export 
###############################################################################
bt.prep <- function
(
  b,				# enviroment with symbols time series
  align = c('keep.all', 'remove.na'),	# alignment type
  dates = NULL,	# subset of dates
  fill.gaps = F	# fill gaps introduced by merging
) 
{    
  # setup
  if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
  symbolnames = b$symbolnames
  nsymbols = len(symbolnames) 
  
  if( nsymbols > 1 ) {
    # merge
    out = bt.merge(b, align, dates)
    
    for( i in 1:nsymbols ) {
      b[[ symbolnames[i] ]] = 
        make.xts( coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE], out$all.dates)
      
      # fill gaps logic
      map.col = find.names('Close,Volume,Open,High,Low,Adjusted', colnames(b[[ symbolnames[i] ]]))
      if(fill.gaps & !is.na(map.col$Close)) {	
        close = coredata(b[[ symbolnames[i] ]][,map.col$Close])
        n = len(close)
        last.n = max(which(!is.na(close)))
        close = ifna.prev(close)
        if(last.n + 5 < n) close[last.n : n] = NA
        b[[ symbolnames[i] ]][, map.col$Close] = close
        indx = !is.na(close)	
        
        if(!is.na(map.col$Volume)) {
          indx1 = is.na(b[[ symbolnames[i] ]][, map.col$Volume]) & indx
          b[[ symbolnames[i] ]][indx1, map.col$Volume] = 0
        }
        
        #for(j in colnames(b[[ symbolnames[i] ]])) {
        for(field in spl('Open,High,Low,Adjusted')) {
          j = map.col[[field]]
          if(!is.na(j)) {
            indx1 = is.na(b[[ symbolnames[i] ]][,j]) & indx
            b[[ symbolnames[i] ]][indx1, j] = close[indx1]
          }}						
      }
    }	
  } else {
    if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]	
    out = list(all.dates = indx.xts(b[[ symbolnames[1] ]]) )
  }
  
  # dates
  b$dates = out$all.dates
  
  # empty matrix		
  dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
  colnames(dummy.mat) = symbolnames
  dummy.mat = make.xts(dummy.mat, out$all.dates)
  
  # weight matrix holds signal and weight information		
  b$weight = dummy.mat
  
  # execution price, if null use Close	
  b$execution.price = dummy.mat
  
  # populate prices matrix
  for( i in 1:nsymbols ) {
    if( has.Cl( b[[ symbolnames[i] ]] ) ) {
      dummy.mat[,i] = Cl( b[[ symbolnames[i] ]] );
    }
  }
  b$prices = dummy.mat	
}
###############################################################################

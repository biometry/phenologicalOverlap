#' Creates abundance dataset from beta distribution
#'
#' Creates abundance dataset from beta distribution with given parameters.
#'
#'  Creates a data.frame, with two columns, one called dates and one called count. It will get n realizations of a beta distributions with the parameters given by shape 1 and shape 2.Those realizations will be converted, that the range of the realizations is no longer between zero and one, but the range between two dates that should be given by StartAndEnd. It will round those to whole numbers and convert them into dates, by treating these realizations as differences from the 2017-01-01. If fillup = TRUE it will fillup the dates that are in between the minimum and maximum of the dates, and give count values of zero for those days.
#'
#' @param StartAndEnd vector; Start and end values in which the dates will be. If those are not date objects, they will be interpreted as difference from the 01-01-1970.
#' @param shape1 numeric; Parameter for underlying beta-distribution.
#' @param shape2 numeric; Parameter for underlying beta-distribution.
#' @param n numeric, whole number; number of drawings to be done from the normal distribution, also the number of observations in the whole dataset.
#' @param FillUp logical; If TRUE not specified datapoints will be filled with 0.
#' 
#' @return a dataframe with two columns; one with date objects, one with the number of observations on the respective dates.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' dat  <- BetaData( shape1 = 5,shape2 = 1, n = 1000, StartAndEnd = c(0,15))
#' func <- as.function(estdist(dat))
#' curve(func, from = as.numeric(min(dat$Date)), to = as.numeric(max(dat$Date)))
#' 
#' @export

BetaData <- function(n, shape1, shape2, StartAndEnd, FillUp=TRUE){
  
  #   creates a data.frame, with two columns, one called dates and one called count.
  #   It will get n realizations of a beta distributions with the parameters given
  #   by shape 1 and shape 2.Those realizations will be converted, that the range of
  #   the realizations is no longer between zero and one, but the range between two dates
  #   that should be given by StartAndEnd. It will round those to whole numbers and convert them into dates, by treating
  #   these realizations as differences from the 2017-01-01. If fillup = TRUE it will
  #   fillup the dates that are in between the minimum and maximum of the dates, and
  #   give count values of zero for those days.
  
  
  StartAndEnd <- as.numeric(StartAndEnd)
  
  values <- rbeta(n, shape1 = shape1, shape2 = shape2) * (max(StartAndEnd)-min(StartAndEnd))  + min(StartAndEnd)
  values <- round(values)
  Dates  <- sort(unique(values))
  Counts <- vector()
  
  for(i in c(1:length(Dates))){
    Counts[i] <- sum(values == Dates[i])
    
  }
  
  Dates <- as.Date(Dates, origin = '1970-01-01')
  
  
  if(FillUp){
    
    return(FillUp(data.frame(Date = Dates, Count = Counts)))
  }else{
    return(data.frame(Date = Dates, Count = Counts))
    
  }
}



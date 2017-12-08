#' Creates abundance dataset
#'
#' Creates abundance dataset from a normal distribution
#'
#' This function will create an abundance dataset, a data.frame, with two columns, one called dates and one called count. It will get n realizations of a normal distributions with the parameters given by mean and sd. It will round those to whole numbers and convert them into dates, by treating these realizations as differences from the 2017-01-01. If fillup = TRUE it will fillup the dates that are in between the minimum and maximum of the dates, and give count values of zero for those days.
#' 
#' @param mean numeric; Mean for the underlying normal distribution, will be interpreted as difference of days from the 01-01-2017
#' @param sd numeric; standard deviation of the underlying normal distribution.
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
#' dat  <- NormData(mean = 0, sd = 5, n = 1000, FillUp = TRUE)
#' func <- as.function(estdist(dat))
#' curve(func, from = min(dat$Date), to = max(dat$Date))
#' 
#' @export

NormData <- function(mean, sd, n, FillUp = TRUE){
  
  #   creates a data.frame, with two columns, one called dates and one called count.
  #   It will get n realizations of a normal distributions with the parameters given
  #   by mean and sd. It will round those to whole numbers and convert them into dates, by treating
  #   these realizations as differences from the 2017-01-01. If fillup = TRUE it will
  #   fillup the dates that are in between the minimum and maximum of the dates, and
  #   give count values of zero for those days.
  
  
  Values         <- rnorm(mean = mean, sd = sd, n = n )
  
  Values <- round(Values)
  Date      <- sort(unique(Values)) #Gives a sorted list of the different digits appearing in Values
  Count <- vector()
  
  for(i in Date){
    Count <- c(Count, sum(Values == i))
  }
  
  mean  <- as.Date(mean,  '2017/01/01')
  Date  <- as.Date(Date, '2017/01/01')
  if(FillUp){
    val   <- FillUp(data.frame(Date, Count))
  }else{
    val   <- data.frame(Date, Count)
  }
}


  
  
#' Creates abundance dataset for given time-range 
#'
#' Creates abundance dataset for given time-range from normal distribution.
#'
#' Will produce a dataset with two columns, one named dates, that will be filled with dates, and one called count with full numbers. It will get n realizations from a normal distribution  with given sd. Those realizations will be converted, so that they lie between the dates given by StartAndEnd. this will be done, by calculating the distance to some dates, that are equally distributed between the maximum and minimum value of the realizations. There will be as many dates as there are days in the range of StartAndEnd, so those represent the actual dates. The count values for those days will be calculated by the number of realizations that have their minimum distance to the respective date. In the end these constructed dates will be converted, so that they lie in between StartAndEnd. 
#'
#' @param startandend vector; Start and end values in which the dates will be. If those are not date objects, they will be interpreted as difference from the 01-01-1970.
#' @param sd numeric; standard deviation of the underlying normal distribution.
#' @param n numeric, whole number; number of drawings to be done from the normal distribution, also the number of observations in the whole dataset.
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
#' dat  <- NormDataDates( sd = 30, n = 1000, StartAndEnd = c(0,15))
#' func <- as.function(estdist(dat))
#' curve(func, from = min(dat$Date), to = max(dat$Date))
#' 
#' @export

NormDataDates <- function(StartAndEnd, sd, n=1000){
  
  #   will produce a dataset with two columns, one named dates, that will be filled with dates,
  #   and one called count with full numbers. It will get n realizations from a normal distribution
  #   with given sd. Those realizations will be converted, so that they lie between the dates given
  #   by StartAndEnd. this will be done, by calculating the distance to some dates, that are
  #   equally distributed between the maximum and minimum value of the realizations. There will be as
  #   many dates as there are days in the range of StartAndEnd, so those represent the actual dates.
  #   The count values for those days will be calculated by the number of realizations that have their
  #   minimum distance to the respective date. In the end these constructed dates will be converted, so
  #   that they lie in between StartAndEnd. 
  
  Values <- rnorm(n = n, sd = sd)
  Rangewidth <- as.numeric(max(StartAndEnd) - min(StartAndEnd))
  Distrwidth <- max(Values) - min(Values)
  Steps <- Distrwidth/Rangewidth
  
  #Make a list of points, that spread regularly over the range of the points drawn from the normla ditribution
  
  Points <- vector()
  Points[1] <- min(Values)+0.5*Steps # start value for the Points, 0.5 Steps is added in advance, so that there is the same distances around each Point
  
  for(i in c(2:Rangewidth)){
    Points[i] <- Points[i-1]+Steps
  }
  Counts <- vector(length=as.numeric(max(StartAndEnd) - min(StartAndEnd))+1)
  for(j in c(1:length(Values))){
    diff <- vector()
    for(k in c(1:length(Points))){
      diff[k] <- abs(Points[k] - Values[j]) #get the differences between the datapoint from the normal distribution to every Point of Points, do that for every Point from the distr.
    }
    # add one 'observation' to the List of Counts, at the same index, at which the connected Point is in Points, so that there will be a sum of the points from the distr. with the lowest distance to that point
    Counts[which(diff == min(diff))] <- Counts[which(diff == min(diff))] + 1 
  }
  data <- data.frame(Dates = as.Date(c(min(StartAndEnd):max(StartAndEnd)), origin = '1970-01-01'), Counts = Counts)
  return(data)
}



#' Crosses out days from abundance dataset(s) using a normal distribution
#'
#' Crosses out days from abundance dataset(s) using a normal distribution
#'
#' this function will reduce the number of observation in this dataset by one on a day, that is determined by a normal distribution, with the parameters mean and sd. if reduction is more than one, then there are several days determined. 
#' 
#' @param data data.frame; an abundance dataset, with one column of date objects, and one column numeric values. 
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance. If FALSE the columns of data1 (and possibly data2) must be named Date and Count.
#' @param mean numeric; the mean of the normal distribution. Number of days after the 01.01.1970
#' @param sd numeric; the standard deviation of the normal distribution.
#' @param reduction numeric; Determines how many 'observations' shall be excluded. 
#'
#' @return A data.frame like data, but with excluded Observations. 
#' 
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' #Create Datasets
#'  a     <- c(1:10)
#'  dates1 <- as.Date(a, origin = '2017-01-01')
#'  count1 <- c(1,1,3,4,6,9,5,4,2,1)
#'  dat1   <- data.frame(dates1, count1)
#'  
#'  normaldist.crossout(dat1, mean = mean(as.numeric(dat1$dates1)), sd = 2, reduction =8)
#' 
#' @export

normaldist.crossout <- function(data, mean, sd, reduction = 1, DataCheck = TRUE){
  
  #   has abundance datasets as input, with two columns, one called dates with dates
  #   in it and one called count with numbers in it. this function will reduce the
  #   number of observation in this dataset by one on a day, that is determined by a normal
  #   distribution, with the parameters mean and sd. if reduction is more than one, then
  #   there are several days determined. Abortion mechanism if given normal distribution is too far from the range of the dataset should be implemented.
  
  if(DataCheck){
    data <- CheckData(data)
  }

  if(reduction > sum(data$Count)){
    stop('You can only crossout as many datapoints as there are there')
  }
  
  reduce  <- vector()
  
  for(i in c(1:reduction)){
    
    red <- as.Date(round(rnorm(n=1, mean = mean, sd = sd)), origin = '1970-01-01')
    stats <- FALSE
    while(stats == F){
      if(length(which(data$Date == red)) == 0){
        red <- as.Date(round(rnorm(n=1, mean = mean, sd = sd)), origin = '1970-01-01')
      }else{
        if(data$Count[which(data$Date == red)] == 0){
          red <- as.Date(round(rnorm(n=1, mean = mean, sd = sd)), origin = '1970-01-01')
        }else{
          stats <- TRUE
        }
      }
    }
    data$Count[which(data$Date == red)] <- data$Count[which(data$Date == red)] - 1
  }
  
  
  
  return(data)
  
}

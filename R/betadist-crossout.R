#' Crosses out days from abundance dataset(s) using a beta distribution
#'
#' Crosses out days from abundance dataset(s) using a beta distribution
#'
#' this function will reduce the number of observation in this dataset by one on a day, that is determined by a beta distribution, with the parameters shape1, shape2, start (start date of the beta distribution) and end (end date of beta distribution). if reduction is more than one, then there are several days determined. 
#' 
#' @param data data.frame; an abundance dataset, with one column of date objects, and one column numeric values. 
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance. If FALSE the columns of data1 (and possibly data2) must be named Date and Count.
#' @param shape1 numeric; the shape1 parameter of the beta distribution.
#' @param shape2 numeric; the shape2 parameter of the beta distribution.
#' @param start date object; determines the start date of the beta distribution. 
#' @param end date object; determines the end date of the beta distribution. If left out it will be start + 130 days.
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
#'  betadist.crossout(dat1, shape1 = 1, shape2 = 4, start = min(dat1$dates1), end = max(dat1$dates1), reduction = 10)
#' 
#' @export

betadist.crossout <- function(data, shape1=1,shape2=10, start= as.Date('2017-01-01'), end, reduction = 1, DataCheck = TRUE){
  #Abortion mechanism if given beta distribution is not in the range of the dataset should be implemented.
  if(is.null(end)){
    end <- start + 130
  }
  
  if(DataCheck){
    data <- CheckData(data)
  }
  
  if(reduction > sum(data$Count)){
    stop('You can only crossout as many datapoints as there are there')
  }
  
  reduce  <- vector()
  
  for(i in c(1:reduction)){
    
    red <- as.Date(round(rbeta(n=1, shape1 = shape1, shape2=shape2)*(abs(as.numeric(end-start))+0.9999)+start), origin = '2017-01-01')
    stats <- FALSE
    while(stats == F){
      if(length(which(data$Date == red)) == 0){
        red <- as.Date(round(rbeta(n=1, shape1 = shape1, shape2=shape2)*(abs(as.numeric(end-start))+0.9999)+start), origin = '2017-01-01')
      }else{
        if(data$Count[which(data$Date == red)] == 0){
          red <- as.Date(round(rbeta(n=1, shape1 = shape1, shape2=shape2)*(abs(as.numeric(end-start))+0.9999)+start), origin = '2017-01-01')
        }else{
          stats <- TRUE
        }
      }
    }
    data$Count[which(data$Date == red)] <- data$Count[which(data$Date == red)] - 1
  }
  
  
  
  return(data)
  
  
}

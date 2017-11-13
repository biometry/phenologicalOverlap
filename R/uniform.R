#' Fits uniform distribution to abundance data
#'
#' Fits uniform distribution to abundance data (data.frame)
#'
#' Fits uniform distribution to abundance data, by using the dunif() function and giving it the minimum and maximum dates of the input dataframe. 
#'
#' @param data data.frame;  abundance data, with one column of date objects and one with numeric values. 
#' @param CheckData logical; if TRUE CheckData function will check format of input data.frame
#' 
#' @return returns a list with the levels: $func = a function object, that is created by this function using the dunif() function. $estimationmethod = 'uniform', $startpoint = minimum of dates column in input dataframe, $endpoint = maximum of dates column in input dataframe. 
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' a     <- c(1:10)
#' dates <- as.Date(a, origin = '2017-01-01')
#' count <- c(1,1,3,4,6,9,5,4,2,1)
#' dat   <- data.frame(dates, count)
#' uniform(dat)
#' 
#' @export

uniform <- function(data, CheckData = TRUE){
  
  #   Takes a abundance dataset as inputwith two columns, one called dates with dates
  #   in it and one called count with numbers. A uniform distribution will be fitted,
  #   by giving the dunif function the minimum and maximum value of the dates column
  #   
  
  if(CheckData){
    data <- CheckData(data)
  }
  
  return(c(func = function(x)dunif(x, min = min(as.numeric(data$Date)), max = max(as.numeric(data$Date))),estimationmethod = 'uniform', startpoint = min(data$Date), endpoint = max(data$Date)))
}
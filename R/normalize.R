#' Creates a dataframe of probabilities from abundances data set
#'
#' Creates a dataframe of probabilities from abundances data set by dividing each datapoint by the sum of observations
#'
#' Takes abundance data as dataframes as input. Will cause an exception if there is not at least one datapoint for each full-day in the range of time given by the maximum and minimum value in the dates column of the input dataframe. 
#'
#' @param data data.frame;  abundance data, with one column of date objects and one with numeric values. 
#' @param Interpolate logical; will be passed to CheckData function
#' @param CheckData logical; if TRUE CheckData function will check format of input data.frame
#'
#' @return returns a list with the levels: $data = dataframe which has the same dates as the input dataframe in one column but has probabilitis instead of observations. those are created by dividing the number of observations for each date through the sum of all observations in the datasets. $estimationmethod = 'normalization', $startpoint = minimum of dates column in input dataframe, $endpoint = maximum of dates column in input dataframe. 
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' a     <- c(1:10)
#' dates <- as.Date(a, origin = '2017-01-01')
#' count <- c(1,1,3,4,6,9,5,4,2,1)
#' dat   <- data.frame(dates, count)
#' normalize(dat)
#' 
#' @export

normalize <- function(data, Interpolate = FALSE, CheckData = TRUE){
  
  #   Takes a abundance dataset as inputwith two columns, one called dates with dates
  #   in it and one called count with numbers. The count values will be divided by the
  #   sum of all the count values the so created new data frame will be returned. 
  
  
  if(CheckData){
    data <- CheckData(data, Interpolate=Interpolate, normalize = TRUE)
  }
  
  if(any(!c(ceiling(min(as.numeric(data$Date))):floor(max(as.numeric(data$Date)))) %in% data$Date) & Interpolate == FALSE){
    stop('Please make sure, that your data has at least one Datapoint for each full day in the range from its minimum to its maximum')
  }
  data$Count <- data$Count/sum(data$Count)
  
  return(list(data = data.frame(x= data$Date, y=data$Count), estimationmethod = 'normalization', startpoint = min(data$Date), endpoint = max(data$Date)))
  
}

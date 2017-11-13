#' Makes a vector of a abundance dataframe
#'
#' Makes a vector with each date stored in it as many times as there were observations.
#'
#' Makes a vector with each date stored in it as many times as there were observations.
#' 
#' @param data data.frame;  abundance data, with one column of date objects and one with numeric values. 
#' @param CheckData logical; If TRUE the function will check the data input whether it fits the requirements using the CheckData function. If FALSE it will process without, which might result in wrong results so to use the default value (TRUE) is strongly recommended.
#' 
#' @return A vector where each date in the input data.frame is stored in as many times as there are counts in the same row in the numeric column.
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
#' CountsToDrawing(dat)
#' 
#' @export

CountsToDrawing <- function(data, CheckData = TRUE){
  #make a new vector, which stores the dates as often as they appear, to make the estimation of the distribution possible
  
  if(CheckData == TRUE){
    data <- CheckData(data, Interpolate = FALSE)
  }
  
  newCount <- vector()
  for(i in c(1:length(data$Date))){
    for(j in c(1:data$Count[i])){
      newCount <- c(newCount, as.numeric(data$Date[i]))
    }
  }
  return(newCount)
}
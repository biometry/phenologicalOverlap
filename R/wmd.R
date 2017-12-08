#' Calculates the weighted mean day for abundance dataset
#'
#' Calculates the weighted mean day of an abundance dataset by weigthing the days according to the observances on that day. 
#'
#' Calculates the weighted mean day of an abundance dataset by weigthing the days according to the observances on that day. This is accomplished by using the mena function on an vector created by the CountsToDrawing function.
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
#' wmd(dat)
#' 
#' @export

wmd <- function(data, CheckData = TRUE){
  #   takes abundance data as input, determines the weigthed mean day by first 
  #   creating a vector from the abundance data, where the days are stored in as 
  #   many times, as the respective number of occurence. And then simply returns the mean of it.
  

  data <- CountsToDrawing(data, CheckData = CheckData)
  
  return(mean(data))
}



 
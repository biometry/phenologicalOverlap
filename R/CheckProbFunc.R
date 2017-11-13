#' checks input data for EstimatorData() Function
#'
#' checks input data if it fits the requirenments of the EstimatorData() Function
#'
#' This function Checks whether given data fits requirenments for the EstimatorData() function those need to have a $startpoint, $endpoint and $func,  $func needs to be a function It doesn't return anything. Use prior to EstimatorData() Function.
#'   
#' @param data list; list that is to be checked, as returned by estdist() or uniform() function. 
#'
#' @return nothing.
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
#' checkProbFunc(estdist(dat, returnonlyfunction = FALSE))
#' 
#' 
#' @export
#' 

checkProbFunc <- function(data){
  
  # This function Checks whether given data fits requirenments for the EstimatorData() function
  # those need to have a $startpoint, $endpoint and $func,  $func needs to be a function
  # It doesn't return anything
  
  if(is.null(data$startpoint) | is.null(data$endpoint)){
    stop('If you provide a function, the start and the end value where to use that function need to be concatinated to the function like: c(func=function, startdate=..., enddate=....')
  }
  if(is.null(data$func)){
    stop('please make sure you give an object where $func is th function, a $startpoint and a $endpoint must also be callable' )
  }
}
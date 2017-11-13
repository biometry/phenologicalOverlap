#' checks input data for EstimatorData() Function
#'
#' checks input data if it fits the requirenments of the EstimatorData() Function
#'
#' This function checks if given data fits the requirenments for the EstimatorData() function. It will return a list with the given dataframe, startpoint and endpoint (if those don't exist it will take their minimum and maximum values) As it checks the data, no specific input is needed, but it will only return if its a data frame with two rows (one that contains only dates the other only numeric) and when there is a row for every day in between the start and the enddate, and if those don't exist between the first and the last x-value. Makes an Object usable for EstimatorData() Function.
#'   
#' @param data list; list as returned by the functions: kernelest() and normalize(). 
#' @param interpolate logical; determines whether data should be interpolated later on or not. Will run additional Checks if TRUE. 
#'
#' @return list; with the levels: x = the dates in the inup data.frame, y = the corresponding probability densities, startpoint = (if not declared in input list) minimum of dates in input data.frame, endpoint = (if not declared in input list) maximum of dates in input data.frame.
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
#' pdf   <- CheckProbData(kernelest(dat))
#' 
#' 
#' @export
#' 


CheckProbData <- function(data, Interpolate = TRUE){
  
  # This function checks if given data fits the requirenments for the EstimatorData()
  # function. It will return a list with the given dataframe, startpoint and endpoint
  # (if those don't exist it will take their minimum and maximum values)
  # As it checks the data, no specific input is needed, but it will only return if its 
  # a data frame with two rows (one that contains only dates the other only numeric)
  # and when there is a row for every day in between the start and the enddate, and if those
  # don't exist between the first and the last x-value
  
  minimumlength <- 10
  nodates <- vector()
  starter <- NULL
  ender   <- NULL
  
  if(! is.null(data$startpoint)){
    starter <- data$startpoint
  }
  if(! is.null(data$endpoint)){
    ender <- data$endpoint
  }
  if(! is.null(data$estimationmethod)){
    estmeth <- data$estimationmethod
  }
  
  for(i in data){
    if(is.data.frame(i)){
      data <- i
    }
    
  }
  
  if(length(data) != 2){
    stop('Please make sure your data has two columns')
  }
  
  # if(length(data[,1]) < minimumlength){
  #   stop(paste0('Please make sure, that you have at least ', minimumlength,' datapoints'))
  # }
  
  for(i in c(1,2)){
    
    if(any(!(inherits(data[,i], 'Date')))){
      nodates <- c(nodates,i)
    }
  }
  
  if(length(nodates) > 1 | length(nodates) == 0){
    stop('Please make sure that one column contains only Dates')
  }
  
  if(nodates == 1){
    dates <- 2
  }
  
  if(nodates == 2){
    dates <- 1
  }
  
  for( j in c(1:length(data[,nodates]))){
    
    if(!(is.numeric(data[j,nodates]))){
      stop(paste0('The data in column: ', nodates, 'in line: ', j,' is not numeric, but thats necessary' ))
    }
  }
  
  if(is.null(starter)){
    starter <- min(data[[dates]])
  }
  
  if(is.null(ender)){
    ender   <- max(data[[dates]])
  }
  
  if(!Interpolate){
    
    if(any(!c(ceiling(as.numeric(starter)):floor(as.numeric(ender))) %in% data[[dates]]) & estmeth != 'kernel density estimation'){
      stop('Please make sure, that your data has at least one Datapoint for each full day (each full date needs to be in the Dates column) in the range from its minimum (startpoint) to its maximum (endpoint)')
    }
    
    if(any(!data[[dates]] %in% c(ceiling(as.numeric(starter)):floor(as.numeric(ender))))){
      warning('If Interpolate is set FALSE  the functions will only use the data for full dates')
      index <- which(data[[dates]] %in% c(ceiling(as.numeric(starter)):floor(as.numeric(ender))))
      return(list(x = data[[dates]][index], y = data[[nodates]][index], startpoint = starter, endpoint = ender))
    }else{
      return(list(x = data[[dates]], y = data[[nodates]], startpoint = starter, endpoint = ender))
    }
    
  }else{
    return(list(x = data[[dates]], y = data[[nodates]], startpoint = starter, endpoint = ender))
  }
  
}


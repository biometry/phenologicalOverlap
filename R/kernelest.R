#' Runs Kernel Density Estimation for abundance data
#'
#' This function will perform a Kernel Density Estimation
#'
#' This function performs a simple Kernel Density estimation using the density() function. By default the bandwidth will be selected using the h.ucv() function from the 'kedd' package. 
#'
#' @param kernel string; the kerel which shall be used. Can be "gaussian", "epanechnikov", "rectangular", "triangular" or "biweight".
#' @param bandwidth numeric; the bandwidth for the kernel density estimation. If is null it will be determined by the unbiased cross validation using the h.ucv() function from the 'kedd' package.
#' @param numberofdatapoints numeric, whole number; determines, how much datapoints shall be produced by the density() function. As can be seen in the help of density() this shall be a power of two.  
#' @param CheckData logical; If TRUE the function will check the data input whether it fits the requirements using the CheckData function. If FALSE it will process without, which might result in wrong results so to use the default value (TRUE) is strongly recommended.
#'
#' @return returns a lit with 5 levels: data = results of the kernel density estimation (dataframe) with sublevels 'x' and 'y',estimationmethod = 'kernel density estimation', kernel = used kernel, bandwidth = used bandwidth, startpoint = first date in the data level, endpoint =  last point in data level)
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
#' kernelest(dat)
#' 
#' @export


kernelest <- function(data, kernel = 'gaussian', bandwidth = NULL, numberofdatapoints = 1024, CheckData = TRUE){
  
  
  #   Takes a abundance dataset as inputwith two columns, one called dates with dates
  #   in it and one called count with numbers. The kernel density estimation will be
  #   runned with the gaussian kernel by default. The bandwidth will be chosen with 
  #   the h.ucv function from the kedd package by default, if no other value is given. Will return a 
  #   dataframe, with two columns, x and y which represent the probability density 
  #   function along with the bandwidth, the start and end date, which are determined 
  #   by the minimum and maximum date of the output dataframe. 
  
  
  if(!(is.null(bandwidth))){
    if(is.numeric(bandwidthestimation)){
      bandwidth <- bandwidthestimation
    }else{
      stop('bandwidth must be numeric or null for default')
    }
  }
  
  if(!(kernel %in% c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight"))){
    stop('kernel must be one of those: "gaussian", "epanechnikov", "rectangular", "triangular", "biweight"')
  }
  
  if(CheckData){
    data <- CheckData(data)
  }
  newdata   <- CountsToDrawing(data)
  
  if(is.null(bandwidth)){
    
    PackageCheck('kedd')
    
    if(kernel == 'rectangular'){
      bandwidth <- h.ucv(newdata, kernel = 'uniform')$h
    }else{
      bandwidth <- h.ucv(newdata, kernel = kernel)$h
    }
  }
  dens <- density(x = newdata, bw = bandwidth, kernel = kernel, n = numberofdatapoints)
  return(list(data = data.frame(x = as.Date(dens$x, origin= '1970-01-01'), y = dens$y),estimationmethod = 'kernel density estimation', kernel = kernel, bandwidth = bandwidth, startpoint = min(dens$x), endpoint = max(dens$x)))
}



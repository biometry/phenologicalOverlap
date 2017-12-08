#' calculates Lloyd's interspecies patchiness
#'
#' calculates Lloyd's interspecies patchiness from two datasets
#'
#' calculates Lloyd's interspecies patchiness from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set False it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing Lloyd's interspecies patchiness.
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
#'  b     <- c(6:15)
#'  dates2 <- as.Date(b, origin = '2017-01-01')
#'  count2 <- c(1,2,4,5,6,9,3,4,1,1)
#'  dat2   <- data.frame(dates2, count2)
#'  
#'  #Calculate corresponding probability densities
#'  a1 <- estdistr(dat1, returnonlyfunction = FALSE)
#'  b1 <- kernelest(dat2)
#'  
#'  #Calculate Lloyd's interspecies patchiness
#'  OLE.lloyd(a1,b1)
#' 
#' @export

OLE.lloyd <- function(data1, data2, datapoints=NULL, CheckData = TRUE, Interpolate = TRUE){
  
  if(CheckData){
    dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = TRUE)
    dat1 <- dat[[1]]
    dat2 <- dat[[2]]
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  if(length(dat1$y) < length(dat2$y)){
    xs <- dat1$x
  }else{
    xs <- dat2$x
  }
  
  multi    <- vector()
  squares1 <- vector()
  squares2 <- vector()
  lloyd    <- 0
  
  for(i in xs){
    multi    <- c(multi, dat1$y[which(dat1$x == i)] * dat2$y[which(dat2$x == i)])
  }
  
  lloyd <- sum(multi) * sum(dat1$y, dat2$y)
  
  
  return(lloyd)
}

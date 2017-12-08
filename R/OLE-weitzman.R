#' calculates Weitzman's Delta
#'
#' calculates Weitzman's Delta from two datasets
#'
#' calculates Weitzman's Delta from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set False it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param addtoplot logical; if TRUE an lines() function will be called which illustrates the outcome. (actually the Weitzman's Delta is the area under this line)
#' @param consider string; can either be : 'both', 'data1' or 'data2'. If both Weitzman's Delta will be calculated as usual, if it os data1 it will calculate the Integral of data1 in the range where both datasets overlap. Accordingly for 'data2'. 
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing Weitzman's Delta.
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
#'  #Calculate Weitzman's Delta
#'  OLE.weitzman(a1,b1)
#' 
#' @export
  


  
OLE.weitzman <- function(data1, data2, datapoints=NULL, addtoplot = FALSE, consider='both', CheckData = TRUE, Interpolate = TRUE){
  
  # needs two ddataframes as input, those shall represent the values of a timeline and the belonging probability densities. The Program Will determine Weitzmanns delta with the help of those by taking. if consider is data1 or data2 it will only give the area under the PDF for the respective data in the overlaprange. 
  
  
  if(!consider %in% c('both','data1','data2')){
    stop('Consider only takes: both, data1, data2. It determines, whether the overlap should be calculated for both datasets or if it considers only one dataset in the overlaprange')
  }
  
  if(CheckData){
    dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = TRUE)
    dat1 <- dat[[1]]
    dat2 <- dat[[2]]
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  mins <- vector()
  if(consider == 'both'){
    xs <- sort(unique(dat1$x, dat2$x))
    xs <- xs[which(xs %in% dat1$x & xs %in% dat2$x)]
  }
  
  if(consider == 'data1'){
    xs      <- dat1$x
  }
  if(consider == 'data2'){
    xs      <- dat2$x
  }
  area    <- 0
  i1 <- as.numeric(xs[1])
  
  
  for(i in xs){
    
    
    if(consider == 'both'){
      mins  <- c(mins, min(dat1$y[which(dat1$x == i)], dat2$y[which(dat2$x == i)]))
      
    }
    
    if(consider == 'data1'){
      mins  <- c(mins, dat1$y[which(dat1$x == i)])
    }
    
    if(consider == 'data2'){
      mins  <- c(mins, dat2$y[which(dat2$x == i)])
    }
    
    
    area <- area + min(mins[which(xs == i1)], mins[which(xs == i)]) * abs(as.numeric(i - i1)) 
    area <- area + (abs(mins[which(xs == i)] - mins[which(xs == i1)]) * abs(as.numeric(i - i1)) * 0.5)
    
    i1   <- i
  }
  
  
  if(addtoplot == TRUE){
    
    lines(xs, mins, ylab=' ', xlab=' ', col='green')
    
  }
  
  return(area)
  
}


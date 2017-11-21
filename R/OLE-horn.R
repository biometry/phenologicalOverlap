#' calculates Horn's Ro
#'
#' calculates Horn's Ro from two datasets
#'
#' calculates Horn's Ro from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set False it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing Horn's Ro.
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
#'  #Calculate Horn's Ro
#'  OLE.horn(a1,b1)
#' 
#' @export

OLE.horn <- function(data1, data2, CheckData = TRUE, datapoints = NULL, Interpolate = TRUE){
  
  # needs two ddataframes as input, those shall represent the values of a timeline and the belonging probability densities. Will calculate Horns RO with the help of those, by going through the x-values that both datasets have in common. This function is ment to be called by the OLE function.
  
  if(CheckData){
    dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = FALSE)
    dat1 <- dat[[1]]
    dat2 <- dat[[2]]
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  xs   <- sort(unique(c(dat1$x,dat2$x)))
  d1    <- vector()
  d2    <- vector()
  both <- vector()
  first <- T
  d21 <- 0 
  
  for(i in xs){
    d10 <- F
    d20 <- F
    
    if(i %in% dat1$x & i %in% dat2$x){
      if(!first){
        if(dat1$y[which(dat1$x == i)] != 0 & dat2$y[which(dat2$x == i)] != 0 ){
          
          d1                 <- c(d1, min(dat1$y[which(dat1$x == i)],dat1$y[which(dat1$x == j)])*abs(i-j) + abs(dat1$y[which(dat1$x == i)]-dat1$y[which(dat1$x == j)])*abs(i-j)*0.5)
          d2                 <- c(d2, min(dat2$y[which(dat2$x == i)],dat2$y[which(dat2$x == j)])*abs(i-j) + abs(dat2$y[which(dat2$x == i)]-dat2$y[which(dat2$x == j)])*abs(i-j)*0.5)
          both               <- c(both, d2[length(d2)]+d1[length(d1)])
          d1[length(d1)]     <- d1[length(d1)]*log(d1[length(d1)],base =10)
          d2[length(d2)]     <- d2[length(d2)]*log(d2[length(d2)],base =10)
          both[length(both)] <- both[length(both)]*log(both[length(both)], base=10)
          
        }else{
          if(dat1$y[which(dat1$x == i)] == 0 & dat1$y[which(dat1$x == j)] == 0){
            d10 <- TRUE
          }
          if(dat2$y[which(dat2$x == i)] == 0 & dat2$y[which(dat2$x == j)] == 0){
            d20 <- TRUE
          }
        }
      }
      
      if(!i %in% dat1$x & i %in% dat2$x| i %in% dat2$x & d10 ){
        if(dat2$y[which(dat2$x == i)] > 0 | dat2$y[which(dat2$x == j)] > 0){
          
          d2                 <- c(d2, min(dat2$y[which(dat2$x == i)],dat2$y[which(dat2$x == j)])*abs(i-j) + abs(dat2$y[which(dat2$x == i)]-dat2$y[which(dat2$x == j)])*abs(i-j)*0.5)
          both               <- c(both,  d2[length(d2)])
          d2[length(d2)]     <- d2[length(d2)]*log(d2[length(d2)],base =10)
          both[length(both)] <- both[length(both)]*log(both[length(both)], base=10)
        }
      }
      
      if(i %in% dat1$x & !i %in% dat2$x| i %in% dat1$x & d20){
        if(dat1$y[which(dat1$x == i)] > 0 | dat1$y[which(dat1$x == j)] > 0){
          
          
          d1                 <- c(d1, min(dat1$y[which(dat1$x == i)],dat1$y[which(dat1$x == j)])*abs(i-j) + abs(dat1$y[which(dat1$x == i)]-dat1$y[which(dat1$x == j)])*abs(i-j)*0.5)
          both               <- c(both, d1[length(d1)])
          d1[length(d1)]     <- d1[length(d1)]*log(d1[length(d1)],base =10)
          both[length(both)] <- both[length(both)]*log(both[length(both)], base=10)
        }
      }
    }
    first <- F
    j <- i
  }
  
  log(2,base = 10)
  return((sum(both) - sum(d1) - sum(d2))/(2*log(2, base = 10)))
}
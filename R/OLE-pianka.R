#' calculates Pianka's alpha
#'
#' calculates Pianka's alpha from two datasets
#'
#' calculates Pianka's alpha from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set FALSE it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing Pianka's alpha.
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
#'  #Calculate Pianka's alpha
#'  OLE.pianka(a1,b1)
#' 
#' @export

OLE.pianka <- function(data1, data2, datapoints=NULL, CheckData = TRUE, Interpolate = TRUE){
  
  # needs two dataframes as input, those shall represent the values of a timeline and the belonging probability densities. Will calculate piankas alpha with the help of those, by going through the x-values that both datasets have in common. This function is ment to be called by the OLE function.
  
  if(CheckData){
    dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = FALSE)
    dat1 <- dat[[1]]
    dat2 <- dat[[2]]
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  xs <- sort(unique(c(dat1$x,dat2$x)))
  
  
  multi        <- vector()
  squares1     <- vector()
  squares2     <- vector()
  areamulti    <- 0
  areasquares1 <- 0
  areasquares2 <- 0
  pianka       <- 0
  j            <- 0
  
  for(i in xs){
    if(i %in% dat1$x & i %in% dat2$x){
      multi    <- c(multi, dat1$y[which(dat1$x == i)] * dat2$y[which(dat2$x == i)])
      squares1 <- c(squares1, dat1$y[which(dat1$x == i)]**2)
      squares2 <- c(squares2, dat2$y[which(dat2$x == i)]**2)
    }
    if(i %in% dat1$x & ! i %in% dat2$x){
      multi    <- c(multi, 0)
      squares1 <- c(squares1, dat1$y[which(dat1$x == i)]**2)
      squares2 <- c(squares2, 0)
    }
    if(i %in% dat2$x & ! i %in% dat1$x){
      multi    <- c(multi, 0)
      squares1 <- c(squares1, 0)
      squares2 <- c(squares2, dat2$y[which(dat2$x == i)]**2)
    }
    
    j <- j+1
    
    if(j > 1){
      areamulti    <- areamulti + min(multi[j],multi[j-1])*abs(xs[j] - xs[j-1]) + abs(multi[j]-multi[j-1])*abs(xs[j] - xs[j-1])*0.5
      areasquares1 <- areasquares1 + min(squares1[j],squares1[j-1])*abs(xs[j] - xs[j-1]) + abs(squares1[j]-squares1[j-1])*abs(xs[j] - xs[j-1])*0.5
      areasquares2 <- areasquares2 + min(squares2[j],squares2[j-1])*abs(xs[j] - xs[j-1]) + abs(squares2[j]-squares2[j-1])*abs(xs[j] - xs[j-1])*0.5
    }
  }
  pianka <- areamulti/(areasquares1*areasquares2)**0.5
  
  return(pianka)
}

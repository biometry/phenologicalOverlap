#' calculates the Dissimilarity Index
#'
#' calculates the Dissimilarity Index from two datasets
#'
#' calculates the Dissimilarity Index from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set False it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param addtoplot logical; if TRUE an lines() function will be called which illustrates the outcome. (actually the the Dissimilarity Index is the area under this line)
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing the Dissimilarity Index.
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
#'  #Calculate the Dissimilarity Index
#'  OLE.dissimilarity(a1,b1)
#' 
#' @export
#' 
OLE.dissimilarity <- function(data1, data2, datapoints=NULL, Interpolate = TRUE, addtoplot = FALSE, CheckData = TRUE){

if(CheckData){
  dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = FALSE)
  dat1 <- dat[[1]]
  dat2 <- dat[[2]]
}else{
  dat1 <- data1
  dat2 <- data2
}


xs <- sort(unique(c(dat1$x, dat2$x)))

multi     <- vector()


areamulti <- 0

j1        <- as.numeric(xs[1])
for(i in xs){
  
  if(i %in% dat1$x & i %in% dat2$x){
    multi     <- c(multi, abs(dat1$y[which(dat1$x == i)] - dat2$y[which(dat2$x == i)]))
    
  }else{
    
    if(! i %in% dat1$x){
      multi     <- c(multi, dat2$y[which(dat2$x == i)])
      
    }else{
      multi     <- c(multi, dat1$y[which(dat1$x == i)])
    }
  }

  areamulti <- areamulti + min(multi[which(xs == i)], multi[which(xs == j1)]) * abs(as.numeric(i - j1))
  areamulti <- areamulti + abs(multi[which(xs == i)] - multi[which(xs == j1)]) * abs(as.numeric(i - j1)) * 0.5
  
  j1        <- i
}


if(addtoplot == TRUE){
  if(Interpolate == TRUE){
    lines(xs, multi, ylab=' ', xlab=' ', col='red')
  }else{
    points(xs, multi, ylab=' ', xlab=' ', col='red', type = 'p')
  }
  
}

return(areamulti)
}

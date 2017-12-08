#' Creates two datasets with same x-values
#'
#' Creates two datasets with same x-values 
#'
#'  Takes two datasets, it will return a list, of two data-frames, which now have the same x-values. The function will linearly interpolate the x-values for all 'xs' (from data1 and data2) for both data. the input data need to have two columns, one called x and one called y. those should include only numeric values.
#'
#' @param data1 data.frame; dataset, which shall have the same x-values (Dates) as data2. It must have two columns, one named 'x' and one named 'y' both containing only numeric values.
#' @param data2 data.frame; dataset, which shall have the same x-values (Dates) as data1. It must have two columns, one named 'x' and one named 'y' both containing only numeric values.
#' 
#' @return a list of data.frames. $data1 is the same as data1 (input) but has in addition the x-values (Dates) of data2 (input), and vice-versa.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' a     <- NormData(mean=0 , sd=5, n = 100)
#' b     <- NormData(mean=50, sd=5, n = 100)
#' a1    <- kernelest(a)$data
#' b1    <- kernelest(b)$data
#' dats  <- sameXs(a1,b1)
#' a1new <- dats[[1]]
#' b1new <- dats[[2]]
#' 
#' @export

sameXs <- function(data1, data2){
  
  #   Takes two abundance datasets, it will return a list, of two data-frames,
  #   which now have the same x-values. The function will linearly interpolate
  #   the x-values for all 'xs' (from data1 and data2) for both data.
  #   the input data need to have two columns, one called x and one called y.
  #   those should include only numeric values.
  
  mins <- min(min(data1$x),min(data2$x))
  maxs <- max(max(data1$x),max(data2$x))
  xs   <- sort(unique(c(data2$x, data1$x)))
  d1x  <- vector()
  d1y  <- vector()
  d2x  <- vector()
  d2y  <- vector()
  
  for(i in xs){
    
    if((!(i %in% data1$x))&! i < min(data1$x) &! i > max(data1$x) ){
      after  <- min(which(data1$x > i))
      before <- after-1
      newy   <- ((data1$y[after] - data1$y[before]) / as.numeric(data1$x[after] - data1$x[before])) * (i - as.numeric(data1$x[before])) + data1$y[before]
      
      d1x <- c(d1x,i)
      d1y <- c(d1y,newy)
    }
    
    if( i < min(data1$x) | i > max(data1$x)){
      d1x <- c(d1x, i)
      d1y <- c(d1y, 0)
    }
    
    if(i %in% data1$x){
      d1x <- c(d1x,i)
      d1y <- c(d1y, data1$y[which(data1$x == i)])
    }
    
    
    if(!i %in% data2$x & ! i <= min(data2$x) & ! i >= max(data2$x)){
      after  <- min(which(data2$x > i))
      before <- after-1
      newy   <- ((data2$y[after] - data2$y[before]) / as.numeric(data2$x[after] - data2$x[before])) * (i -  as.numeric(data2$x[before])) + data2$y[before]
      
      d2x <- c(d2x, i)
      d2y <- c(d2y, newy)
      
    }
    if( i < min(data2$x) | i > max(data2$x)){
      d2x <- c(d2x, i)
      d2y <- c(d2y, 0)
    }
    
    if(i %in% data2$x){
      d2x <- c(d2x,i)
      d2y <- c(d2y, data2$y[which(data2$x==i)])
    }
  }
  
  d1 <- data.frame(x = d1x, y = d1y)
  d2 <- data.frame(x = d2x, y = d2y)
  
  
  
  return(list(d1,d2))
}



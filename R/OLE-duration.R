#' calculates the duration ratio
#'
#' calculates the duration ratio from two abundance-datasets
#'
#' calculates the duration ratio, by dividing the duration that both datasets share by the duratio of both datasets (meaning from earliest point to the latest, if there i a continuity between the two datasets (if they share at least one datapoint)). The function takes abundance datasets as input. A list with the levels $startpoint and $endpoint can be used alternatively. 
#'   
#' @param data1 data.frame or list; can either be an abundance dataset or an object for which the levels $startpoint and $endpoint are numeric values. The abundance dataset needs to have the levels $Date and $Count if CheckData = FALSE.
#' @param data1 data.frame or list; can either be an abundance dataset or an object for which the levels $startpoint and $endpoint are numeric values. The abundance dataset needs to have the levels $Date and $Count if CheckData = FALSE.
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance.
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
#'  OLE.duration(dat1, dat2)
#' 
#' @export

OLE.duration <- function(data1, data2, CheckData = TRUE){

#needs two objects that have a $startdate and a $enddate. With those it will calculate it will calculate the percentage how much of this timerange they have in common.

if(CheckData){
  data1 <- CheckData(data1)
  data2 <- CheckData(data2)
}

if(is.null(data1$startpoint)){
  min1 <- min(data1$Date)
}else{
  min1 <- data1$startpoint
}
  if(is.null(data2$startpoint)){
    min2 <- min(data2$Date)
  }else{
    min2 <- data2$startpoint
  }
  if(is.null(data1$endpoint)){
    max1 <- max(data1$Date)
  }else{
    max1 <- data1$endpoint
  }
  if(is.null(data2$endpoint)){
    max2 <- max(data2$Date)
  }else{
    max2 <- data2$endpoint
  }
wholerange <- c(min(min1, min2), max(max1, max2))
OLrange    <- c(max(min1, min2), min(max1, max2))

if(OLrange[1] > OLrange[2]){
  return(0)
}else{
  return(as.numeric((OLrange[2] - OLrange[1])/(as.numeric(wholerange[2] - wholerange[1]))))
}
}


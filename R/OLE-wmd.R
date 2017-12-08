#' calculates a 'weighted mean day' based overlap measure
#'
#' calculates the overlap on the basis of the concept of the 'weighted mean day' 
#'
#'  calculates the overlap on the basis of the concept of the 'weighted mean day' by dividing the difference of wmd by the square-root of the sum of the squared standard deviations of both datasets.
#'   
#' @param data1 data.frame; abundance dataset. Needs a column with only date objects and one with only numerics. If CheckData = TRUE those need to be named $Date and $Count.
#' @param data2 data.frame; abundance dataset. Needs a column with only date objects and one with only numerics. If CheckData = TRUE those need to be named $Date and $Count. needs to have the levels $Date and $Count if CheckData = FALSE.
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance.
#'
#' @return numeric value representing the overlap.
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
#'  OLE.wmd(dat1, dat2)
#' 
#' @export
#' 

OLE.wmd <- function(data1, data2, CheckData = TRUE){
  
  #This will calculate the weighted mean day of an abundance dataframe, with one column of dates and one of numerics. 
  
  if(CheckData){
    data1 <- CheckData(data1)
    data2 <- CheckData(data2)
  }
  
  wmd1 <- wmd(data1, CheckData=FALSE)
  wmd2 <- wmd(data2, CheckData=FALSE)
  sum1 <- sum(data1$Count)
  sum2 <- sum(data2$Count)
  z    <- abs(wmd1 - wmd2)/(((sd(data1$Count)**2)/sum1)+(sd(data2$Count)**2/sum2))**0.5
  
  return(z)
}

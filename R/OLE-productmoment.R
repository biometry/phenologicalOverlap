#' calculates product-moment correlation coefficient
#'
#' calculates product-moment correlation coefficient from two datasets
#'
#'  Calculates the product moment correlation coefficient as found in 'The Measurement of Niche Overlap and Some Relatives' (Hurlbert, S.H. 1971). The function takes abundance datasets as input. 
#'
#' @param data1 data.frame; can either be an abundance dataset.
#' @param data1 data.frame; can either be an abundance dataset.
#' @param CheckData logical; if TRUE the CheckData() function will be run in advance. 
#'
#' @return numeric value representing product-moment correlation coefficient.
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
#'  OLE.productmoment(dat1,dat2)
#' 
#' @export

OLE.productmoment <- function(data1, data2, CheckData = TRUE){
  
  if(CheckData){
    dat1 <- CheckData(data1)
    dat2 <- CheckData(data2)
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  if(length(dat1$Count) < length(dat2$Count)){
    xs <- dat1$Date
  }else{
    xs <- dat2$Date
  }
  
  multi    <- vector()
  squares1 <- vector()
  squares2 <- vector()
  product   <- 0
  propdat1 <- sum(dat1$Count)/sum(c(dat1$Count,dat2$Count))
  propdat2 <- sum(dat2$Count)/sum(c(dat1$Count,dat2$Count))
  
  for(i in xs){
    multi    <- c(multi, (dat1$Count[which(dat1$Date == i)] - propdat1) * (dat2$Count[which(dat2$Date == i)] - propdat2))
    squares1 <- c(squares1, (dat1$Count[which(dat1$Date == i)]-propdat1)**2)
    squares2 <- c(squares2, (dat2$Count[which(dat2$Date == i)]-propdat2)**2)
  }
  
  product <- sum(multi)/(sum(squares1)*sum(squares2))**0.5
  
  return(product)
}

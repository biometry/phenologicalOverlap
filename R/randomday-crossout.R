#' Crosses out days randomly from abundance dataset(s)
#'
#' Crosses out days randomly from one or two abundance dataset
#'
#' crosses out random days completely from given abundance dataframe. if no second dataframe is given, it will cross out the dates only for one dataset. Otherwise it will cross out the same days for both datasets or in only one if said date is only present in one dataset.
#'   
#' @param data1 data.frame; an abundance dataset, with one column of date objects, and one column numeric values. 
#' @param data2 data.frame; an abundance dataset, with one column of date objects, and one column numeric values. Can also be NULL. 
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance. If FALSE the columns of data1 (and possibly data2) must be named Date and Count.
#' @param numberofdays numeric whole numbers; Determines the number of days that will be excluded from the dataset.  
#'
#' @return If dataset2 i NULL a data.frame like data1, but with excluded days. If data2 i a data.frame a list with two levels: 1 is data1 2 is data2 both with th same days (same dates) excluded.
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
#'  randomday.crossout(dat1, dat2, numberofdays = 1, CheckData = TRUE)
#' 
#' @export
#' 

randomday.crossout <- function(data1, data2=NULL, numberofdays = 1, CheckData = TRUE){
  # crosses out random days completely from given abundance dataframe. if no second 
  #   dataframe is given, it will cross out the dates only for one dataset. Otherwise 
  #   it will cross out the same days for both datasets.
  
  if(CheckData){
    data1 <- CheckData(data1)
    if(! is.null(data2)){
      data2 <- CheckData(data2)
    }
  }
  
  if(! is.null(data2)){
    xs <- sort(unique(c(data1$Date, data2$Date)))
  }else{
    xs <- data1$Date
  }
  print(xs)
  if(length(xs)<numberofdays){
    stop('There were more days to cross out, than actual dates')
  }
  
  

  
  
  
  takeout <- vector()
  
  for(i in c(1:numberofdays)){
    out <- floor(runif(n=1, min=min(xs), max = max(xs)+1))
    while(out %in% takeout |! out %in% xs){
      out <- floor(runif(n=1, min=min(xs), max = max(xs)+1))
    }
    takeout <- c(takeout, out)
  }
  
  if(! is.null(data2)){
    return(list( data1 = data1[-which(data1$Date %in% takeout),], data2 = data2[-which(data2$Date %in% takeout),]))
  }else{
    return(data1[-which(data1$Date %in% takeout),])
  }
}
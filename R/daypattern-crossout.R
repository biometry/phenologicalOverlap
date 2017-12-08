#' Crosses out days in a pattern from abundance dataset(s)
#'
#' Crosses out days in a pattern from one or two abundance dataset
#'
#' This method will cross out whole days in a pattern, meaning, that if stepsize is positive, every xth day will be crossed out, when its negative every xth day will be left in the dataset. If there is no second dataset given it will cross out the days for only one and return only one dataset otherwise it will cross out the exact same days. The startday for the pattern will be chosen randomly  in the interval of the first x days, where x is the value the stepsize argument is given.
#'   
#' @param data1 data.frame; an abundance dataset, with one column of date objects, and one column numeric values. 
#' @param data2 data.frame; an abundance dataset, with one column of date objects, and one column numeric values. Can also be NULL. 
#' @param CheckData logical; If TRUE the CheckData() function will be runned in advance. If FALSE the columns of data1 (and possibly data2) must be named Date and Count.
#' @param stepsize numeric (whole numbers); determines the pattern in which the dates shall be crossed out. If stepsize positive it will exclude every Xth day, if negative every Xth day will be included. 
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
#'  daypattern.crossout(dat1, data2 = dat2, stepsize = -2, CheckData = TRUE)
#' 
#' @export
#' 


daypattern.crossout <- function(data1, data2 = NULL, stepsize, CheckData = TRUE){
  
  # has abundance datasets as input, with two columns, one called dates with dates
  #   in it and one called count with numbers in it. This method will cross out whole
  #   days in a pattern, meaning, that if stepsize is positive, every xth day will be
  #   crossed out, when its negative every xth day will be left in the dataset. If
  #   there is no second dataset given it will cross out the days for only one and
  #   return only one dataset otherwise it will cross out the exact same days. The startday for the pattern will be chosen randomly 
  #   in the interval of the first x days, where x is the value the stepsize argument
  #   is given.
  
  if(stepsize == 0 | abs(stepsize) == 1){
    stop('Please give a stepsize unequal 0, 1 or -1, positive numbers will be that each stepsize days will be crossed out, negative will be that all other values are crossed out')
  }
  
  if(stepsize %% 1 != 0){
    stop('Please give a whole number for a stepsize')
  }
  
  if(CheckData){
    data1 <- CheckData(data1)
    
    if(! is.null(data2)){
      data2 <- CheckData(data2)
    }
  }
  
  crossout <- vector()
  stay     <- vector()

  if(! is.null(data2)){
    xs       <- sort(unique(c(data1$Date,data2$Date)))
  }else{
    xs <- sort(data1$Date)
  }

  xs <- as.numeric(xs)
  j <- floor(runif(n=1, min =min(xs), max=abs(stepsize)+1+min(xs))) #get random startpoint

  while(j < max(xs)){
    
    crossout <- c(crossout, j)
    
    j <- j + abs(stepsize)
    
  }
  
  
  
  if( stepsize > 0){
    data1 <- data1[-which(data1$Date %in% crossout),]
  }else{
    data1 <- data1[which(data1$Date %in% crossout),]
  }
  if(! is.null(data2)){
    if( stepsize > 0){
      data2 <- data2[-which(data2$Date %in% crossout),]
    }else{
      data2 <- data2[which(data2$Date %in% crossout),]
    }
    return(list(data1 = data1, data2=data2))
  }else{
    return(data1)
  }
}

#' checks input data for OLE.hurlbert()
#'
#' checks input data for OLE.hurlbert()
#'
#' This function checks if given data fits the requirenments for the OLE.hurlbert() function. Will interpolate the data if required.
#'   
#' @param data data-frame; It must have two columns, one wih date-objects one with numeric values.
#' @param interpolate logical; determines whether data should be interpolated or not.
#'
#' @return data.frame. based on input data.frame, may include new datapoints.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' a      <- c(1:10)
#' dates  <- as.Date(a, origin = '2017-01-01')
#' count  <- c(1,1,3,4,6,9,5,4,2,1)
#' dat    <- data.frame(dates, count)
#' newdat <- CheckHurlbert(dat)
#' 
#' 
#' @export
#' 


CheckHurlbert <- function(data, Interpolate = TRUE){
  
  #checks data for hurlbert function
  
  if(any(!c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date)))) %in% data$Date) & Interpolate == FALSE){
    stop('To calculate hurlberts PIE it is necessary that the Raw-Data has at least one datapoint for each Full day if Interpolate == FALSE')
  }
  
  if(any(!data$Date %in% c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date)))))){
    
    warning('To calculate hurlberts PIE only Data for Full Dates will be considered if your data shall be normalized')
    
    index <- which(data$Date %in% c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date)))))
    data  <- data[index,]
    
    if(Interpolate){
      
      not      <- FALSE
      newCount <- vector()
      newDate  <- vector()
      
      for(i in c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date))))){
        
        if(i %in% data$Date){
          
          if(not){
            end     <- i-1
            toadd   <- (data$Count[which(data$Date == end + 1)] - data$Count[which(data$Date == start - 1)])/abs((start-1)-(end+1))
            
            for(j in c(start:end)){
              newDate  <- c(newDate, j)
              newCount <- c(newCount, data$Count[which(data$Date == start - 1)]+((j - (start - 1))*toadd))
              
            }
            
            not <- FALSE
          }
          
          newDate  <- c(newDate, i)
          newCount <- c(newCount, data$Count[which(data$Date == i)])
        }else{
          
          if(not == FALSE){
            start <- i
          }
          
          not <- TRUE
          
        }
      }
      return(data.frame(Count = newCount, Date = newDate))
      
    }else{
      return(data)
    }
    
  }else{
    return(data)
  }
}
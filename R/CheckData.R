#' checks input data for pdf-estimation and some overlap-estimators
#'
#' checks input data for pdf-estimation and some overlap-estimators
#'
#' This function Checks if the data fits the requirements for the distribution estimation and for some of the Overlapmeasures As it Checks the data, the data argument can be everything, but it will only be returned again, if it's a dataframe with two columns, one with dates and one with numerics It has an Dataframe as outcome, which is exactly like the data that is given but the two columns are called Date and Count
#'   
#' @param data data-frame; Abundance dataset. It must have two columns, one wih date-objects one with numeric values.
#' @param normalize logical; determines, whether data will be normalized or if probabilities will be estimated in another way. Runs additional checks if TRUE.
#' @param interpolate logical; determines whether data should be interpolated later on or not. Will run additional Checks if TRUE. 
#'
#' @return data.frame. Same as data(input) but with renamed columns, one named Date (the one with date objects) and one named Count.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' a <- NormData(mean=0, sd =20 ,n=1000, FillUp = FALSE)
#' newa <- FillUp(a, NA)
#' 
#' @export

CheckData <- function(data, normalize = FALSE, Interpolate = TRUE){
  
  # This function Checks if the data fits the requirements for the distribution estimation
  # and for some of the Overlapmeasures
  # As it Checks the data, the data argument can be everything, but it will only be returned
  # again, if it's a dataframe with two columns, one with dates and one with numerics
  # It has an Dataframe as outcome, which is exactly like the data that is given but the two
  # columns are called Date and Count
  
  
  minimumlength <- 10 #The minimum length is set to ten for no reason
  nodates       <- vector()
  
  if(!is.data.frame(data)){
    stop('Please make sure, that your data is a dataframe')
  }
  
  if(length(data) != 2){
    stop('Please make sure, that your data has two columns')
  }
  
  # if(length(data[,1]) < minimumlength){
  #   stop(paste0('Please make sure, that you have at least ', minimumlength,' datapoints'))
  # }
  
  for(i in c(1,2)){
    if(any(!(inherits(data[,i], 'Date')))){
      nodates <- c(nodates,i)
    }
  }
  
  if(length(nodates) > 1 | length(nodates) == 0){
    stop('Please make sure that one column contains only Dates')
  }
  
  if(nodates == 1){
    dates <- 2
  }
  
  if(nodates == 2){
    dates <- 1
  }
  
  #go through the nodates column and give exact row where the data does not fit
  for( j in c(1:length(data[,nodates]))){
    if(!(is.numeric(data[j,nodates])) & !(is.na(data[j,nodates])) & !(is.null(data[j,nodates]))){
      stop(paste0('The data in column: ', nodates, 'in line: ', j,' is neither numeric nor an NA nor null. Please make sure that this column only consists of these datatypes' ))
    }
  }
  
  data <-   data.frame(Date = data[,dates], Count = data[,nodates])
  
  if(normalize){
    
    if(any(!c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date)))) %in% data$Date) & Interpolate == FALSE){
      stop('To calculate the overlap it is necessary that the Raw-Data has at least one datapoint for each Full day if Interpolate == FALSE')
    }
    
    if(any(!data$Date %in% c(ceiling(as.numeric(min(data$Date))):floor(as.numeric(max(data$Date)))))){
      warning('To calculate the overlap only Data for Full Dates will be considered if your data shall be normalized')
      
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
  }else{
    return(data)
  }
  
}
#' Fills up abundance data-set
#'
#' Fills up not specified data-points (whole days) in an abundance dataset.
#'
#' will fill up a abundance dataset, meaning that it will add the value given by the Fillup argument for all the dates, that are not existing in the dataset given by the Data argument. Only dates that are in between the minimum and the maximum of the dates in Data. the input data needs to have two columns, one named dates which should be filled with dates and count.
#'   
#' @param data data-frame; Abundance dataset. It must have two columns, one wih date-objects one with numeric values.
#' @param FillUp numeric; This will be what will be filled up. for example 0, NA etc.
#'
#' @return data.frame. Same as data(input) but with filled up not specified dates.
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

FillUp <- function(Data, Fillup = 0){
  
  #   will fill up a abundance dataset, meaning that it will add the value
  #   given by the Fillup argument for all the dates, that are not existing
  #   in the dataset given by the Data argument. Only dates that are in
  #   between the minimum and the maximum of the dates in Data.
  #   the input data needs to have two columns, one named dates which should
  #   be filled with dates and count.
  
  Date <- Data$Date
  Count <- Data$Count
  
  for(z in c(1:(length(Date)-1))){ #Go throuh all the indexes of Data$Date and so also the indexes of Data$Count
    i <- (length(Data$Date))-z # Reverse z, Here i take data$Date because that does not change
    if(as.numeric(Date[i+1]-Date[i]) > 1){ #check if there is a gap biger than one day in the Dates
      for(j in c(2:as.numeric(Date[i+1]-Date[i])-1)){ #if so fill them up
        Date <- append(Date, Data$Date[i+1] - j, after = i ) #Here Data$Date is also used, because it doesn't change it's indexes
        Count <- append(Count, Fillup, after = i)
      }
    }
  }
  return(data.frame(Date,Count))
}



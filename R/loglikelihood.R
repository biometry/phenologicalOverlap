#' Calculates the loglikelihood for an abundance datesset and a given function
#'
#' Calculates the loglikelihood for an abundance datesset and a given function 
#'
#' Calculates the loglikelihood for an abundance datesset and a given function by multiplying all the values given by the given function, that uses the dates given by dates.
#' 
#' @param func function object; a given function, for whih the loglikelihood shall be calculated.
#' @param dates vector;  dates, that should be in the range of the function.
#' @param allowDistrBorders logical; If TRUE the first and last values of the dates vector will b changed by 0.00001 (first) or -0.00001(last) if the corresponding result of the function is Inf or -Inf.
#' 
#' @return A numeric value representing the loglikelihood.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' a     <- c(1:10)
#' dates <- as.Date(a, origin = '2017-01-01')
#' count <- c(1,1,3,4,6,9,5,4,2,1)
#' dat   <- data.frame(dates, count)
#' loglikelihood(dates = dates, func = estdist(dat))
#' 
#' @export

loglikelihood <- function(dates, func, allowDistrBorders = FALSE){
  
  #Gets the loglikelihood of a function and a datesset. The dates shall be a single vector.
  # if allowDistrBorders = TRUE the first and last datespoints given by dates will be changed slightly
  # if the function returns -Inf or Inf for them.
  
  
  #create a vector with the propabillities of all observed dates
  probs  <- vector()
  startvalues <- TRUE
  for(i in c(1:length(dates))){
    #checks whether dates[i] is in a row (of same values) of the starting value
    if(allowDistrBorders ==TRUE){
      if(dates[i] == dates[1] & startvalues == TRUE){
        if(func(dates[i]) == 0 | func(dates[i]) == Inf | func(dates[i]) == -Inf ){
          probs[i] <- func(dates[i]+0.00001)
          
        }
      }else{
        startvalues == FALSE
      }
      if(!(any(c(i:length(dates) %in% which(dates == dates[length(dates)])) == FALSE))){ #checks whether dates[i] is in a row (of same values) of the last value
        if(func(dates[i]) == 0 | func(dates[i]) == Inf | func(dates[i]) == -Inf ){
          probs[i] <- func(dates[i]-0.00001)
        }
      }
    }
    if(length(probs) == i-1){
      probs[i] <- func(dates[i])
    }
  }
  
  #make a product of all that propabilities
  likelihood <- probs[1]
  zeroavoidance <- vector()
  for(j in probs[-1]){
    if(likelihood * j == 0){
      zeroavoidance <- c(zeroavoidance, log(likelihood, base = 10))
      likelihood    <- 1 #because than the next calculation step will have j as result and so no probabillity is used twice
    }
    likelihood <- likelihood * j
  }
  
  #return the logarythm of base ten of that product
  return(log(likelihood, base = 10) + sum(zeroavoidance))
}



 
#' Fits a Distribution to abundance data
#'
#' Fits a beta and a truncated normal distribution to the abundance data.
#'
#' This function will estimate a probability density function for given time range, it takes abundance data as input. There will be a truncated normal distribution and a beta distribution fitted and their loglikelihood will be compared, and the best fitting distribution will be returned, along with its parameters and the start and end date. This function uses the 'truncnorm','stats' and the 'MASS' Package. 
#'
#' @param data data-frame; This is the data the distributions shall be fitted for. It must have two columns, one wih date-objects one with numeric values.
#' @param StartAndEnd vector; In this vector shall be two date-objects, defining the range, in which the distributions shall be fitted. As default the first and last dates in the data data-frame will be selected.
#' @param returnonlyfunction logical; If TRUE, the function will return only the Function-Object of the Results, if FALSE it will return a list with the function, distibution parameters and the used start and enddates. Default is TRUE.
#' @param forcedistrtype string; Can either be 'normal' for the truncated normal distribution or 'beta' for beta distribution. This will force the function to only consider one of those distributions for the calculation.
#' @param CheckData logical; If TRUE the function will check the data input whether it fits the requirements using the CheckData function. If FALSE it will process without, which might result in wrong results so to use the default value (TRUE) is strongly recommended.
#'
#' @return a list with 5 levels: $func = fitted distribution as function object, $estimationmethod = 'fitted distribution', $distributiontype = name of the distribution used to create the func level, $startpoint = selected startpoint, $endpoint = selected endpoint.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' a     <- c(1:10)
#' dates <- as.Date(a, origin = '2017-01-01')
#' count <- c(1,1,3,4,6,9,5,4,2,1)
#' dat   <- data.frame(dates, count)
#' estdist(dat, returnonlyfunction = FALSE)
#' 
#' @export

estdist <- function(data, StartAndEnd = NULL, returnonlyfunction = TRUE, forcedistrtype = NULL, CheckData = TRUE){
  
  #   This function will estimate a probability density function, it takes abundance
  #   data as input, which are dataframes, that have two columns, one with dates, one
  #   with numbers. There will be a truncated normal distribution and a beta 
  #   distribution fitted and their loglikelihood will be compared, and the best 
  #   fitting distribution will be returned, along with its parameters and the start
  #   and end date. These start and enddates will be determined by the first and last 
  #   date that can be found in the abundance data, or can be set manually by the 
  #   StartAndEnddate argument. The forcedistrtype argument can force the function to 
  #   fit only one of the distributions.  If returnonlyfunction = TRUE it will return 
  #   only the function.
  
  if(!is.null(forcedistrtype)){
    
    if(!forcedistrtype %in% c('normal','beta')){
      stop('only normal or beta can be given for forcedistrtype or NULL as default')
    }
  }
  
  
  PackageCheck('MASS')
  PackageCheck('stats')
  
  #check if StartAndEnd is in right format
  if(!(is.null(StartAndEnd))){
    
    if(any(!(inherits(StartAndEnd, 'Date')))){
      stop('StartAndEnd values need to be dates')
    }
    
    if(length(StartAndEnd) != 2 ){
      stop('StartAndEnd needs to contain two dates')
    }
    
  }
  
  if(CheckData){
    data <- CheckData(data)
  }
  
  #creates a vector containing the dates as often as the Count value is on that day
  newCount     <- CountsToDrawing(data)
  
  # get the default values for StartAndEnd
  if(is.null(StartAndEnd)){
    StartAndEnd <- as.Date(c(min(newCount),max(newCount)), origin = '1970-01-01')
    
  }
  
  StartAndEnd  <- as.numeric(StartAndEnd)
  
  
  
  #remove datapoints that are not in the range of StartAndEnd if there are some
  if(min(newCount) < min(StartAndEnd) | max(newCount) > max(StartAndEnd)){
    newCount <- newCount[which(newCount %in% c(min(StartAndEnd):max(StartAndEnd)))]
  }
  
  #create datapoints, for beta distribution, those need to be betwenn 0 and 1
  newCountBeta <- vector()
  
  newCountBeta <- (newCount - min(StartAndEnd))/(max(StartAndEnd) - min(StartAndEnd))
  
  betaTrunk    <- (newCountBeta[length(newCountBeta)-1]+0.5)/length(newCountBeta) #values how close the values of the data for the estimation for the beta distribution should be to one and zero if needed
  
  if(any(newCountBeta == 0)){
    newCountBeta[which(newCountBeta == 0)] <-newCountBeta[which(newCountBeta == 0)] + betaTrunk
  }
  if(any(newCountBeta == 1)){
    newCountBeta[which(newCountBeta == 1)] <-newCountBeta[which(newCountBeta == 1)] - betaTrunk
  }
  
  
  #create the estimated functions 
  
  func      <- vector()
  names     <- vector()
  parms     <- list()
  parmnames <- list()
  LL        <- vector()
  
  #normal distribution
  getfunc <- FALSE
  if(!is.null(forcedistrtype)){
    if(forcedistrtype == 'normal'){
      getfunc <- TRUE
    }
  }else{
    getfunc <- TRUE
  }
  if(getfunc  == TRUE){
    PackageCheck('truncnorm')
    
    norm <- tryCatch(fitdistr(newCount, truncnorm::dtruncnorm, start = list(mean = wmd(data), sd = sd(newCount)), a = min(StartAndEnd), b = max(StartAndEnd)), error=function(d){return('F')})
    
    if(norm[1] != 'F'){
      
      mean       <- norm$estimate[1]
      sd         <- norm$estimate[2]
      names[1]   <- 'truncnormal'
      parms[[1]] <- list(mean = mean, sd = sd)
      parmnames[[1]]  <- c('mean', 'sd')
      func       <- c(function(x) dtruncnorm(x, mean = mean, sd = sd, a = min(StartAndEnd), b = max(StartAndEnd)))
    }
  }
  
  # beta distribution
  
  getfunc <- FALSE
  if(!is.null(forcedistrtype)){
    if(forcedistrtype == 'beta'){
      getfunc <- TRUE
    }
  }else{
    getfunc <- TRUE
  }
  
  if(getfunc  == TRUE){
    
    beta       <- fitdistr(newCountBeta, 'beta', start = list(shape1=0.5, shape2=0.5) )
    shape1     <- beta$estimate[[1]]
    shape2     <- beta$estimate[[2]]
    names      <- c(names,'beta')
    parms[[length(parms)+1]] <- list(shape1 = shape1, shape2 = shape2)
    parmnames[[length(parmnames)+1]]  <- list('shape1','shape2')
    
   print(StartAndEnd)
    
    #the term in surrounding x stretches the beta-distribution, so that it fits the whole range of the Dates
    func   <- c(func,function(x) dbeta((x-min(StartAndEnd))/(max(StartAndEnd) - min(StartAndEnd)), shape1 = shape1, shape2 = shape2)/(max(StartAndEnd)-min(StartAndEnd)))
  }
  
  for(k in func){
    LL <- c(LL, loglikelihood(newCount, as.function(k), allowDistrBorders = TRUE))
  }
  
  index <- which(LL == max(LL))
  
  
  if(returnonlyfunction == FALSE){
    ret <- list(func = as.function(func[[index]]), estimationmethod = 'fitted distribution', distributiontype = names[index], startpoint = min(StartAndEnd), endpoint = max(StartAndEnd))
    for(i in c(1:length(parms[[index]]))){
      ret <- c(ret,parms[[index]][i])
      names(ret)[length(ret)] <- parmnames[[index]][i]
    }
    return(ret)
  }else{
    return(as.function(func[[index]]))
  }
}


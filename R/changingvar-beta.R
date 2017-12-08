#' simulates abundance data and calculates overlap for changing variances
#'
#' simulates abundance data and calculates overlap for changing variances
#'
#' Will determine the overlap-measures given by OLest for two simulated datasets both coming from a beta distribution with the sample size given by n. This will be done for every number of days given by days which determines the time-range over which the dataset spreads. And for every number of day there will be as many overlaps will be determined as the repeatings argument says. 
#'
#' @param days vector of numerics (whole numbers); determines the length of the time range over which the data shall stretch. 
#' @param repeatings numeric (whole numbers); Determines how often the whole procedure (data producing and calculation of the overlap) shall be repeated. 
#' @param n numeric (whole number); Determines the sample size for creating the datasets. Will be passed as as 'n' argument to to the NormData() function. 
#' 
#' 
#' @return a list. First index level determines the way of estimating the pdf so it has the three indexes: 'kernel', 'fitdistr' and 'normalize'. The second index-level is the chosen numbers of days and the last the number of the repeating. In there are the results as returned by OLE function. To illustrate: list[[kernel]][[numbers of days]][[repeating]].
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#' 
#' @export

changingvar.beta <- function(repeatings = 100, days = c(730,680,550,365,180,90,7) , n = 1000){
  
  #implementing arguments for the parameters of beta distribution might be helpful.
  time.start <- Sys.time()
  calc       <- 1
  days       <- sort(days, decreasing = T)
  OL         <- list('kernel'= rep(list(list()), length(days)), 'fitdistr'=rep(list(list()), length(days)), 'normalize'= rep(list(list()), length(days)))
  
  
  for(dis in c('kernel','fitdistr','normalize')){
    OL[[as.character(dis)]][[1]] <- rep(list(data.frame()), repeatings ) 
  }
  
  first1  <- TRUE
  
  for(i in c(1:repeatings)){
    
    listing    <- 1
    
    if(first1){
      first      <- TRUE
    }
    
    for(day in days){
      
      if(first){
        for(dis in c('kernel','fitdistr','normalize')){
          OL[[as.character(dis)]][[listing]] <- rep(list(data.frame()), repeatings)
        }
        first <- FALSE
      }
      start <- as.numeric(as.Date('2017-01-01'))
      end <- start  + day
      
      a1 <- BetaData(n=n, shape1 = 1, shape2 = 10, startandend = c(as.Date(c(start,end),origin = '1970-01-01')))
      b1 <- BetaData(n=n, shape1 = 1, shape2 = 10, startandend = c(as.Date(c(start,end),origin = '1970-01-01')))
      
      
      for(dis in c('kernel','fitdistr','normalize')){
        OL[[as.character(dis)]][[listing]][[i]] <- OLE(a1,b1, distrest = dis, OLest = c('Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Duration', 'WMD', 'Horn'))$Overlap
        print(paste0('finished: ', calc/((length(days))*repeatings*3) *100, '%'))
        calc <- calc +1
      }
      
      
      listing <- listing + 1
      
      
    }
    if(first1){
      time.end <- Sys.time()
      print(paste0('estimated time of ending: ', Sys.time() + (repeatings-1) * (time.end - time.start) ))
      first1 <- FALSE
    }
  }
  for(dis in c('kernel','fitdistr','normalize')){
    names(OL[[as.character(dis)]]) <- days
  }
  return(OL)
  
  
}
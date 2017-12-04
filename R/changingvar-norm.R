#' simulates abundance data and calculates overlap for changing variances
#'
#' simulates abundance data and calculates overlap for changing variances
#'
#' #Will determine the overlap-measures given by OLest for two simulated datasets both coming from a normla distribution with the sample size given by n. This will be done for every standard deviation given by sds. And ffor every standard deviation as many overlaps will be determined as the repeatings argument says. 
#'
#' @param sds vector of numerics; Those will be the standard deviations of the noemal distributions which is used to create the datasets.
#' @param repeatings numeric (whole numbers); Determines how often the whole procedure (data producing and calculation of the overlap) shall be repeated. 
#' @param OLest vector of strings; determines in which way(s) the overlap shall be determined, will be passed to OLE function.
#' @param n numeric (whole number); Determines the sample size for creating the datasets. Will be passed as as 'n' argument to to the NormData() function. 
#' 
#' 
#' @return a list. First index level determines the way of estimating the pdf so it has the three indexes: 'kernel', 'fitdistr' and 'normalize'. The second index-level is the chosen standard deviation and the last the number of the repeating. In there are the results as returned by OLE function. To illustrate: list[[kernel]][[standard deviation]][[repeating]].
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' changingvar.norm(sds=c(5,12), n = 500, repeatings = 2)
#' 
#' @export

changingvar.norm <- function(repeatings = 100, sds = c(112, 104,  84,  57,  27,  13,   1), n = 1000, OLest = c('Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Duration', 'WMD', 'Horn','n')){
  
  #Will determine the overlap-measures given by OLest for two simulated datasets both coming from a normla distribution with the sample size given by n. This will be done for every standard deviation given by sds. And ffor every standard deviation as many overlaps will be determined as the repeatings argument says. 
  
  time.start <- Sys.time()
  calc       <- 1
  sds       <- sort(sds, decreasing = T)
  OL         <- list('kernel'= rep(list(list()), length(sds)), 'fitdistr'=rep(list(list()), length(sds)), 'normalize'= rep(list(list()), length(sds)))
  
  
  for(dis in c('kernel','fitdistr','normalize')){
    OL[[as.character(dis)]][[1]] <- rep(list(data.frame()), repeatings ) 
  }
  
  first1  <- TRUE
  
  for(i in c(1:repeatings)){
    
    listing    <- 1
    
    if(first1){
      first      <- TRUE
    }
    
    for(var in sds){
      
      if(first){
        for(dis in c('kernel','fitdistr','normalize')){
          OL[[as.character(dis)]][[listing]] <- rep(list(data.frame()), repeatings)
        }
        first <- FALSE
      }
      
      
      a1 <- NormData(n=n, sd = var, mean = 0)
      b1 <- NormData(n=n, sd = var, mean = 0)
      
      
      for(dis in c('kernel','fitdistr','normalize')){
        OL[[as.character(dis)]][[listing]][[i]] <- OLE(a1,b1, distrest = dis, OLest = OLest)$Overlap
        print(paste0('finished: ', calc/((length(sds))*repeatings*3) *100, '%'))
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
    names(OL[[as.character(dis)]]) <- sds
  }
  return(OL)
  
  
}

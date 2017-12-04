#' simulates abundance data and calculates overlap for changing sample sizes
#'
#' simulates abundance data and calculates overlap for changing sample sizes(crossing out days in patterns)
#'
#' This function produces abundance data with the sample size :start.n. This data can be drawn either from a normal (sigma = 20) or a beta distribution (shape1 =1, shape2 =5 and a spread over 130 days), depending on the type argument. afterwards there is an Overlap estimation made for this data. Then the data will be modified by the daypattern.crossout function, and the Overlap will be estimated again. If equal = TRUE the same days will be crossed out, if it is FALSE there will be different days crossed out for both datasets. The original dataset, will be the basis for all the modifications, that are chosen with stepsizes argument. This process will be repeated as many times as the reapeatings argument says. The Overlap will be determined in three different ways, with the kernel density estimatiion with fitted distributions and with 'normalization' done using the functions estdist, kernelest and normalize. A listt will be returned, that has 3 lists in it, one for the kernel ddensity estimation one for fitted distributions etc. This lists have a list stored in them for each stepsize. Those lists contain all the results of the estimations for the respective sample size and way the overlap was determined. 
#'
#' @param stepsizes vector of numeric (whole numbers); will determined which patterns will be used to cross out data. Will be passed to daypattern.crossout() function. 
#' @param repeatings numeric (whole numbers); Determines how often the whole procedure (data producing and calculation of the overlap) shall be repeated. 
#' @param type string; If set to beta the abundance data will bbe drawn from a beta distribution. If not beta this will be from a normal distribution.
#' @param OLest vector of strings; determines in which way(s) the overlap shall be determined, will be passed to OLE function.
#' @param start.n numeric (whole number); Determines the start sample size for creating the datasets. Will be passed as as 'n' argument to either BetaData() function (if type = 'beta') or to the NormData() function. 
#' @param equal logical; if TRUE the same days (dates) will be crossed out from dataset, if FALSE there will be different days crossed out.
#' 
#' 
#' @return a list. First index level determines the way of estimating the pdf so it has the three indexes: 'kernel', 'fitdistr' and 'normalize'. The second index-level is the sample size and the last the number of the repeating. In there are the results as returned by OLE function. To illustrate: list[[kernel]][[sample-size]][[repeating]].
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' changingn.daypattern(stepsizes = c(4,-3), start.n = 500, repeatings = 2, type = 'normal', equal = TRUE)
#' 
#' @export
changingn.daypattern <- function(stepsizes = c(2,-4,-7,-14,-19,-22), repeatings = 100, start.n = 1000, type= 'beta', equal = FALSE, OLest = c('Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Duration', 'WMD', 'Horn','n')){
  
  # This function produces abundance data with the sample size :start.n. This data can be drawn either from a normal (sigma = 20) or a beta distribution (shape1 =1, shape2 =5 and a spread over 130 days), depending on the type argument. afterwards there is an Overlap estimation made for this data. Then the data will be modified by the daypattern.crossout function, and the Overlap will be estimated again. If equal = TRUE the same days will be crossed out, if it is FALSE there will be different days crossed out for both datasets. The original dataset, will be the basis for all the modifications, that are chosen with stepsizes argument. This process will be repeated as many times as the reapeatings argument says. The Overlap will be determined in three different ways, with the kernel density estimatiion with fitted distributions and with 'normalization' done using the functions estdist, kernelest and normalize. A listt will be returned, that has 3 lists in it, one for the kernel ddensity estimation one for fitted distributions etc. This lists have a list stored in them for each stepsize. Those lists contain all the results of the estimations for the respective sample size and way the overlap was determined.  
  
  time.start <- Sys.time()
  calc       <- 1
  stepsizes  <- sort(stepsizes, decreasing = T)
  
  #create the basis for the list that is to be returned
  OL         <- list('kernel'= rep(list(list()), length(stepsizes)+1), 'fitdistr'=rep(list(list()), length(stepsizes)+1), 'normalize'= rep(list(list()), length(stepsizes)+1))
  
  
  for(dis in c('kernel','fitdistr','normalize')){
    OL[[as.character(dis)]][[1]] <- rep(list(data.frame()), repeatings ) 
  }
  
  first1  <- TRUE
  
  
  #get the datasets
  
  for(i in c(1:repeatings)){
    
    if(type == 'beta'){
      start <- as.Date('2017-01-01')
      end <- start + 130
      a <- BetaData(n=start.n, shape1 = 1, shape2 = 5, startandend = c(start, end))
      b <- BetaData(n=start.n, shape1 = 1, shape2 = 5, startandend = c(start, end))
      
    }else{
      a <- NormData(mean = 0, sd=20, n=start.n)
      b <- NormData(mean = 0, sd=20, n=start.n)
    }
    
    #determine the Overlap measures
    
    for(dis in c('kernel','fitdistr','normalize')){
      OL[[as.character(dis)]][[1]][[i]] <- OLE(a,b, distrest = dis, OLest = OLest)$Overlap
      print(paste0('finished: ', calc/((length(stepsizes)+1)*repeatings*3) *100, '%'))
      calc <- calc +1
    }
    
    listing    <- 2
    
    if(first1){
      first      <- TRUE
    }
    
    for(step in stepsizes){
      
      if(first){
        for(dis in c('kernel','fitdistr','normalize')){
          OL[[as.character(dis)]][[listing]] <- rep(list(data.frame()), repeatings)
        }
        first <- FALSE
      }
      
      
      #reduce sample size
      
      
      if(equal){
        a2 <- daypattern.crossout(a,b, stepsize = step, CheckData = FALSE)
        
        a1 <- a2$data1
        b1 <- a2$data2
      }else{
        a1 <- daypattern.crossout(a, stepsize = step, CheckData = FALSE)
        b1 <- daypattern.crossout(b, stepsize = step, CheckData = FALSE)
      }
      
      #determine overlap
      
      for(dis in c('kernel','fitdistr','normalize')){
        OL[[as.character(dis)]][[listing]][[i]] <- OLE(a1,b1, distrest = dis, OLest = OLest)$Overlap
        print(paste0('finished: ', calc/((length(stepsizes)+1)*repeatings*3) *100, '%'))
        calc <- calc +1
      }
      
      
      listing <- listing + 1
      
      
    }
    
    # give time estimation
    if(first1){
      time.end <- Sys.time()
      print(paste0('estimated time of ending: ', Sys.time() + (repeatings-1) * (time.end - time.start) ))
      first1 <- FALSE
    }
  }
  for(dis in c('kernel','fitdistr','normalize')){
    names(OL[[as.character(dis)]]) <- c(0,stepsizes)
  }
  return(OL)
  
  
}

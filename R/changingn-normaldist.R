#' simulates abundance data and calculates overlap for changing sample sizes
#'
#' simulates abundance data and calculates overlap for changing sample sizes
#'
#' Produces two abundance datastes and dteremines the overlpa-measures given with OLest betweeen these. The overlap will be determined using three different methods : kernel density estimation, fitting a distribution or normalization (normalize function). The sample size will be set to the maximum value of ns. The type argument determinnes whether these data shall be created using a beta or a normal distribution. Then the sample size will be decreased, using the normaldist.crossout function, to  the next lower value of ns, and the overlap will be determined again. This will be repeated, until an overlap for all the ns was calculated. Afterwards This procedure itself will be repeated as many times as repeatings says. A listt will be returned, that has 3 lists in it, one for the kernel ddensity estimation one for fitted distributions etc. This lists have a list stored in them for each stepsize. Those lists contain all the results of the estimations for the respective sample size and way the overlap was determined. 
#'
#' @param ns vector of numeric objects (whole numbers); the sample sizes for which the overlap will be determined. Applies for both datasets.
#' @param repeatings numeric (whole numbers); Determines how often the whole procedure (data producing and calculation of the overlap) shall be repeated. 
#' @param type string; If set to beta the abundance data will bbe drawn from a beta distribution. If not beta this will be from a normal distribution.
#' @param OLest vector of strings; determines in which way(s) the overlap shall be determined, will be passed to OLE function.
#' 
#' @return a list. First index level determines the way of estimating the pdf so it has the three indexes: 'kernel', 'fitdistr' and 'normalize'. The second index-level is the sample size and the last the number of the repeating. In there are the results as returned by OLE function. To illustrate: list[[kernel]][[sample-size]][[repeating]]
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' changingn.normaldist(ns=c(300,50), repeatings = 2, type = 'normal')
#' 
#' @export

changingn.normaldist <- function(ns = c(1000,500,200,100,50,20,10), repeatings = 100, type= 'beta', OLest = c('Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Duration', 'WMD', 'Horn','n')){
  
  # Produces two abundance datastes and dteremines the overlpa-measures given with OLest betweeen these. The overlap will be determined using three different methods : kernel density estimation, fitting a distribution or normalization (normalize function). The sample size will be set to tthe maximum value of ns. The type argument determinnes whether these data shall be created using a beta or a normal distribution. Then the sample size will be decreased, using the normaldist.crossout function, to  the next lower value of ns, and the overlap will be determined again. This will be repeated, until an overlap for all the ns was calculated. Afterwards This procedure itself will be repeated as many times as repeatings says. A listt will be returned, that has 3 lists in it, one for the kernel ddensity estimation one for fitted distributions etc. This lists have a list stored in them for each stepsize. Those lists contain all the results of the estimations for the respective sample size and way the overlap was determined. 
  
  time.start <- Sys.time()
  calc    <- 1
  ns      <- sort(ns, decreasing = T)
  start.n <- ns[1]
  OL      <- list('kernel'= rep(list(list()), length(ns)), 'fitdistr'=rep(list(list()), length(ns)), 'normalize'= rep(list(list()), length(ns)))
  
  
  for(dis in c('kernel','fitdistr','normalize')){
    OL[[as.character(dis)]][[1]] <- rep(list(data.frame()), repeatings ) 
  }
  
  first1  <- TRUE
  
  for(i in c(1:repeatings)){
    
    if(type=='beta'){
      start <- as.Date('2017-01-01')
      end <- start + 250
      a <- BetaData(n=start.n, shape1 = 1, shape2 = 10, startandend = c(start, end))
      b <- BetaData(n=start.n, shape1 = 1, shape2 = 10, startandend = c(start, end))
      
    }else{
      a <- NormData(mean = 0, sd=20, n=start.n)
      b <- NormData(mean = 0, sd=20, n=start.n)
    }
    
    for(dis in c('kernel','fitdistr','normalize')){
      OL[[as.character(dis)]][[1]][[i]] <- OLE(a,b, distrest = dis, OLest = OLest)$Overlap
      print(paste0('finished: ', calc/(length(ns)*repeatings*3) *100, '%'))
      calc <- calc +1
    }
    
    listing    <- 2
    
    if(first1){
      first      <- TRUE
    }
    
    for(n in ns[-which(ns == start.n)]){
      
      if(first){
        for(dis in c('kernel','fitdistr','normalize')){
          OL[[as.character(dis)]][[listing]] <- rep(list(data.frame()), repeatings)
        }
        first <- FALSE
      }
      
      red1 <- sum(a$Count) - n
      red2 <- sum(b$Count) - n
      
      if(type == 'beta'){
        a <- betadist.crossout(a, shape1 = 1, shape2 = 10, reduction = red1, DataCheck = FALSE, start = start, end = end)
        b <- betadist.crossout(b, shape1 = 1, shape2 = 10, reduction = red1, DataCheck = FALSE, start = start, end = end)
        
      }else{
        a <- normaldist.crossout(a, mean=0, sd=20, reduction = red1, DataCheck = FALSE)
        b <- normaldist.crossout(b, mean=0, sd=20, reduction = red2, DataCheck = FALSE)
      }
      
      for(dis in c('kernel','fitdistr','normalize')){
        OL[[as.character(dis)]][[listing]][[i]] <- OLE(a,b, distrest = dis, OLest = OLest)$Overlap
        print(paste0('finished: ', calc/(length(ns)*repeatings*3) *100, '%'))
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
    names(OL[[as.character(dis)]]) <- ns
  }
  return(OL)
  
}

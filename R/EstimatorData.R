#' checks and converts input data for Overlap-Estimators
#'
#' checks and converts input data for Overlap-Estimators
#'
#' provides the data used to calculate the overlap measure. It can handle dataframes and functions. First it determines , whether the data provided is a function or a dataframe. After this is done, it checks whether those functions are structured adequately for further calculation purposes (this is done by CheckProbData() or CheckProbFunc().) Those function make sure There are three possible initial combinations for data. Both given datasets are functions, only one is a function, or both are dataframes. If both are functions, it will create two dataframes, with equally distributed datapoints. To calculate the overlap it is necessary that the x values for the overlaprange are the same, which makes it impossible to give one specific number of datapoints for each particular functions, so the sum of all the x-values (of both functions) is twice the number of funcdatapoints. Those x-values are distributed equally on the range of both  functions. In the end those x-values will be cut to the range needed, so either the range in which both functions overlap (onlyOverap = TRUE) or the range of the respective function. To create the dataframe, those x values are combined with the belonging y-values of the given functions. The interpolate argument does not affect this type of calculation. If only one of the given data is a function (and the other accordingly a dataframe), and Interpolate is set FALSE, it will return the given dataframe and  create one, with one x-value for every day in the range of the function. If interpolate = TRUE it will do the same as for two functions, and get the missing  datapoints in the dataframe by interpolating linearly. If there are two dataframes and Interpolate is set FALSE it will return the dataframes with  the datapoints that both dataframes share in the overlaprange. If it is set TRUE it will get both dataframes to the same x- values by interpolating linearly. 
#'   
#' @param data1 list: like returned by either the estdist() and uniform()functions or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform()functions or by the kernelest() and normalize() function.
#' @param mindatapoints numeric(whole numbers); the minimum number of datapoints that both data share.
#' @param interpolate logical; determines whether data should be interpolated linearly if one datapoin is missing in one dataset.
#' @param funcdatapoints numeric (whole numbers); determines how much datapoints shall be drawn from functions. only rough guideline, results may vary. 
#' @param onlyOverlap logical; determines, whether the whole timerange shall be returned or just the time-range of the actual overlap-
#'
#' @return list of two data.frames. Those represent the pdfs. Used to determine overlap.
#' 
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#'  a     <- c(1:10)
#'  dates1 <- as.Date(a, origin = '2017-01-01')
#'  count1 <- c(1,1,3,4,6,9,5,4,2,1)
#'  dat1   <- data.frame(dates1, count1)
#'  b     <- c(6:15)
#'  dates2 <- as.Date(b, origin = '2017-01-01')
#'  count2 <- c(1,2,4,5,6,9,3,4,1,1)
#'  dat2   <- data.frame(dates2, count2)
#'  EstimatorData(kernelest(dat1), estdist(dat2, returnonlyfunction = FALSE), Interpolate = TRUE, funcdatapoints = 50)

#' 
#' @export



EstimatorData <- function(data1, data2, mindatapoints=20, funcdatapoints=NULL, Interpolate=FALSE, onlyOverlap = FALSE){
  
  #   provides the data used to calculate the overlap measure. It can handle dataframes and functions.
  #   First it determines , whether the data provided is a function or a dataframe. After this is done,
  #   it checks whether those functions are structured adequately for further calculation purposes (this
  #   is done by CheckProbData() or CheckProbFunc().) Those function make sure There are three possible initial combinations for
  #   data. Both given datasets are functions, only one is a function, or both are dataframes.
  #   If both are functions, it will create two dataframes, with equally distributed datapoints.
  #   To calculate the overlap it is necessary that the x values for the overlaprange are the same, which
  #   makes it impossible to give one specific number of datapoints for each particular functions, so the
  #   sum of all the x-values (of both functions) is twice the number of funcdatapoints. Those x-values are
  #   distributed equally on the range of both  functions. In the end those x-values will be cut to the range
  #   needed, so either the range in which both functions overlap (onlyOverap = TRUE) or the range of the
  #   respective function. To create the dataframe, those x values are combined with the belonging y-values
  #   of the given functions. The interpolate argument does not affect this type of calculation.
  #   If only one of the given data is a function (and the other accordingly a dataframe), and Interpolate
  #   is set FALSE, it will return the given dataframe and  create one, with one x-value for every day in the
  #   range of the function. If interpolate = TRUE it will do the same as for two functions, and get the missing 
  #   datapoints in the dataframe by interpolating linearly.
  #   If there are two dataframes and Interpolate is set FALSE it will return the dataframes with  the datapoints
  #   that both dataframes share in the overlaprange. If it is set TRUE it will get both dataframes to the same
  #   x- values by interpolating linearly. 
  
  if(is.null(funcdatapoints)){
    funcdatapoints <- 2500
  }
  
  if(funcdatapoints < 0 | mindatapoints < 0){
    stop('the number of datapoints you give must be positive')
  }
  
  if(funcdatapoints %% 1 != 0 | mindatapoints %% 1 != 0){
    stop('the number of datapoints you give must be a whole number')
  }
  
  d1func <- FALSE
  d2func <- FALSE
  
  #determin whether one of the given datasets includes a function
  for(i in c(1:length(data1))){
    if(is.function(data1[[i]])){
      d1func <- TRUE
    }
  }
  for(i in c(1:length(data2))){
    if(is.function(data2[[i]])){
      d2func <- TRUE
    }
  }
  
  if(d1func == FALSE){
    data1    <- CheckProbData(data1, Interpolate = Interpolate)
  }
  
  if(d2func == FALSE){
    data2    <- CheckProbData(data2, Interpolate = Interpolate)
  }
  
  
  if(sum(c(d2func, d1func)) == 0){
    
    if(Interpolate == FALSE){
      
      if(length(which(! data1$x %in% data2$x)) > 0 |length(which(! data2$x %in% data1$x)) > 0 ){
        
        if(onlyOverlap){
          index1 <- which(data1$x %in% data2$x)
          index2 <- which(data2$x %in% data1$x)
          
          if(length(index1) < max(index1) - min(index1) |length(index2) < max(index2) - min(index2)){
            warning('some datapoints were taken out, because they didn\'t fit the other dataset' )
          }
          
        }else{
          index1 <- which(data1$x %in% data2$x | data1$x < min(data2$x) | data1$x > max(data2$x))
          index2 <- which(data2$x %in% data1$x | data2$x < min(data1$x) | data2$x > max(data1$x))
        }
        
        dat1 <- data.frame(x = data1$x[index1], y = data1$y[index1])
        dat2 <- data.frame(x = data2$x[index2], y = data2$y[index2])
        
        
      }else{
        dat1 <- data1
        dat2 <- data2
      }
      
    }else{
      OLrange <- c(max(c(min(data1$x),min(data2$x))), min(c(max(data1$x),max(data2$x))))
      both    <- sameXs(data1, data2)
      dats1   <- both[[1]]
      dats2   <- both[[2]]
      
      if(onlyOverlap){
        dat1 <- dats1[which(dats1$x >= OLrange[1] & dats1$x <= OLrange[2]),]
        dat2 <- dats2[which(dats2$x >= OLrange[1] & dats2$x <= OLrange[2]),]
        
      }else{
        dat1 <- dats1
        dat2 <- dats2
      }
    }
   if(length(dat1$x) < mindatapoints){
      stop('There are not enough datapoints, that both datasets share')
    }
  }
  
  
  if(sum(c(d2func, d1func)) == 1){
    
    #determine which is data, and which is function
    
    if(d1func){
      singlefunc <- data1
      singledat  <- data2
      
    }else{
      singlefunc <- data2
      singledat  <- data1
    } 
    
    OLrange   <- c(max(c(min(singledat$x),singlefunc$startpoint)), min(c(max(singledat$x),singlefunc$endpoint)))
    newx      <- vector()
    
    if(Interpolate == FALSE){
      
      if(singlefunc$startpoint < OLrange[1]){
        newx <- c(ceiling(singlefunc$startpoint):OLrange[1])
      }
      
      if(singlefunc$endpoint > OLrange[2]){
        newx <- sort(c(newx, OLrange[2]:floor(singlefunc$endpoint)))
      }
      
    }else{
      stepsize <- (max(c(max(singledat$x),singlefunc$endpoint)) - min(c(min(singledat$x),singlefunc$startpoint)))/(2*funcdatapoints - 1)
      newx     <- vector()
      steps    <- 2 * funcdatapoints
      newx[1]  <- as.numeric(min(c(min(singledat$x),singlefunc$startpoint)))
      
      for(i in c(2:steps)){
        newx[i] <- newx[i-1] + stepsize
      }
      
    }
    
    if(Interpolate == FALSE){
      
      #add the x-values of the dataframe
      
      sida <- singledat
      
      for(j in singledat$x){
        
        added <- FALSE
        
        if(!j %in% newx){
          
          if(length(newx) > 0){
            
            if(j < min(newx)){
              newx  <- c(j, newx)
              added <- TRUE
            }
            
            if(j > max(newx) & added == FALSE){
              newx  <- c(newx, j)
              added <- TRUE
            }
          }
          
          if(added == FALSE & length(newx) > 0){
            
            after <- max(which(newx <= j))
            newx  <- append(newx, j, after = after)
            added <- TRUE
          }
          
          if(!added){
            newx <- c(j)
          }
        }
      }
      
    }else{
      datafunc <- function(x)funcfromdata(x, as.numeric(singledat$x), singledat$y)
      newxdat  <- newx[which(newx <= max(singledat$x) & newx >= min(singledat$x))]
      sida     <- data.frame(x = newxdat, y = datafunc(as.numeric(newxdat)))
    }
    
    if(onlyOverlap){
      newx    <- newx[which(newx <= OLrange[2] & newx >= OLrange[1])]
      singled <- sida[which(sida$x <= OLrange[2] & sida$x >= OLrange[1]),]
      
    }else{
      singled <- sida
    }
    
    newxfunc <- newx[which(newx <= singlefunc$endpoint & newx >= singlefunc$startpoint)]
    
    if(d2func){
      dat1 <- singled
      dat2 <- data.frame(x = newxfunc, y = singlefunc$func(newxfunc))
      
    }else{
      dat2 <- singled
      dat1 <- data.frame(x = newxfunc, y = singlefunc$func(newxfunc))
    }
  }
  
  
  if(sum(c(d2func, d1func)) == 2){
    
    newx     <- vector()
    range    <- c(min(c(data1$startpoint, data2$startpoint)), max(c(data1$endpoint, data2$endpoint)))
    stepsize <- (range[2]-range[1])/((2*funcdatapoints)-1)
    newx[1]  <- range[1]
    steps    <- 2*funcdatapoints
    
    for(i in c(2:steps)){
      newx[i] <- newx[i-1] + stepsize
    }
    
    if(data1$distributiontype == 'beta'){
      if(any(newx %in% data1$startpoint)){
        newx[which(newx == data1$startpoint)] <- newx[which(newx == data1$startpoint)] + 0.00001
      }
      if(any(newx %in% data1$endpoint)){
        newx[which(newx == data1$endpoint)] <- newx[which(newx == data1$endpoint)] - 0.00001
      }
    }
    if(data2$distributiontype == 'beta'){
      if(any(newx %in% data2$startpoint)){
        newx[which(newx == data2$startpoint)] <- newx[which(newx == data2$startpoint)] + 0.00001
      }
      if(any(newx %in% data2$endpoint)){
        newx[which(newx == data2$endpoint)] <- newx[which(newx == data2$endpoint)] - 0.00001
      }
    }
    
    if(onlyOverlap){
      newx  <- newx[which(newx >= min(c(data1$endpoint, data2$endpoint)) & newx <= max(c(data1$startpoint, data2$startpoint)))]
      dat1  <- data.frame(x=newx, y = data1$func(newx))
      dat2  <- data.frame(x=newx, y = data2$func(newx))
      
    }else{
      newx1 <- newx[which(newx <= data1$endpoint & newx >= data1$startpoint)]
      newx2 <- newx[which(newx <= data2$endpoint & newx >= data2$startpoint)]
      dat1  <- data.frame(x=newx1, y = data1$func(newx1))
      dat2  <- data.frame(x=newx2, y = data2$func(newx2))
    }
    
  }
  
  
  return(list(dat1, dat2))
}
#' calculates Matusita's rho
#'
#' calculates Matusita's rho from two datasets
#'
#' calculates Matusita's rho from two datasets, a dataset should be a list which contains either a function (only if CheckData = TRUE) or a data.frame which represents the corresponding probability density function. If CheckData is set False it will go through all the points in the datasets and will not treat it as continous data, meaning, that if the datasets do not share the same points, the result may be calculated wrong. If DataCheck is set TRUE it will run data processing in advance with the EstimatorData() function.
#'   
#' @param data1 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param data2 list: like returned by either the estdist() and uniform() functions (only if CheckData = TRUE) or by the kernelest() and normalize() function.
#' @param datapoints numeric (whole numbers); will be passed as funcdatapoints-argument to the EstimatorData() Function and so represents the amount of datapoints which shall be drawn from an possible input function . 
#' @param addtoplot logical; if TRUE an lines() function will be called which illustrates the outcome. (actually the Matusita's rho is the area under this line)
#' @param CheckData logical; If TRUE the EstimatorData() function will be runned in advance.
#' @param Interpolate logical; will be passed to EstimatorData() function.
#'
#' @return numeric value representing Matusita's rho.
#' 
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' #Create Datasets
#'  a     <- c(1:10)
#'  dates1 <- as.Date(a, origin = '2017-01-01')
#'  count1 <- c(1,1,3,4,6,9,5,4,2,1)
#'  dat1   <- data.frame(dates1, count1)
#'  b     <- c(6:15)
#'  dates2 <- as.Date(b, origin = '2017-01-01')
#'  count2 <- c(1,2,4,5,6,9,3,4,1,1)
#'  dat2   <- data.frame(dates2, count2)
#'  
#'  #Calculate corresponding probability densities
#'  a1 <- estdistr(dat1, returnonlyfunction = FALSE)
#'  b1 <- kernelest(dat2)
#'  
#'  #Calculate Matusita's rho
#'  OLE.matusita(a1,b1)
#' 
#' @export
#' 

OLE.matusita <- function(data1, data2, datapoints=NULL,  addtoplot = FALSE, CheckData = TRUE, Interpolate = TRUE){
  
  # needs two ddataframes as input, those shall represent the values of a timeline and the belonging probability densities. Will calculate matusitas rho with the help of those, by going through the x-values that both datasets have in common. This function is ment to be called by the OLE function.
  
  if(CheckData){
    dat  <- EstimatorData(data1 = data1, data2 = data2, funcdatapoints = datapoints, Interpolate = Interpolate, onlyOverlap = TRUE)
    dat1 <- dat[[1]]
    dat2 <- dat[[2]]
  }else{
    dat1 <- data1
    dat2 <- data2
  }
  
  multi <- vector()
  
  xs <- dat1$x[which(dat1$x %in% dat2$x)]
  
  
  area    <- 0
  i1      <- as.numeric(xs[1])
  first <- T
  
  for(i in xs){
    if(first){
      i1     <- i
      first <- F
      multi  <- c(multi, (dat1$y[which(dat1$x == i)] * dat2$y[which(dat2$x == i)])**0.5)
    }else{
      multi  <- c(multi, (dat1$y[which(dat1$x == i)] * dat2$y[which(dat2$x == i)])**0.5)
      area   <- area + min(multi[which(xs == i1)], multi[which(xs == i)]) * abs(as.numeric(i - i1)) 
      area   <- area + (abs(multi[which(xs == i)] - multi[which(xs == i1)]) * abs(as.numeric( i - i1)) * 0.5)
      
      i1     <- i
    }
  }
  
  if(addtoplot == TRUE){
    lines(xs, multi, ylab=' ', xlab=' ', col='blue')
  }
  
  return(area)
}

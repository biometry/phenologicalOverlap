#' basis to make a function from a dataset
#'
#' basis to make a function from a dataset
#'
#' basis for creating a function from data, xs and ys is the basis for the calculation, so the function will return the interpolated y value to the x-value that is given by dat. the function will return 0 for Every value for dat, that is not in between the minimum and the maximum of xs.
#' 
#' @param dat numeric; X value for which a y-value shall be calculated by interpolating the data.
#' @param xs vector; vector of numeric values, 'x'component of dataset. In combination with ys this makes a dataset. 
#' @param ys vector; vector of numeric values, 'y'component of dataset. In combination with xs this makes a dataset.
#' 
#' @return a numeric value, representing the new y-value to the x-value givn by dat.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
#' 
#' dat  <- BetaData( shape1 = 5,shape2 = 1, n = 1000, StartAndEnd = c(0,15))
#' func <- function(x)funcfromdata(x, xs = as.numeric(dat$Date), y = dat$Count)
#' curve(func, from = as.numeric(min(dat$Date)), to = as.numeric(max(dat$Date)))
#'  
#' @export

funcfromdata <- function(dat, xs, ys){
  
  #   basis for creating a function from data, xs and ys is the basis for the 
  #   calculation, so the function will return the interpolated y value to the
  #   x-value that is given by dat. the function will return 0 for Every value
  #   for dat, that is not in between the minimum and the maximum of xs.
  
  
  ys2 <- vector()
  for(x in dat){
    if(x >= min(xs) & x <= max(xs)){
      if(x %in% xs){
        ys2 <- c(ys2, ys[which(xs == x)])
      }else{
        before <- max(which(xs < x))
        newy <- ys[before] + ((ys[before + 1] - ys[before])/(xs[before + 1] - xs[before]) * (x - xs[before]))
        ys2 <- c(ys2, newy)
      }
    }else{
      ys2 <- c(ys2, 0)
    }
  }
  return(ys2)
}




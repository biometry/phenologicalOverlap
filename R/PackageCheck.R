#' Checks if Package is already installed
#'
#' Checks if Package is already installed and attaches it if it is available.
#'
#' This function checks whether given Package is installed, and if so it will use the library() function on it This function needs a string variable with the name of the package, it has no return()
#'   
#' @param Package string; Name of Package that is to be checked.
#' 
#' @return attaches Package but returns nothing. 
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

PackageCheck <- function(Package){
  
  # This function checks whether given Package is installed, and if so it will use
  # the library() function on it
  # This function needs a string variable with the name of the package, it has no return()
  
  if(!(require(Package, character.only = TRUE))){
    stop(paste0('Please make sure, that package ', Package, ' is installed'))
  }else{
    library(Package, character.only = TRUE)
  }
}
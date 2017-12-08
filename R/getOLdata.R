#' extracts data for a single overlap-measure from simulation dataset
#'
#' extracts data for a single overlap-measure from simulation dataset
#'
#' needs the simulation data, as produced by changingn.(...) or changingvar.(...) functions, for one estimation method('kernel/fitdistr/normalize) as input. Will return a list of all the results for the Overlpa-measure given by olname. This list will be seperated by the units the simulation function has (e.g. stepsizes, number of days. etc.)
#'
#' @param data list; as returned by changingn.(...) or changingvar.(...) functions but with already determined pdf-estimation method, so e.g. simulationdata[['kernel']].
#' @param olname string; name of overlap-measure, as returned by OLE function. Must be in the data.

#' 
#' 
#' @return a list as the input list but with the first two sublevels excluded.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' bla <-changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#' getOLdata(bla, olname = 'Weitzman\'s delta')
#' 
#' @export

getOLdata <- function(data, olname){
  
  #needs the simulation data, as produced by changingn.(...) or changingvar.(...) functions, for one estimation method('kernel/fitdistr/normalize) as input. Will return a list of all the results for the Overlpa-measure given by olname. This list will be seperated by the units the simulation function has (e.g. stepsizes, number of days. etc.)

  if(!olname %in% data[[1]][[1]][[1]][[1]]){
    stop('olname cannot be found in the data')
  }
  
  Oldat <- list()
  
  for(i in c(1:length(data))){
    
    Oldat[[i]] <- vector()
    
    for(j in c(1:length(data[[1]]))){

      Oldat[[i]] <- c(Oldat[[i]], data[[i]][[j]]$Overlap[which(data[[i]][[j]]$Overlapname == olname)])
    }
  }
  
  names(Oldat) <- names(data)
  return(Oldat)
}

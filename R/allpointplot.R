#' plots simulation data with samplle size on x-axis
#'
#' plots simulation data with sample size on x-axis and every datapoint in the plot
#'
#' #Takes Simulation data, as produced by for example changingn.daypattern as input, those need to have a roq named n where the sample sizes for the calculation are stored in. This function will plot the results of the measures on the y axis and the sample sizes on the x axis.
#'
#' @param data list; first simulation data as returned by changingn.(...) or changingvar.(...) functions. 
#' 
#' @return a data.frame representing the input data, where the rows are the different measures and the columns are the datapoints given by the simulation data (for example stepsizes or different standard deviations).
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#'
#' bla <-changingn.daypattern(stepsizes = c(2,-4), repeatings = 2)
#' allpointplot(bla)
#' 
#' @export
allpointplot <- function(data){
  #Takes Simulation data, as produced by for example changingn.daypattern as input, those need to have a roq named n where the sample sizes for the calculation are stored in. This function will plot the results of the measures on the y axis and the sample sizes on the x axis.
  
  
  par(mar=c(1.5,1.5,1.5,1.5))
  first <- T
  
  par(mfrow = c(length(data[[1]][[1]][[1]][[1]]),2))
  
  for( k in data[[1]][[1]][[1]][[1]]){
    
    for(i in c('fitdistr','kernel')){
      values <- vector()
      ns <- vector()
      for( j in names(data[[i]])){
        for(l in c(1:length(data[[i]][[j]]))){
          
          values <- c(values,data[[i]][[j]][[l]][[2]][[which(data[[i]][[j]][[l]][[1]] == k)]])
          ns <- c(ns, data[[i]][[j]][[l]][[2]][[which(data[[i]][[j]][[l]][[1]] == 'n')]])
          
        }
      }
      if(! k == 'n'){
        plot(ns, values, main = paste0(k, ' ', i))
      }
    }
  }
  par(mfrow=c(1,1))
}

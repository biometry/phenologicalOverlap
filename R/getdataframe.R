#' converts simulation data to a data.frame
#'
#' converts simulation data to a data.frame
#'
#' This function needs simulation data as produced by changingn.daypattern as input and gives a dataframe of the results for the probability density function estimation method (namely 'normalize', 'kernel' and 'fitdistr'). If sd = TRUE it will give two columns for each datapoint, one with the means and one with the standard deviations of these means. 
#'
#' @param data list; first sublevel of simulation data as returned by changingn.(...) or changingvar.(...) functions. e.g. simulationdata[['kernel']]
#' @param fitdistr string; determines which data shall be plotted. Determines the first level of the input list and so it can either be 'kernel', 'distrest' or 'normalize'.
#' @param sd logical; If TRUE the function will give two columns for each datapoint, one with the means and one with the standard deviations of these means. 

#' 
#' @return a data.frame representing the input data, where the rows are the different measures and the columns are the datapoints given by the simulation data (for example stepsizes or different standard deviations).
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' 
#' bla <-changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#  getdataframe(bla, fitdistr = 'kernel', sd = TRUE)
#' 
#' @export
getdataframe <- function(data, fitdistr, sd = T){
  
  #This function needs simulation data as produced by changingn.daypattern as input and gives a dataframe of the results for the probability density function estimation method (namely 'normalize', 'kernel' and 'fitdistr'). If sd = TRUE it will give two columns for each datapoint, one with the means and one with the standard deviations of these means. 
  
  dat <- data[[fitdistr]]
  meansres <- list()
  names <- vector()
  
  if(sd){
    names <- c(names,'estimator')
  }
  
  for(k in dat[[1]][[1]][[1]]){
    names <- c(names, k)
  }
  
  for(i in names(dat)){
    means <- list()
    for(k in dat[[1]][[1]][[1]]){
      means[[k]] <- vector() 
    }
    for(j in c(1:length(dat[[i]]))){
      for(k in dat[[i]][[j]][[1]]){
        
        means[[k]] <- c(means[[k]], dat[[i]][[j]][[2]][which(dat[[i]][[j]][[1]] == k)]) 
      }
    }
    meansres[[i]] <- vector()
    if(sd){
      meansres[[i]] <- c(meansres[[i]], 'mean')
      meansres[[paste0(i,'2')]] <- vector()
      meansres[[paste0(i,'2')]] <- c(meansres[[paste0(i,'2')]], 'sd')
    }
    for( l in c(1:length(means))){
      meansres[[i]] <- c(meansres[[i]],mean(means[[l]]))
      if(sd){
        meansres[[paste0(i,'2')]] <- c( meansres[[paste0(i,'2')]], sd(means[[l]]))
      }
    }
    
  }
  
  bla <- data.frame(names, meansres)
  names2 <- vector()
  if(sd){
    for(j in names(dat)){
      names2 <- c(names2, j, j)
    }
  }else{
    names2 <- names(dat)
  }
  
  names(bla) <- c('n', names2)
  
  return(bla)
}

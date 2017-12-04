#' gets mean for overlap measure for simulation dataset
#'
#' gets mean for overlap measure for simulation dataset
#'
#' needs simulation data, as produced by changingn.daypattern but only for one estimation method (kernel/normalize/fitdistr) and calculates the mean for every datapoint considering the measures measures : Weitzman\'s delta','Matusita\'s rho', 'Pianka\'s alpha','Morisitas\'s lambda',  'Horn\'s RO' returns a vector
#'
#' @param data list; first sublevel of simulation data as returned by changingn.(...) or changingvar.(...) functions. e.g. simulationdata[['kernel']]
#' @return a vector of numerrics, representing the means for each ddatapoint in the simulation data (e.g. smaple sizes) and has those as names.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' 
#' bla <-changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#' getmean(bla[['kernel']])
#' 
#' @export
getmean <- function(data){
  
  # needs simulation data, as produced by changingn.daypattern but only for one estimation method (kernel/normalize/fitdistr) and calculates the mean for every datapoint considering the measures measures : Weitzman\'s delta','Matusita\'s rho', 'Pianka\'s alpha','Morisitas\'s lambda',  'Horn\'s RO' returns a vector
  
  nam <- names(data)
  
  means <- vector()
  realmeans <- vector()
  for(i in nam){
    for(j in c(1:length(data[i][1]))){
      means <- c(means, mean(data[i][[j]][[1]][[2]][which(data[i][[j]][[1]][[1]] %in% c('Weitzman\'s delta','Matusita\'s rho', 'Pianka\'s alpha','Morisitas\'s lambda',  'Horn\'s RO'))]))
    }
    realmeans <- c(realmeans, mean(means))
  }
  
  names(realmeans) <- nam
  #c('Weitzman\'s delta','Matusita\'s rho', 'Pianka\'s alpha','Morisitas\'s lambda',  'Horn\'s RO')
  return(realmeans)
}

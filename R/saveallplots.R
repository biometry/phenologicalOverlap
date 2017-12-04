#' saves plots from simulation data
#'
#' saves plots from simulation data
#'
#'  # takes simualtion data as produced by chaningn.daypattern as input. Thiss function will save all the plots using the OLplot function. It will save all the plots for the Overlap-Measures given by OLest. Will save files to directory given by file. if ns = TRUE it wwill use the n (if given as result in the data) on the x-axis instead of the daypattern or number of days. If diff= TRUE it will plot the distance to the mean of the respective datapoint. If logx = TRUE the x-axis will be logarithmic.
#'
#' @param data list; first sublevel of simulation data as returned by changingn.(...) or changingvar.(...) functions. e.g. simulationdata[['kernel']]
#' @param olest string; name of overlap-measure, as returned by OLE function. Must be in the data.
#' @param logx logical; If TRUE x-axis will be logarithmic.
#' @param ns logical; If TRUE the x-axis will be the sample size and not the sublevel of the input data. Can only be TRUE if 'n' is in the data as overlap-measure. 
#' @param diff logical; If TRUE it will plot the difference to the mean of the chosen datapoints, and not the absolute values.
#' @param file string; location where the files shall be saved. will be passed to jpeg() function.
#' 
#' @return nothing but saves files to computer.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' 
#' #saves plots to current working directory
#' bla <-changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#' saveallplots(bla, logx = TRUE, ns = F, diff=F, file = '...')
#' 
#' @export

saveallplots <- function(data, logx = TRUE, ns = F, diff=F, file , OLest = c('Weitzman\'s delta','Matusita\'s rho','Pianka\'s alpha','Morisitas\'s lambda','duration ratio','wmd(z-score)','Horn\'s RO')){
  
  # takes simualtion data as produced by chaningn.daypattern as input. Thiss function will save all the plots using the OLplot function. It will save all the plots for the Overlap-Measures given by OLest. Will save files to directory given by file. if ns = TRUE it wwill use the n (if given as result in the data) on the x-axis instead of the daypattern or number of days. If diff= TRUE it will plot the distance to the mean of the respective datapoint. If logx = TRUE the x-axis will be logarithmic.
  
  
  
  ests <- OLest
  
  trues <- c(1,1,1,1,1,0,1)
  for(i in c('kernel', 'fitdistr', 'normalize')){
    run <- 1
    for(j in ests){
      file <- paste0('betarandomeq',j,i,'.jpeg')
      jpeg(filename = paste0(file, as.character(file)))
      plotOLdata(data, truevalue = trues[run], olname = j, distrest = i, logx=logx, ns = ns, diff=diff)
      print(paste0(i, ' ',j))
      run <- run +1
    }
  }
}
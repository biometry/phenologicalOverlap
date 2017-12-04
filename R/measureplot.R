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
#' @return nothing but creates plot.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' 
#' #saves plots to current working directory
# bla  <- changingn.daypattern(stepsizes = c(2,-4), repeatings = 2, type ='normal')
# bla2 <- changingn.randomday(numbersofdays = c(10,20), repeatings = 2, type = 'normal')
# blas <- list(bla,bla2)
# measureplot(blas, olmeasures = c('Weitzman\'s delta', 'duration ratio'))
#' 
#' @export
#' 

measureplot <- function(data, olmeasures){
  #takes a list of simulated data  as produced by the changingn functions as input. First the ylim of the plots will be determined, by considering all the results for the overlapmeassures given by olmeasures. And will then plot the means those in one plotwindow and also the sd of those in another. 
  
  
  cols <- c('red','blue','green','orange','yellow','black','gray','darkblue','darkred','brown')
  par(mar=c(2,2,2,2))
  first <- T
  #par(mfrow = c(length(names(data)),4))
  par(mfrow = c(2,4))
  minmaxmean <- vector()
  newdatmean <- vector()
  minmaxsd <- vector()
  newdatsd <- vector()
  ylimmean <- vector()
  ylimsd <- vector()
  
  #determine ylim
  
  for(i in names(data)){
    for(j in c('kernel','fitdistr')){
      print(getdataframe(data, fitdistr = j))
      jo <- getdataframe(data[[i]], j)
      
      
      
      for(m in c('mean','sd')){
        
        for(k in which(jo[,1] %in% olmeasures)){
          
          for(n in jo[k,which(jo[1,1:length(jo)] == m)]){
            if(m == 'sd'){
              newdatsd <- c(newdatsd, as.numeric(as.character(n)))
              
            }else{
              newdatmean <- c(newdatmean, as.numeric(as.character(n)))
            }
          }
        }
      }
    }
  }
  
  
  minmaxmean <- c(minmaxmean, min(newdatmean),max(newdatmean))
  ylimmean <- c(min(minmaxmean),max(minmaxmean))
  
  minmaxsd <- c(minmaxsd, min(newdatsd),max(newdatsd))
  ylimsd <- c(min(minmaxsd),max(minmaxsd))
  
  # get means and plot them
  
  for(i in names(data)){
    for(j in c('kernel','fitdistr')){
      jo <- getdataframe(data[[i]], j)
      for(m in c('mean','sd')){
        if(i %in% c('random','randomeq')){
          xlim <- c(max(as.numeric(as.character(names(jo[2:length(jo)])))),min(as.numeric(as.character(names(jo[2:length(jo)])))))
        }else{
          if(i %in% c('pattern','patterneq')){
            xlim <- c(-22,0)
          }else{
            xlim <- c(min(as.numeric(as.character(names(jo[2:length(jo)])))),max(as.numeric(as.character(names(jo[2:length(jo)])))))
          }
        }
        if(m == 'sd'){
          plot(0, ylim = ylimsd, xlim = xlim, main = paste0(i,' ',j,' ',m))
        }else{
          plot(0, ylim = ylimmean, xlim = xlim, main = paste0(i,' ',j,' ',m))
        }
        for(k in which(jo[,1] %in% olmeasures)){
          
          newplot <- 1
          for(l in which(jo[,1] %in% olmeasures)){
            newdat <- vector()
            newdat2 <- vector()
            for(n in jo[l,which(jo[1,1:length(jo)] == m)]){
              newdat <- c(newdat, as.numeric(as.character(n)))
            }
            xs <- sort(unique(round(as.numeric(as.character(names(jo[2:length(jo)]))))))
            
            for(o in xs){
              newdat2 <- c(newdat2,newdat[which(unique(round(as.numeric(as.character(names(jo[2:length(jo)])))))==o)])
            }
            
            #changes 2 to -2 for the pattern results for illustration purpose
            if(2 %in% xs & i %in% c('pattern','patterneq')){
              xs[which(xs == 2)] <- -2
              
              cool <- newdat2[which(xs == -2)]
              newdat2[which(xs == -2)] <- newdat2[which(xs == 0)]
              newdat2[which(xs == 0)] <- cool
              xs <- sort(xs)
            }
            lines(xs, newdat2, col = cols[newplot])
            newplot <- newplot +1
            if(first){
              print(paste0(jo[,1][[l]], ' = ', cols[newplot-1]))
            }
            
          }
          first = F
        }
      }
    }
  }
  par(mfrow=c(1,1))
}

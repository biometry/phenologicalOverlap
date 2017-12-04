#' normal plot for simulation data
#'
#' normal plot for simulation data
#'
#' takes simulation data, as produced by changingn.randomday as input. Plots the results of the simulation for the Overlapmeasure given by olname and the estimation method (kernel/fitdistr/normalize) given by distrest. The truevalue for it has to be given and will be visible in the plot as horizontal line. this function will dettermine the values for xlim and ylim as the minimum and maximum value of the data on those axes. But they can be given as argument. if logx = TRUE the x-axis will be logarithmic. Will plot the means and give the Standrad deviation as error bar. If diff = TRUE it will plot hte difference to the mean of the pdf-based Overlap-measures. 
#'
#' @param data list; first sublevel of simulation data as returned by changingn.(...) or changingvar.(...) functions. e.g. simulationdata[['kernel']]
#' @param olname string; name of overlap-measure, as returned by OLE function. Must be in the data.
#' @param truevalue numeric; a horizontal line with the given number as y-value.
#' @param distrest string; determines which data shall be plotted. Determines the first level of the input list and so it can either be 'kernel', 'distrest' or 'normalize'.
#' @param logx logical; If TRUE x-axis will be logarithmic.
#' @param ns logical; If TRUE the x-axis will be the sample size and not the sublevel of the input data. Can only be TRUE if 'n' is in the data as overlap-measure. 
#' @param diff logical; If TRUE it will plot the difference to the mean of the chosen datapoints, and not the absolute values.
#' @param ylim vector of two numerics; determines the displayed window of the plot on the y axis. will be passed to plot() function. 
#'  @param xlim vector of two numerics; determines the displayed window of the plot on the x axis. will be passed to plot() function. 
#' 
#' @return nothing.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#' @examples
#' bla <-changingvar.beta(days=c(5,12), n = 500, repeatings = 2)
#' plotOLdata(bla, olname = 'Weitzman\'s delta', truevalue = 1, logx = FALSE, distrest = 'kernel')
#' 
#' @export

plotOLdata <- function(data, truevalue, olname, distrest='fitdistr', logx=TRUE, ns = F, diff = F, ylim = NULL, xlim = NULL){
  
  #takes simulation data, as produced by changingn.randomday as input. Plots the results of the simulation for the Overlapmeasure given by olname and the estimation method (kernel/fitdistr/normalize) given by distrest. The truevalue for it has to be given and will be visible in the plot as horizontal line. this function will dettermine the values for xlim and ylim as the minimum and maximum value of the data on those axes. But they can be given as argument. if logx = TRUE the x-axis will be logarithmic. Will plot the means and give the Standrad deviation as error bar. If diff = TRUE it will plot hte difference to the mean of the pdf-based Overlap-measures. 
  
  
  data <- data[[as.character(distrest)]]
  
  if(diff){
    means2 <- getmean(data)
  }else{
    means2 <- 0
  }
  
  if(ns){
    xs <- getOLdata(data, 'n')
  }
  
  data <- getOLdata(data, olname = olname)
  
  means   <- vector()
  sds     <- vector()
  
  
  if(ns){
    x <- vector()
    for(u in c(1:length(xs))){
      x[u] <- mean(xs[[u]])
    }
  }else{
    x <- as.integer(names(data))
  }
  
  if(logx){
    x[which(x!=0)] <- log(x[which(x!=0)], base = 10)
  }
  
  dats <- data.frame(matrix(nrow=length(data), ncol=length(data[[1]])))
  
  for(i in c(1:length(data))){
    for(j in c(1:length(data[[i]]))){
      dats[i,j] <- data[[i]][j]
    }
  }
  ys<- vector()
  
  for(k in c(1:length(dats[,1]))){
    means <- c(means, mean(as.numeric(dats[k,])))
    sds  <- c(sds,sd(as.numeric(dats[k,])))
    if(diff){
      ys <- c(ys, means[k]-means2[k]-sds[k], means[k]-means2[k]+sds[k])
    }
  }
  
  if(is.null(xlim)){
    if(logx){
      xlim <- c(min(x),max(x))
    }else{
      xlim <- c(min(x),max(x))
    }
  }
  if(is.null(ylim)){
    if(diff){
      ylim <- c(min(ys),max(ys))
    }else{
      ylim <- c(min(means-sds,truevalue),max(means+sds,truevalue))
    }
    if(any(is.na(ylim))){
      ylim <- c(0,1)
    }
  }
  epsilon <- (xlim[2]-xlim[1])/100
  
  if(ns){
    if(logx){
      xlab <- 'log(n)'
    }else{
      xlab <- 'n'
    }
  }else{
    if(logx){
      xlab <- 'log(number of days)'
    }else{
      xlab  <- 'number of days'
    }
  }
  
  for(k in c(1:length(dats[,1]))){
    if(diff){
      
      #plot and error bars
      
      plot(x[k], means[k]-means2[k], ylim = ylim, xlim= xlim, ylab = 'Estimated Overlap', xlab= xlab, main = paste0(distrest,': ',olname))
      segments(x[k],means[k] - means2[k]-sds[k],x[k],means[k]- means2[k] + sds[k])
      segments(x[k]-epsilon, means[k]- means2[k] - sds[k],x[k]+epsilon, means[k]- means2[k] - sds[k])
      segments(x[k]-epsilon, means[k]- means2[k] + sds[k],x[k]+epsilon, means[k]- means2[k] + sds[k])
      par(new=T)
    }else{
      
      plot(x[k], (means[k]), ylim = ylim, xlim= xlim, ylab = 'Estimated Overlap', xlab= xlab, main = paste0(distrest,': ',olname))
      segments(x[k],means[k] - sds[k],x[k],means[k] + sds[k])
      segments(x[k]-epsilon, means[k] - sds[k],x[k]+epsilon, means[k] - sds[k])
      segments(x[k]-epsilon, means[k] + sds[k],x[k]+epsilon, means[k] + sds[k])
      par(new=T)
      
    }
  }
  if(!diff){
    abline(h=truevalue)
  }else{
    abline(h=0)
  }
  
  
}

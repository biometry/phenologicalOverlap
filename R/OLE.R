#' Calculates phenological overlap
#'
#' Calculates phenological overlap (in different ways) of abundance datasets
#'
#' This function will determine the Overlap-measures given by OLest for two abundance datasets, whch need to be a dataframe, that has two columns, one with dates and one with numeric values. This function will first estimate a probability density function, using the method given by distrest amd calculate the ovelrpa- measures on the basis of those. Those estimations can be influenced by some argument. For the kernel density esttimation those are: bandwidth which will be the bandwidth that is to use, the kernel will be the kernel that there is to use and kerneldatapoints will determine, how much data-points the density function shall produce. Dor the fitted distributions those arguments are: forcedistrtype which will  force the estdist function to consider only one (or more) distributions. and the fitdistrrange which will be the borders of the fitted distributions. If consider = 'data1' or 'data2' only asymetric measures willbe considered. The interpolate argumetn is used for the normalize estimation method, this will interpolate the data linearly, so that both datassets, that are produced by the normalize function will have the same x vlaeus. If plot = TRue it will plot both PDFs and teh data in the background along with Weitzman's delat and Matusitas rho if they are considered. If the probability densities shall be estimated in different ways, then the distrest argument needs the length two. the datapoints argument will determine,  how much datapoints there are to produce from the probability ddensity functions. And is therefore a measure of accuracy of the calculations. 
#'
#' @param data1 data-frame; Abundance dataset. It must have two columns, one wih date-objects one with numeric values.
#' @param data2 data-frame; Abundance dataset. It must have two columns, one wih date-objects one with numeric values.
#' @param distrest string or vector; Determines in which way the abundace datasets shall be converted into possibility densities. If only a string the same will be used for every possible application, if a vector the first object will be for the first data, and accordingly the second for the second data. Can either be: 'fitdistr', 'kernel', 'normalize' or 'uniform'.
#' @param consider string; Can either be 'both', 'data1' or 'data2'. If both the phenological overlap will be calculated for both datasets, if 'data1' or 'data2' there will be asymmetrical overlaps calculated.
#' @param datapoints numeric (whole numbers); determines how much datapoints shall be drawn from functions. only rough guideline, results may vary. Will b passed to EstimatorData() function as funcdatapoints arguments.
#' @param OLest vector of strings; determines in which ways the Overlap shall be calculated. Can contain the following strings: 'Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Dissimilarity', 'Duration', 'Hurlbert', 'WMD', 'ProductMoment', 'Lloyd','Horn' if consider is set to 'both'  and 'Weitzman'and 'Asym.alpha' if consider is either 'data1' or 'data2'. Default will set all possible measures automatically.
#' @param mindatapoints numeric (whole numbers); Sets a minimum of datapoints from which the overlap is determined. (so it is not applying to the datapoints of th abundance data but for the probability density data). Will be passed to EstimatorData() function.
#' @param plot logical; If TRUE the results will be illustrated in a plot. 
#' @param ylimfunc vector of numerics; sets the limits of the plot of the pdfs on the y-axis, will be passed as ylim argument to plot() or curve() function. 
#' @param StartAndEnd vector or list;If only a string the same will be used for every possible application, if a list the first object (vector) will be for the first data, and accordingly the second for the second data. In this vector shall be two date-objects, defining the range, in which the distributions shall be fitted. As default the first and last dates in the data data-frame will be selected. Does only apply if distrest is 'fitdistr'. Will be passed to estdist() function.
#' @param forcedistrtype string or vector; If only a string the same will be used for every possible application, if a vector the first object will be for the first data, and accordingly the second for the second data. Can either be 'normal' for the truncated normal distribution or 'beta' for beta distribution. This will force the function to only consider one of those distributions for the calculation.  Does only apply if distrest is 'fitdistr'. Will be passed to estdist() function.
#' @param kernel string or vector; If only a string the same will be used for every possible application, if a vector the first object will be for the first data, and accordingly the second for the second data. Can either be 'normal' for the truncated normal distribution or 'beta' for beta distribution.the kerel which shall be used. Can be "gaussian", "epanechnikov", "rectangular", "triangular" or "biweight". Does only apply if distrest is 'kernel'. Will be passed to kernelest() function.
#' @param bandwidth numeric or vector;If only a numeric the same will be used for every possible application, if a vector the first object will be for the first data, and accordingly the second for the second data. Can either be 'normal' for the truncated normal distribution or 'beta' for beta distribution. the bandwidth for the kernel density estimation. If is null it will be determined by the unbiased cross validation using the h.ucv() function from the 'kedd' package. Does only apply if distrest is 'kernel'. Will be passed to kernelest() function.
#' @param kerneldatapoints numeric, whole number or vector;If only a numeric the same will be used for every possible application, if a vector the first object will be for the first data, and accordingly the second for the second data. determines, how much datapoints shall be produced by the density() function. As can be seen in the help of density() this shall be a power of two.Does only apply if distrest is 'kernel'. Will be passed as numberofdatapoints argument to kernelest() function.
#' @param interpolate logical; determines whether data should be interpolated, so that both datasets will share same datapoints later on or not. Will run additional Checks if TRUE. Will be passed to EstimatorData() function and if distres = 'normalize' also to the CheckData() function.
#' 
#' @return a data.frame, with one column named Overlapnames where the names of the used overlap-calculation-methods are stored in and one column named Overlap with the coresponding values.
#'
#' @author Florian Berger <florian_berger@ymail.com>
#'
#' @seealso \code{scale}
#'
#'
#' @examples
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
#'  OLE(dat1,dat2, distrest = c('fitdistr','kernel'), kernel ='gaussian', forcedistrtype = 'beta', datapoints = 1000, plot = TRUE)
#' 
#' @export
OLE <- function(data1, data2, distrest = 'kernel', kerneldatapoints = NULL, OLest = NULL, consider = 'both', datapoints = NULL, mindatapoints=20, Interpolate = TRUE, kernel = NULL, bandwidth = NULL, forcedistrtype = NULL, fitdistrrange = NULL, plot=FALSE, ylimfunc=NULL){
  
  
  #This function will determine the Overlap-measures given by OLest for two abundance datasets, whch need to be a dataframe, that has two columns, one with dates and one with numeric values. This function will first estimate a probability density function, using the method given by distrest amd calculate the ovelrpa- measures on the basis of those. Those estimations can be influenced by some argument. For the kernel density esttimation those are: bandwidth which will be the bandwidth that is to use, the kernel will be the kernel that there is to use and kerneldatapoints will determine, how much data-points the density function shall produce. Dor the fitted distributions those arguments are: forcedistrtype which will  force the estdist function to consider only one (or more) distributions. and the fitdistrrange which will be the borders of the fitted distributions. If consider = 'data1' or 'data2' only asymetric measures willbe considered. The interpolate argumetn is used for the normalize estimation method, this will interpolate the data linearly, so that both datassets, that are produced by the normalize function will have the same x vlaeus. If plot = TRue it will plot both PDFs and teh data in the background along with Weitzman's delat and Matusitas rho if they are considered. If the probability densities shall be estimated in different ways, then the distrest argument needs the length two. the datapoints argument will determine,  how much datapoints there are to produce from the probability ddensity functions. And is therefore a measure of accuracy of the calculations. 
  
  #exception handling and setting default values
  
  if(!consider %in% c('data1','data2','both')){
    stop('Consider can only be: data1, data2 or both')
  }
  
  if(is.null(OLest)){
    if(consider == 'both'){
      OLest <- c('Weitzman', 'Matusita', 'Pianka', 'Morisita', 'Dissimilarity', 'Duration', 'Hurlbert', 'WMD', 'ProductMoment', 'Lloyd','Horn')
    }else{
      OLest <- c('Weitzman', 'Asym.alpha')
    }
  }
  
  
  
  
  if(any(!(distrest %in% c('kernel','fitdistr','unif','normalize','none')))){
    stop(' distrest needs to be one of the following symbols for the method of distribution estimating:\'kernel\',\'fitdistr\',\'unif\',\'normalize\',\'none\' ')
  }
  
  if(length(distrest) == 1){
    distrest <- c(distrest, distrest)
  }
  if(length(distrest) > 2){
    stop('The maximum length of distrest is 2, as there are only two pdfs to estimate')
  }
  
  if(any(!OLest %in% c('Weitzman','Matusita','Pianka','Morisita', 'Dissimilarity','Asym.alpha','Duration','Hurlbert','WMD', 'ProductMoment', 'Lloyd','Horn','n'))){
    stop('One of the given Overlapestimationmethods is not possible, please make sure it is one of: \'Weitzman\',\'Matusita\',\'Pianka\',\'Morisita\', \'Dissimilarity\',\'Asym.alpha\',\'Duration\', \'Hurlbert\', \'WMD\', \'ProductMoment\', \'Lloyd\'')
  }
  
  if(sum(distrest == 'fitdistr') == 0 & length(forcedistrtype) > 0){
    warning('No fitdistr estimation is made, so the forcedistrtype argument is unused')
  }
  
  if(length(forcedistrtype) == 2 & sum(distrest == 'fitdistr') < 2){
    stop('If only one fitdistr estimation is to be made please make sure that the length of forcedistrtype is also 1')
  }
  
  if(length(forcedistrtype) ==1){
    forcedistrtype <- c(forcedistrtype, forcedistrtype)
  }
  
  
  if(is.null(forcedistrtype)){
    forcedistrtype <- c(NULL, NULL)
  }
  
  if(!is.null(fitdistrrange)){
    if(! length(fitdistrrange) %in% c(1,2,4)){
      stop('fitdistrrage needs to contain 2 or 4 dates and can either be a list or a vector')
    }
    if(length(fitdistrrange) == 1){
      if(is.list(fitdistrrange)){
        fitdistrrange <- list(fitdistrrange[[1]], fitdistrrange[[1]])
      }else{
        stop('fitdistrrage needs to contain 2 or 4 dates and can either be a list or a vector')
      }
    }
    if(length(fitdistrrange) == 2){
      if(is.list(fitdistrrange)){
        if(!length(fitdistrrange[[1]]) == 2 | !length(fitdistrrange)[[2]] == 2){
          stop('fitdistrrage needs to contain 2 or 4 dates and can either be a list or a vector')
        }else{
          if(any(! inherits(fitdistrrange[[1]], 'Date') | ! inherits(fitdistrrange[[2]], 'Date'))){
            stop('please make sure, that fitdistrrange consists of only Date objects')
          }
          if(sum(distrest == 'fitdistr') == 1){
            stop('Only one distribution is fitted, so please give only two dates in fitdistrrange')
          }
        }
      }
      if(any(!inherits(fitdistrrange, 'Date'))){
        stop('please make sure, that fitdistrrange consists of only Date objects')
      }else{
        fitdistrrange <- list(fitdistrrange, fitdistrrange)
      }
    }else{
      if(length(fitdistrrange) == 4 & sum(distrest == 'fitdistr') != 1){
        if(any(! inherits(fitdistrrange, 'Date'))){
          stop('please make sure, that fitdistrrange consists of only Date objects')
        }else{
          fitdistrrange <- list(fitdistrrange[c(1,2)],fitdistrrange[c(3,4)])
        }
      }else{
        if(sum(distrest == 'fitdistr') == 2){ 
          stop('fitdistrrage needs to contain 2 or 4 dates and can either be a list or a vector')
        }
        if(sum(distrest) == 1){
          stop('Only one distribution is fitted, so please give only two dates in fitdistrrange')
        }
      }
    }
  }
  
  if(length(forcedistrtype) > 2){
    stop('The maximum length of forcedistrtype is 2 as there are only two fitdistr estimations to be made')
  }
  
  if(sum(distrest == 'kernel')  == 0){
    if(length(kernel) > 0){
      warning('No kernel density estimation is done, so the kernel argument is unused')
    }
    if(length(bandwidth) > 0){
      warning('No kernel density estimation is done, so the bandwidth argument is unused')
    } 
  }
  
  if(is.null(kerneldatapoints)){
    kerneldatapoints <- c(1024, 1024)
  }
  if(!length(kerneldatapoints) %in% c(1,2)){
    stop('please give one or two number of datapoints for kernel density estimation (kerneldatapoints)')
  }
  if(length(kerneldatapoints) == 1){
    if(is.numeric(kerneldatapoints)){
      kerneldatapoints <- c(kerneldatapoints, kerneldatapoints)
    }else{
      stop('please make sure, that your kerneldatapoints is numeric')
    }
  }
  if(length(kerneldatapoints) == 2 & any(! is.numeric(kerneldatapoints))){
    stop('Please make sure, that your kerneldatapoints are numeric')
  }
  
  if(! is.null(fitdistrrange) & sum(distrest == 'fitdistr') == 0){
    warning('No distribution is fitted, so the fitdistrrange argument is unused')
  }
  
  if(is.null(kernel)){
    kernel <- 'gaussian'
  }
  
  if(is.null(bandwidth)){
    bandwidth <- c(NULL, NULL)
  }
  
  if(sum(distrest == 'kernel') == 1){
    
    if(length(kernel) == 2){
      stop('If only one kerne density estimation is to be made, please make sure, that you give only one kernel')
    }
    
    if(length(bandwidth) == 2){
      stop('If only one kerne density estimation is to be made, please make sure, that you give only one bandwidth')
    }
  }
  
  if(length(kernel) == 1){
    kernel <- c(kernel, kernel)
  }
  
  if(length(bandwidth) == 1){
    bandwidth <- c(bandwidth, bandwidth)
  }
  
  if(length(kernel) > 2){
    stop('The maximum length of kernel is 2 as there are only two kernel density estimations to be made')
  }
  
  if(length(bandwidth) > 2){
    stop('The maximum length of bandwidth is 2 as there are only two kernel density estimations to be made')
  }
  
  if(distrest[1] != 'none'){
    data1 <- CheckData(data1)
    if(distrest[1]  == 'kernel'){
      d1est1 <- kernelest(data1, kernel = kernel[1], bandwidth = bandwidth[1], CheckData = FALSE, numberofdatapoints = kerneldatapoints[1])
      
    }
    
    #Estimating the probabilities
    
    if(distrest[1]  == 'fitdistr'){
      d1est1 <- estdist(data1, returnonlyfunction = FALSE, forcedistrtype = forcedistrtype[1], StartAndEnd = fitdistrrange[[1]], CheckData = FALSE)
      
    }
    
    if(distrest[1]  == 'unif'){
      d1est1 <- uniform(data1, CheckData = FALSE)
    }
    
    if(distrest[1]  == 'normalize'){
      d1est1 <- normalize(data1, Interpolate = Interpolate, CheckData = TRUE)
    }
  }else{
    d1est1 <- data1  
  }
  
  if(distrest[2] != 'none'){
    data2 <- CheckData(data2)
    if(distrest[2]  == 'kernel'){
      d2est1 <- kernelest(data2, kernel = kernel[2], bandwidth = bandwidth[2], numberofdatapoints = kerneldatapoints[1], CheckData = FALSE)
    }
    
    if(distrest[2]  == 'fitdistr'){
      d2est1 <- estdist(data2, returnonlyfunction = FALSE, forcedistrtype = forcedistrtype[2], StartAndEnd = fitdistrrange[[2]], CheckData = FALSE)
    }
    
    if(distrest[2]  == 'unif'){
      d2est1 <- uniform(data2, CheckData = FALSE)
    }
    
    if(distrest[2]  == 'normalize'){
      d2est1 <- normalize(data2, Interpolate = Interpolate, CheckData = TRUE)
    }
  }else{
    d2est1 <- data2
  }
  
  
  
  d1func <- FALSE
  d2func <- FALSE
  
  #determin whether one of the given datasets includes a function
  for(i in c(1:length(d1est1))){
    if(is.function(d1est1[[i]])){
      d1func <- TRUE
    }
  }
  for(i in c(1:length(d2est1))){
    if(is.function(d2est1[[i]])){
      d2func <- TRUE
    }
  }
  
  #prepare Data for overlapestimation
  
  data         <- EstimatorData(d1est1, d2est1, funcdatapoints = datapoints, mindatapoints = mindatapoints, Interpolate = Interpolate, onlyOverlap = FALSE)
  d1est        <- data.frame(x = data[[1]]$x, y= data[[1]]$y)
  d2est        <- data.frame(x = data[[2]]$x, y= data[[2]]$y)
  Overlaprange <- c(max(d1est1$startpoint,d2est1$startpoint),min(d1est1$endpoint,d2est1$endpoint))
  d1estol      <- d1est[which(d1est$x <= Overlaprange[2] & d1est$x >= Overlaprange[1]),]
  d2estol      <- d2est[which(d2est$x <= Overlaprange[2] & d2est$x >= Overlaprange[1]),]
  
  
  #make PLOTS
  
  
  if(plot==TRUE){
    
    xlim <- as.numeric(c(min(d1est1$startpoint, d2est1$startpoint)-0.1,max(d1est1$endpoint, d2est1$endpoint)+0.1))
    
    if(d1func == FALSE){
      max1 <- max(d1est1$data$y)
    }else{
      max1 <- optimize(d1est1$func, interval = c(d1est1$startpoint, d1est1$endpoint), maximum = TRUE)$objective
    }
    if(d2func == FALSE){
      
      max2 <- max(d2est1$data$y)
      
    }else{
      max2 <- optimize(d2est1$func, interval = c(d2est1$startpoint, d2est1$endpoint), maximum = TRUE)$objective
    }
    
    ylim <- as.numeric(c(0,max(max1,max2,data1$Count/sum(data1$Count),data2$Count/sum(data2$Count))))
    
    if(is.null(ylimfunc)){
      ylimfunc <- ylim
    }
    
    
    if(distrest[1] != 'none'){
      if(distrest[1]=='normalize' & Interpolate == FALSE){
        plot(data1$Date[which(data1$Date %in% d1est$x)], data1$Count[which(data1$Date %in% d1est$x)]/sum(data1$Count[which(data1$Date %in% d1est$x)]), xlim=xlim, ylim = ylim, type='h', col='gray', xlab = '', ylab =' ')
      }else{
        plot(data1$Date, data1$Count/sum(data1$Count), xlim=xlim, ylim = ylim, type='h', col='gray', xlab = '', ylab =' ')
      }
    }
    
    if(distrest[2] != 'none'){
      if(distrest[2] == 'normalize' & Interpolate == FALSE){
        par(new=T)
        plot(data2$Date[which(data2$Date %in% d2est$x)], data2$Count[which(data2$Date %in% d2est$x)]/sum(data2$Count[which(data2$Date %in% d2est$x)]), xlim=xlim, ylim = ylim, type='h', col='gray', xlab = '', ylab =' ')
        
      }else{
        par(new=T)
        plot(data2$Date, data2$Count/sum(data2$Count), xlim=xlim, ylim = ylim, type='h', col='gray', xlab = '', ylab =' ')
      }
    }
    
    if(d1func == FALSE){
      par(new=T)
      plot(d1est1$data$x, d1est1$data$y, ylab='Probability Density', xlab='Date', type ='l',  xlim = xlim, ylim = ylimfunc, axes=FALSE , main= 'Overlap-Estimation', col = 'BLACK')
    }else{
      func1 <- as.function(d1est1$func)
      par(new=T)
      curve(func1, xlim = xlim, ylim = ylimfunc, ylab='Probability Density', xlab = 'Date', axes=FALSE, main= 'Overlap-Estimation', col = 'darkred')
    }
    if(d2func == FALSE){
      par(new=T)
      plot(d2est1$data$x, d2est1$data$y, ylab='Probability Density', xlab=' ', type ='l', xlim = xlim, ylim = ylimfunc, axes=FALSE, main= 'Overlap-Estimation')
    }else{
      func2 <- as.function(d2est1$func)
      par(new=T)
      curve(func2, xlim = xlim, ylim = ylimfunc, ylab='Probability Density', axes=FALSE, xlab=' ', main= 'Overlap-Estimation')
    }
    
    
  }
  
  #OVERLAP ESTIMATIONS
  
  Overlaps     <- vector()
  Overlapnames <- vector()
  
  if('Weitzman' %in% OLest){
    Overlapnames <- c(Overlapnames, 'Weitzman\'s delta')
    Overlaps     <- c(Overlaps, OLE.weitzman(d1estol, d2estol, datapoints = datapoints, addtoplot = plot, consider = consider, CheckData = FALSE))
  }
  
  if('Matusita' %in% OLest ){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Matusita\'s rho')
      Overlaps     <- c(Overlaps, OLE.matusita(d1estol, d2estol, datapoints = datapoints, addtoplot = plot, CheckData = FALSE))
    }else{
      stop('Matusitas rho can only be calculated if both datasets are considered')
    }
  }
  
  if('Pianka' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Pianka\'s alpha')
      Overlaps     <- c(Overlaps, OLE.pianka(d1est, d2est, datapoints = datapoints, CheckData = FALSE))
    }else{
      stop('Piankas alpha can only be calculated if both datasets are considered')
    }
  }
  
  if('ProductMoment' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Product Moment Correlation Coefficient')
      Overlaps     <- c(Overlaps, OLE.productmoment(data1, data2, datapoints = datapoints, addtoplot = plot, CheckData = FALSE))
    }else{
      stop('The Product Moment Correlation Coefficient can only be calculated if both datasets are considered')
    }
  }
  
  if('Lloyd' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'LLoyd\'s Interspecies Patchiness')
      Overlaps     <- c(Overlaps, OLE.lloyd(d1estol, d2estol, datapoints = datapoints, CheckData = FALSE))
    }else{
      stop('Lloyds interspecies patchiness can only be calculated if both datasets are considered')
    }
  }
  
  if('Morisita' %in% OLest ){
    if( consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Morisitas\'s lambda')
      Overlaps     <- c(Overlaps, OLE.morisita(d1est, d2est, datapoints = datapoints, addtoplot = plot, CheckData = FALSE))
    }else{
      stop('Morisitas lambda can only be calculated if both datasets are considered')
    }
  }
  
  if('Dissimilarity' %in% OLest ){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Dissimilarity index')
      Overlaps     <- c(Overlaps, OLE.dissimilarity(d1est, d2est, datapoints = datapoints, addtoplot = FALSE, CheckData = FALSE))
    }else{
      stop('The dissimilarity index can only be calculated if both datasets are considered')
    }
  }
  
  if('Asym.alpha' %in% OLest){
    if(consider != 'both'){
      Overlapnames <- c(Overlapnames, 'asymmetrical alpha')
      Overlaps     <- c(Overlaps, OLE.asymalpha(d1est, d2est, datapoints = datapoints, addtoplot = plot, CheckData = FALSE, weightedby = consider))
    }else{
      stop('The assymetrical alpha can only be calculated if only one datasets is considered for weighting')
    }
  }
  
  if('Duration' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'duration ratio')
      Overlaps     <- c(Overlaps, OLE.duration(d1est1, d2est1, CheckData = FALSE))
    }else{
      stop('duration ratio can only be calculated if both datasets are considered')
      
    }
  }
  
  if('WMD' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'wmd(z-score)')
      Overlaps     <- c(Overlaps, OLE.wmd(data1, data2, datacheck = FALSE))
    }else{
      stop('wmd (Z-score) can only be calculated if both datasets are considered')
      
    }
  }
  
  if('Hurlbert' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Hurlbert\'s PIE')
      Overlaps     <- c(Overlaps, OLE.hurlbert(data1, data2, datacheck = FALSE, Interpolate = Interpolate))
    }else{
      stop('Hurlberts PIE can only be calculated if both datasets are considered')
    }
  }
  
  if('Horn' %in% OLest){
    if(consider == 'both'){
      Overlapnames <- c(Overlapnames, 'Horn\'s RO')
      Overlaps     <- c(Overlaps, OLE.horn(d1est, d2est, CheckData = FALSE))
    }else{
      stop('Hurlberts PIE can only be calculated if both datasets are considered')
    }
  }
  if('n' %in% OLest){
    Overlapnames <- c(Overlapnames, 'n')
    Overlaps <- c(Overlaps, min(sum(data1$Count),sum(data2$Count)))
  }
  OL <- data.frame(Overlapname = Overlapnames, Overlap = Overlaps)
  
  return(list(Overlap = OL, pdf1 = d1est1[2:length(d1est1)], pdf2 = d2est1[2:length(d2est1)]))
}

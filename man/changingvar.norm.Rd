% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changingvar-norm.R
\name{changingvar.norm}
\alias{changingvar.norm}
\title{simulates abundance data and calculates overlap for changing variances}
\usage{
changingvar.norm(repeatings = 100, sds = c(112, 104, 84, 57, 27, 13, 1),
  n = 1000, OLest = c("Weitzman", "Matusita", "Pianka", "Morisita",
  "Duration", "WMD", "Horn", "n"))
}
\arguments{
\item{repeatings}{numeric (whole numbers); Determines how often the whole procedure (data producing and calculation of the overlap) shall be repeated.}

\item{sds}{vector of numerics; Those will be the standard deviations of the noemal distributions which is used to create the datasets.}

\item{n}{numeric (whole number); Determines the sample size for creating the datasets. Will be passed as as 'n' argument to to the NormData() function.}

\item{OLest}{vector of strings; determines in which way(s) the overlap shall be determined, will be passed to OLE function.}
}
\value{
a list. First index level determines the way of estimating the pdf so it has the three indexes: 'kernel', 'fitdistr' and 'normalize'. The second index-level is the chosen standard deviation and the last the number of the repeating. In there are the results as returned by OLE function. To illustrate: list[[kernel]][[standard deviation]][[repeating]].
}
\description{
simulates abundance data and calculates overlap for changing variances
}
\details{
#Will determine the overlap-measures given by OLest for two simulated datasets both coming from a normla distribution with the sample size given by n. This will be done for every standard deviation given by sds. And ffor every standard deviation as many overlaps will be determined as the repeatings argument says.
}
\examples{
changingvar.norm(sds=c(5,12), n = 500, repeatings = 2)

}
\seealso{
\code{scale}
}
\author{
Florian Berger <florian_berger@ymail.com>
}

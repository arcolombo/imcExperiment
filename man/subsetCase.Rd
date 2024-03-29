% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imcExperiment-methods.R
\name{subsetCase}
\alias{subsetCase}
\alias{subsetCase,imcExperiment-method}
\title{subsets the imcExperiment to a case along with all slots for a single ROI, using for distance analysis}
\usage{
subsetCase(object, value, ...)

\S4method{subsetCase}{imcExperiment}(object, value)
}
\arguments{
\item{object}{IMC container}

\item{value}{this is ROIID a single character ID}

\item{...}{additional parameters}
}
\value{
returns IMC object of a single case

roi  imcExperiment
}
\description{
subsets the imcExperiment to a case along with all slots for a single ROI, using for distance analysis

method to subset the slots, requires colData with column "ROIID"
}
\examples{
data(imcdata)
myCase<-subsetCase(imcdata,"30-BM16-202_7Pre_s1_p1_r4_a4_ac")
myCase
data(imcdata)
myCase<-subsetCase(imcdata,"30-BM16-202_7Pre_s1_p1_r4_a4_ac")
myCase
}

\name{CFSRStationSampleData}
\alias{SampleData}
\alias{CFSRStationSampleData}
\docType{data}
\title{
Sample dataset of precipitation with CFSR and weather station data
}
\description{
Sample dataset of precipitation to be used in the function \code{\link{getAnomalies}} and then plot it by using \code{\link{plotCFSRStation}}
}
\usage{data(SampleData)}
\format{
  A data frame with 10748 observations on the following 2 variables.
  \describe{
    \item{\code{CFSR}}{a numeric vector}
    \item{\code{Station}}{a numeric vector}
  }
}
\details{
This dataset can be compiled with the data downloaded from drfuka.org service and bound with weather station data.
}
\examples{
data(SampleData)
CFSR = CFSRStationSampleData$CFSR
Station = CFSRStationSampleData$Station
}
\keyword{datasets}

\name{getAnomalies-methods}
\docType{methods}
\alias{getAnomalies-methods}
\alias{getAnomalies}
\title{ Method \code{getAnomalies}}
\description{
This method calculates the anomalies (deltas) subtracting CFSR values from weather station values.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "CFSRStationData")}}{
Based on an object of class \code{\linkS4class{CFSRStationData}} with CFSR and Station data, it calculates the anomalies (deltas) between both and returns an object of class \code{\linkS4class{CFSRStationDataExtra}}.
}
}}
\details{
This method removes all the pairs of data with any NA value.
}
\seealso{
\code{\linkS4class{CFSRStationData}}, \code{\linkS4class{CFSRStationDataExtra}}
}
\examples{
a = new("CFSRStationData", CFSR = 1:10, Station = 11:20)
b = getAnomalies(a)
}
\keyword{methods}

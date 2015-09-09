\name{plotCFSRStation-methods}
\docType{methods}
\alias{plotCFSRStation-methods}
\alias{plotCFSRStation}
\title{ Method \code{plotCFSRStation}}
\description{
This method plots the anomalies (deltas) which were obtained subtracting CFSR values from weather station values.
A scatter diagram and histogram of anomalies are showed on the plot for the variable defined as an input.
}
\section{Methods}{
\describe{

\item{\code{signature(data = "CFSRStationDataExtra")}}{
Based on an object of class \code{\linkS4class{CFSRStationDataExtra}} with CFSR, Station and Anomalies data, it plots the anomalies (deltas) which were obtained subtracting CFSR values from weather station values.
}
}}
\examples{
data(SampleData)
CFSR = CFSRStationSampleData$CFSR
Station = CFSRStationSampleData$Station

a = new("CFSRStationData", CFSR = CFSR, Station = Station)

b = getAnomalies(a)

plotCFSRStation(b, variable = "PCP")
}
\keyword{methods}

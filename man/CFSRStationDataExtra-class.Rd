\name{CFSRStationDataExtra-class}
\Rdversion{1.1}
\docType{class}
\alias{CFSRStationDataExtra-class}
\alias{plotCFSRStation,CFSRStationDataExtra-method}

\title{Class \code{"CFSRStationDataExtra"}}
\description{
Objects with this class store CFSR, Station and Anomalies data to be showed on a plot by \code{plotCFSRStation} function.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CFSRStationDataExtra", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{CFSR}:}{Object of class \code{"numeric"}}
    \item{\code{Station}:}{Object of class \code{"numeric"}}
	\item{\code{Anomaly}:}{Object of class \code{"numeric"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{CFSRStationData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{plotCFSRStation}{\code{signature(data = "CFSRStationDataExtra")}: 
	This method plots the anomalies (deltas) which were obtained subtracting CFSR values from weather station values.
	}
	}
}
\author{
Jefferson Valencia Gomez
}
\seealso{
\code{\linkS4class{CFSRStationData}}, \code{\link{getAnomalies}}, \code{\link{plotCFSRStation}}
}
\examples{
CFSR = 1:10
Station = 11:20
Anomaly = Station - CFSR
a = new("CFSRStationDataExtra", CFSR = CFSR, Station = Station, Anomaly = Anomaly)

showClass("CFSRStationDataExtra")
}
\keyword{classes}

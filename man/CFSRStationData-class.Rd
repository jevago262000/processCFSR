\name{CFSRStationData-class}
\Rdversion{1.1}
\docType{class}
\alias{CFSRStationData-class}
\alias{getAnomalies,CFSRStationData-method}

\title{Class \code{"CFSRStationData"}}
\description{
Objects with this class store CFSR and Station data to be computed by \code{getAnomalies} function. Each pair of values must refer to the same day.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CFSRStationData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{CFSR}:}{Object of class \code{"numeric"} }
    \item{\code{Station}:}{Object of class \code{"numeric"}}
  }
}
\section{Methods}{
  \describe{
    \item{getAnomalies}{\code{signature(object = "CFSRStationData")}: 
	This method calculates the anomalies (deltas) subtracting CFSR values from weather station values.
	}
	}
}
\author{
Jefferson Valencia Gomez
}
\seealso{
\code{\linkS4class{CFSRStationDataExtra}}, \code{\link{getAnomalies}}
}
\examples{
a = new("CFSRStationData", CFSR = 1:10, Station = 11:20)

showClass("CFSRStationData")
}
\keyword{classes}

\name{processSamplePoints}
\alias{processSamplePoints}
\title{
Download CFSR datasets from drfuka.org service
}
\description{
This function allows user to download CFSR datasets and cut them based on the start and end dates defined in the CSV input file.
}
\usage{
processSamplePoints(samplePointsFile, finalFolder)
}
\arguments{
  \item{samplePointsFile}{
	CSV file with the id's, coordinates (Lat and Long) and start and end dates of the points to be downloaded. See \code{extdata/SamplePoints.csv} as an example file .
}
  \item{finalFolder}{
	Folder path where all the downloaded CFSR datasets will be stored in CSV format.
}
}
\details{
Id's have to be unique. 
Lat and Long coordinates must be in decimal degrees. 
Start and end dates have to be formatted as \code{"m/d/Y"}.
}
\value{
In the \code{finalFolder} it will be stored in CSV format all the downloaded CFSR datasets. See \code{extdata/CFSRData.csv} as an example file .
}
\author{
Jefferson Valencia Gomez
}
\section{Warning }{
This function adds rows with \code{NA} values where missing data.
}

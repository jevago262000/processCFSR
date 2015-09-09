\name{convertCFSRTo}
\alias{convertCFSRTo}
\title{
Convert from CFSR to SWAT Model/WGN Excel Macro input files format
}
\description{
Function to convert CFSR datasets downloaded by using \code{\link{processSamplePoints}} function to either SWAT Model or WGN Excel Macro input files format
}
\usage{
convertCFSRTo(CSVspath, format = "SWAT", inputStartDate = FALSE, inputEndDate = FALSE)
}
\arguments{
  \item{CSVspath}{
	Folder path where stored all the CSV files downloaded by using \code{\link{processSamplePoints}} function. See \code{extdata/CFSRData.csv} as an example file.
}
  \item{format}{
	Format which datasets have to be converted to. Two possible options \code{"SWAT"} or \code{"WGN"}.
}
  \item{inputStartDate}{
	Start date to cut/trunc the data with format \code{"m/d/Y"}.
}
  \item{inputEndDate}{
	End date to cut/trunc the data with format \code{"m/d/Y"}.
}
}
\details{
If \code{format} is not defined, the CFSR datasets are converted to SWAT Model input files format. 
If either \code{inputStartDate} or \code{inputStartDate} are not defined, the start date or end date are read from the input files.
}
\value{
In the \code{CSVspath} folder it will be created another folder named either SWAT or WGN with all the created TXT files that are ready to enter to SWAT Model or WGN Excel Macro.
}
\references{
For more references related to SWAT model please visit: \href{http://swat.tamu.edu/}{SWAT Model webpage}
}
\author{
Jefferson Valencia Gomez
}
\section{Warning }{
This function adds rows with -99 values where missing data.
}
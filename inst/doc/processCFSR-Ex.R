pkgname <- "processCFSR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('processCFSR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CFSRStationData-class")
### * CFSRStationData-class

flush(stderr()); flush(stdout())

### Name: CFSRStationData-class
### Title: Class '"CFSRStationData"'
### Aliases: CFSRStationData-class getAnomalies,CFSRStationData-method
### Keywords: classes

### ** Examples

a = new("CFSRStationData", CFSR = 1:10, Station = 11:20)

showClass("CFSRStationData")



cleanEx()
nameEx("CFSRStationDataExtra-class")
### * CFSRStationDataExtra-class

flush(stderr()); flush(stdout())

### Name: CFSRStationDataExtra-class
### Title: Class '"CFSRStationDataExtra"'
### Aliases: CFSRStationDataExtra-class
###   plotCFSRStation,CFSRStationDataExtra-method
### Keywords: classes

### ** Examples

CFSR = 1:10
Station = 11:20
Anomaly = Station - CFSR
a = new("CFSRStationDataExtra", CFSR = CFSR, Station = Station, Anomaly = Anomaly)

showClass("CFSRStationDataExtra")



cleanEx()
nameEx("CFSRStationSampleData")
### * CFSRStationSampleData

flush(stderr()); flush(stdout())

### Name: CFSRStationSampleData
### Title: Sample dataset of precipitation with CFSR and weather station
###   data
### Aliases: SampleData CFSRStationSampleData
### Keywords: datasets

### ** Examples

data(SampleData)
CFSR = CFSRStationSampleData$CFSR
Station = CFSRStationSampleData$Station



cleanEx()
nameEx("getAnomalies-methods")
### * getAnomalies-methods

flush(stderr()); flush(stdout())

### Name: getAnomalies-methods
### Title: Method 'getAnomalies'
### Aliases: getAnomalies-methods getAnomalies
### Keywords: methods

### ** Examples

a = new("CFSRStationData", CFSR = 1:10, Station = 11:20)
b = getAnomalies(a)



cleanEx()
nameEx("plotCFSRStation-methods")
### * plotCFSRStation-methods

flush(stderr()); flush(stdout())

### Name: plotCFSRStation-methods
### Title: Method 'plotCFSRStation'
### Aliases: plotCFSRStation-methods plotCFSRStation
### Keywords: methods

### ** Examples

data(SampleData)
CFSR = CFSRStationSampleData$CFSR
Station = CFSRStationSampleData$Station

a = new("CFSRStationData", CFSR = CFSR, Station = Station)

b = getAnomalies(a)

plotCFSRStation(b, variable = "PCP")



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

downloadFile = function(folder, id, lat, lon, emailaddr="", timeoff=0, interppow=2) {
  
  # This is a modification of the funciton to grab daily summaries of the CFSR from the drfuka.org service
  
  options(timeout = 120, internet.info = 0)

  url = paste("http://www.cfsr.tamu-cornell.drfuka.org/swat-cfsr-v03.pl?lat=",lat,"&lon=",lon,"&timeoff=",timeoff,"&interppow=",interppow,"&.submit=Submit",sep="")

  urlline = grep("data/data", readLines(url), value=T)
  
  urlgz = strsplit(urlline,"\"")[[1]][2]
  
  fileName = paste(folder, "/Compressed/swat-cfsr-v03-point-", id, ".gz", sep = "")

  cat(paste("\nDownloading point: ", id, "\n", sep = ""))
  
  download.file(urlgz, fileName)

  return(fileName)

}


getCFSRData = function(folder, id, lat, lon) {

	compressedFileName = downloadFile(folder, id, lat, lon)
	
	exctractedFile = gzfile(compressedFileName)

	dataCFSR = read.csv(
		exctractedFile,
		header=TRUE,
		colClasses=c("character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
	)

	dataCFSR$date = as.Date(dataCFSR$date, format="%Y-%m-%d")

	#file.remove(compressFileName)

	colnames(dataCFSR)=c("DATE","TMX","TMN","PRECIP","WIND","AVGRH","MAXRH","MINRH","SOLAR")

	return(dataCFSR)

}


writeCSV = function(data, pathFile) {
	write.csv(data, file = pathFile, row.names = FALSE)
	print(paste("Writing file : ", pathFile, sep = ""))
}


fillDates = function(data, startDate, endDate) {
  
  startDate = as.Date(startDate, format = "%m/%d/%Y")
  endDate = as.Date(endDate, format = "%m/%d/%Y")
  
  seqDates = seq(startDate, to = endDate, by = '1 day')
  numDates = length(seqDates) 
  numData = nrow(data)
    
  row.names(data) = 1:numData
  
  if (numData < numDates){
    
    print("Filling missing rows with NA values")
    
    for (l in 1:numDates){
      
      if(l <= nrow(data)){
        
        if(data[l,1] != seqDates[l]){
          
          if (l == 1){
          
            data = rbind(
              c(strftime(seqDates[l],"%Y-%m-%d"), rep(NA, ncol(data)-1)),
              data[l:nrow(data),])
            
            row.names(data) = 1:nrow(data)
          }
          
          else{
            
            data = rbind(
              data[1:(l-1),],
              c(strftime(seqDates[l],"%Y-%m-%d"), rep(NA, ncol(data)-1)),
              data[l:nrow(data),])
            
            row.names(data) = 1:nrow(data)
          }
        }
      }
      else{
        data = rbind(data,c(strftime(seqDates[l],"%Y-%m-%d"), rep(NA, ncol(data)-1)))
        
        row.names(data) = 1:nrow(data)        
      }
    }
  }
 
  return(data)
}


truncByDate = function(data, startDate, endDate) {
  
  firstDate = data[1,1] 
  lastDate = data[nrow(data),1]
  
  startDate = as.Date(startDate, format = "%m/%d/%Y")
  endDate = as.Date(endDate, format = "%m/%d/%Y")
  
  iniRow = FALSE
  finRow = FALSE
  
  if (startDate < firstDate){
    iniRow = 1
  }
  
  else if (startDate > lastDate){
    warning("The start date is out of the available period of the CFSR dataset")
    return(NULL)
  }
  
  if (endDate < firstDate){
    warning("The end date is out of the available period of the CFSR dataset")
    return(NULL)
  }
  
  else if (endDate > lastDate){
    finRow = nrow(data)
  }

  if (iniRow == FALSE){
  
    for (i in 1:nrow(data)){
      if (data[i,1] >= startDate){    
        iniRow = i
        break
      }
    }
  }
  
  if (finRow == FALSE){
    
    for (i in nrow(data):1){
      if (data[i,1] <= endDate){    
        finRow = i
        break
      }
    }
  }
  
  print(paste("Truncating data for the period ", startDate, " - ", endDate, sep = ""))
  
  data = fillDates(data[iniRow:finRow,], startDate, endDate)
  
  return(data)
  
}


processSamplePoints = function(samplePointsFile, finalFolder) {

	samplePoints = read.csv(samplePointsFile)
	missingPoints = matrix(nrow = 0, ncol = 4)
  
  id = samplePoints$ID
  lat = samplePoints$LAT
  lon = samplePoints$LONG
  startDate = samplePoints$STARTDATE
  endDate = samplePoints$ENDDATE
  
	createFolders(finalFolder, "Compressed")
  numErrors = 0

  for (i in 1:length(samplePoints)){

    CFSRData = tryCatch(
		  getCFSRData(finalFolder, id[i], lat[i], lon[i]),
		  error = function(e) e
	  )
    
	  if(inherits(CFSRData,'error')){
      
      if(numErrors == 0){
        
        errorsFile = file(paste(finalFolder, "/MissingPoints.csv", sep = ""), "w")
        
        writeLines(
          paste("ID", "LAT", "LONG", "ERROR", sep=","),
          con = errorsFile,
          sep = "\n")
      }
      
      writeLines(
        paste(id[i], lat[i], lon[i], CFSRData$message, sep=","),
        con = errorsFile,
        sep = "\n")    
      
	    warning(paste("It was not possible to process the point ",id[i],"! Error: ",CFSRData$message,sep=""))
	    numErrors = numErrors + 1
	    next
	  }
	  
		CFSRData = truncByDate(CFSRData, startDate[i], endDate[i])
		writeCSV(CFSRData, paste(finalFolder, "/", id[i], ".csv", sep = ""))
  
  }
  
	if(file.exists(paste(finalFolder, "/MissingPoints.csv", sep = "")) == TRUE){
	  if(isOpen(errorsFile)){close(errorsFile)}    
	}
  
}

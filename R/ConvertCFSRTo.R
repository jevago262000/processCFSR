createFolders = function(path, folders){
  if(file.exists(path) == FALSE){
    dir.create(path)
  }
  for (f in folders){
    dir.create(file.path(path, f), showWarnings = FALSE)
  }
}


listCSVs = function(path){
  return(list.files(path = path, pattern = "*.csv", full.names = FALSE))
}


defineSufix = function(index){  
  if (index == 4){sufix = "PCP"}
  if (index == 5){sufix = "WND"}
  if (index == 6){sufix = "HMD"}
  if (index == 7){sufix = "SLR"}
  
  return(sufix)  
}


defineName = function(fileName, sufix){
  prefix = substring(fileName, 1, nchar(fileName)-4)
  if (nchar(prefix) > 5){
    name = paste(substring(prefix, 1, 5), sufix, ".txt", sep = "")
  }
  
  else{
    name = paste(prefix, sufix, ".txt", sep = "")
  }
  return(name)
}


cleanSolveData = function(CFSRData){
  dates = CFSRData[,1]
  others = CFSRData[,2:ncol(CFSRData)]
  
  others[!is.na(others)] = round(as.numeric(as.character(others[!is.na(others)])), 3)
  file = cbind(DATE = dates, others)
  file[is.na(file)] = -99
  return(file)
}


CFSRToSWAT = function(CFSRData, fileName, folder, startDate, endDate) {
  
  startDate = as.Date(startDate, format = "%m/%d/%Y")
  endDate = as.Date(endDate, format = "%m/%d/%Y")
  
  file = cleanSolveData(CFSRData)

  for (j in 2:ncol(CFSRData)){
    
    if (j == 2){ #Temperature
      
      sufix = "TMP"

      SWATFile = c(strftime(startDate, "%Y%m%d"), paste(file$TMX, file$TMN, sep = ","))
      name = defineName(fileName, sufix)
      writeLines(
        SWATFile,
        con = paste(folder, "/", name, sep = ""),
        sep = "\n", 
        useBytes = FALSE)
      
      print(paste("Writing file ", name, sep = ""))  
    }
    
    if (j >= 4){ #Other variables
      
      sufix = defineSufix(j)
      
      SWATFile = c(strftime(startDate, "%Y%m%d"), file[,sufix])
      name = defineName(fileName, sufix)
      writeLines(
        SWATFile,
        con = paste(folder,"/", name, sep = ""),
        sep = "\n",
        useBytes = FALSE)
      
      print(paste("Writing file ", name, sep = ""))  
    }  
  }
}


CFSRToWGN = function(CFSRData, fileName, folder, startDate, endDate) {
  
  startDate = as.Date(startDate, format = "%m/%d/%Y")
  endDate = as.Date(endDate, format = "%m/%d/%Y")
  
  file = cleanSolveData(CFSRData)

  for (j in 2:ncol(CFSRData)){
    
    if (j == 2){ #Temperature
      
      sufix = "TMP"

      WGNFile = c(
        "Text",
        strftime(startDate,"%m/%d/%Y"),
        strftime(endDate,"%m/%d/%Y"),
        paste(file$TMX, file$TMN, sep = ","))
      name = defineName(fileName, sufix)
      writeLines(
        WGNFile,
        con = paste(folder, "/", name, sep = ""),
        sep = "\n", 
        useBytes = FALSE)
      
      print(paste("Writing file ", name, sep = ""))  
    }
    
    if (j >= 4){ #Other variables
      
      sufix = defineSufix(j)
      
      WGNFile = c(
        "Text",
        strftime(startDate,"%m/%d/%Y"),
        strftime(endDate,"%m/%d/%Y"), 
        file[,sufix])
      name = defineName(fileName, sufix)
      writeLines(
        WGNFile,
        con = paste(folder, "/", name, sep = ""),
        sep = "\n", 
        useBytes = FALSE)
      
      print(paste("Writing file ", name, sep = ""))  
    }
  }
}


checkDates = function(inputStartDate, inputEndDate){
  if(inputStartDate == inputEndDate){
    stop("The start and end dates are the same.")
  }  
  else if(inputStartDate > inputEndDate){
    stop("The start date is greater than the end date.")
  }
}


convertCFSRTo = function(CSVspath, format = "SWAT", inputStartDate = FALSE, inputEndDate = FALSE) {
    
    if (format == "SWAT"){
      functionToProcess = CFSRToSWAT      
    }
    
    else if (format == "WGN"){
      functionToProcess = CFSRToWGN
    }
    
    else{
      stop("It is only allowed to convert to either SWAT or WGN format. The word is not valid.")
    }
    
    if(inputStartDate != FALSE && inputEndDate != FALSE){
      checkDates(inputStartDate, inputEndDate)
    }
    
    createFolders(CSVspath, format)
    CSVsFiles = listCSVs(CSVspath)
    
    for (file in CSVsFiles){

      realStartDate = NULL
      realEndDate = NULL
      
      data = read.csv(paste(CSVspath, "/", file, sep = ""))[,c(1:6,9)]
      colnames(data) = c("DATE","TMX","TMN","PRECIP","WIND","AVGRH","SOLAR")
      
      data$DATE = as.Date(data$DATE, format = "%Y-%m-%d")
      data$AVGRH = (data$AVGRH)/100 # HR in rate
      
      cat(paste("\nProcessing file ",file,"\n", sep=""))
      
      if (inputStartDate == FALSE || inputEndDate == FALSE) {     
        realStartDate = strftime(data[1,1], "%m/%d/%Y")
        realEndDate = strftime(data[nrow(data),1], "%m/%d/%Y")
        data = fillDates(data, realStartDate, realEndDate)
      }
      else{
        realStartDate = as.Date(inputStartDate, format = "%m/%d/%Y")
        realEndDate = as.Date(inputEndDate, format = "%m/%d/%Y")
        
        data = tryCatch(
          truncByDate(data, realStartDate, realEndDate),
          warning = function(war) war
        )
    
        if(inherits(data,'warning')){
          warning(paste("It was not possible to process the CSV file ",file,": ",data$message,sep=""))
          next
        }
 
      }
      
      colnames(data) = c("DATE","TMX","TMN","PCP","WND","HMD","SLR")
      
      functionToProcess(
        data, 
        file, 
        paste(CSVspath, "/", format, sep = ""), 
        realStartDate, 
        realEndDate)      
    }  
}


#Examples
#convertCFSRTo("D:/Borrar/tmp", "SWAT")
#convertCFSRTo("D:/Borrar/tmp", "WGN")
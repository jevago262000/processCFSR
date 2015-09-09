setClass(
  # Set the name for the class
  "CFSRStationData",
  
  # Define the slots
  representation(
    CFSR = "numeric",
    Station = "numeric"
  ),
  
  # Set the default values for the slots (optional)
  prototype(
    CFSR = c(),
    Station = c()
  ),
  
  validity = function(object)
  {
    x = length(object@CFSR)
    y = length(object@Station)
    
    if(x != y) {
      return("The lengths of the elements have to be equal.")
    }
    return(TRUE)
  }
)


setClass(
  # Set the name for the class
  "CFSRStationDataExtra",
  
  # Define the slots
  representation(
    Anomaly = "numeric"
    ),
  
  # Set the default values for the slots (optional)
  prototype(
    Anomaly = c()
    ),
  
  contains = "CFSRStationData"
)


setGeneric(name = "getAnomalies",
           def = function(object)
           { 
             standardGeneric("getAnomalies")
           }
)

setMethod("getAnomalies",
          signature = "CFSRStationData",
          definition = function(object)
          {
            
            theObject = data.frame(CFSR = object@CFSR, Station = object@Station)
            
            #Remove the lines in this dataframe that contain NAs across all columns
            theObject = theObject[complete.cases(theObject),]
            
            theObject$Anomaly = theObject$Station - theObject$CFSR
            finalData = new("CFSRStationDataExtra", CFSR = theObject$CFSR, Station = theObject$Station, Anomaly = theObject$Anomaly)
            return(finalData)
          }
)


defineVariable = function(variable){
  
  if (variable == "PCP"){
    var = "Precipitation"
    unit = "(mm)"
  }
  
  else if (variable == "TMAX" || variable == "TMX"){
    var = "Maximum Temperature"
    unit = "(\u00B0C)"
  }
  
  else if (variable == "TMIN" || variable == "TMN"){
    var = "Minimum Temperature"
    unit = "(\u00B0C)"
  }
  
  else if (variable == "HMD"){
    var = "Relative Humidity"
    unit = "(%)"
  }
  
  else if (variable == "SLR"){
    var = "Solar Radiation"
    unit = "(MJ/m2)"
  }
  
  else if (variable == "WND"){
    var = "Wind Speed"
    unit = "(m2/s)"
  }  
  else{
    stop("The variable was not found, please use the following conventions: PCP, TMAX, TMIN, HMD, SLR or WND.")
  }
  
  return(c(var, unit))
}


setGeneric(name = "plotCFSRStation",
           def = function(data, variable, path, name)
           { 
             standardGeneric("plotCFSRStation")
           }
)

setMethod("plotCFSRStation",
          signature = "CFSRStationDataExtra",
          definition = function(data, variable)
          {
            CFSR = data@CFSR
            station = data@Station
            anomaly = data@Anomaly
            
            num.data = length(CFSR)
            mean = mean(anomaly)
            std = sd(anomaly)
            
            letters = defineVariable(variable)
            var = letters[1]
            unit = letters[2]
            #image = paste(path, "/", name, "_", variable, ".tiff", sep="")
            
            #tiff(
              #file = image,
              #width = 1600,
              #height = 1200,
              #res = 250)
            
            old.par = par(mfrow = c(1, 2))
            par(mar = c(4,4,4,1), xpd = F) # Space between the margin and the axis c(below,left,above,right)
            
            ##Left Plot##            
            plot(
              x = 1:num.data, 
              y = anomaly, 
              cex = 0.4, 
              cex.main = 0.7, 
              cex.lab = 0.8, 
              cex.axis = 0.7, 
              xlab = "Days", 
              ylab = paste("Anomaly of ", var ," ", unit, sep = ""), 
              main = "Scatter Diagram",
              pch = 19, 
              las = 1)
            
            # Apply loess smoothing using the default span value of 0.8.  You can change the curve by changing the span value.
            y.loess = loess(y ~ x, span = 0.8, data.frame(x = 1:num.data, y = anomaly))
            
            # Compute loess smoothed values for all points along the curve
            y.predict <- predict(y.loess, data.frame(x = 1:num.data))
            
            # Plot the curve
            lines(1:num.data, y.predict, col = "red", lwd = 1.5)
            
            legend("topleft", c("Anomalies", "Smoothed curve"), col = c("black", "red"), cex = 0.5,
                   text.col = "black", lty = c(-1 , 1), pch = c(19,-1), lwd = c(-1, 1.5), merge = T, bg = 'gray90')
            
            NS = round(NSE(sim = CFSR, obs = station), 2) # Nash-Sutcliffe coefficient of efficiency
            RM = round(rmse(sim = CFSR, obs = station), 2) # Root Mean Square Error
            IA = round(d(sim = CFSR, obs = station), 2) # Index of Agreement
            MA = round(mae(sim = CFSR, obs = station), 2) # Mean Absolute Error
            
            ymax = max(anomaly)
            
            label = paste("NSE=", NS, "\n", "d=", IA, "\n", "RMSE=", RM, "\n", "MAE=", MA, sep="")
            
            legend("topright", c(paste("NSE=",NS),paste("d=", IA),paste("RMSE=",RM),paste("MAE=",MA)), cex=0.5, text.col="black", bg='white')
            
            ##Right Plot##            
            hist(
              anomaly, 
              prob = T, 
              cex.main = 0.7, 
              cex.lab = 0.8, 
              cex.axis = 0.7, 
              density = 20, 
              breaks = 20, 
              xlab = paste("Anomaly of ", var, " ", unit, sep = ""), 
              ylab = "Relative Frequency", 
              main="Histogram of Anomalies", 
              las = 1)
            
            curve(dnorm(x, mean = mean, sd = std), col = "blue", add = T, lwd = 1.5)
            box(which = "plot", lty = "solid")
            legend("topleft", "Normal curve", col = "blue", cex = 0.5, text.col = "black", lwd = 1.5, lty = 1, merge = T, bg = 'gray90')            
            par(old.par)
            
            #dev.off()
            return(image)
          }
)
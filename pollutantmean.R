pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
  fileName <- id
  
  if(id<100) {
    fileName <- paste("0",id, sep="")
  }
  
  if (id<10) {
    fileName <- paste("00",id, sep="")
  }
    
  #print (fileName)
  dataFile = read.table(paste(directory,"/",fileName,".csv", sep=""),sep=",",header=TRUE)
  
  if(pollutant == "sulfate") {
    x <- dataFile[,2,drop=FALSE]
  }
  else if (pollutant == "nitrate") {
    x <- dataFile[,3,drop=FALSE]
    
  } else {
    stop( "Invalid pollutant")
    
  }
  
  bad = is.na(x)
  good = x[!bad]
  
  round(mean(good),digits=3)
}

setwd("/users/Eric/My_Dev/coursera/rprog/rprog_airpollution")
y <- pollutantmean("specdata","sulfate",1)
y

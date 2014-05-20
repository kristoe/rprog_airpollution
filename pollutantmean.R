pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## submission by Eric Kristoff, kristoe@yahoo.com
  ## 2014-05-18
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  fooGood <- vector()
  
  for(i in id)
  {
    fileName <- i
  
    if(i<100) {
      fileName <- paste("0",i, sep="")
    }
    if (i<10) {
      fileName <- paste("00",i, sep="")
    }
    
    
    dataFile = read.table(paste(directory,"/",fileName,".csv", sep=""),sep=",",header=TRUE)
  
    if(pollutant == "sulfate") {rm
     x <- dataFile[,2,drop=FALSE]
    }
    else if (pollutant == "nitrate") {
      x <- dataFile[,3,drop=FALSE]
    } else {
     stop( "Invalid pollutant")
    }
  
   bad = is.na(x)
   good = x[!bad]
    
   fooGood <- c(good,fooGood)
  }  
  round(mean(fooGood),digits=3)
}


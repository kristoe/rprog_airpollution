corr <- function(directory, threshold = 0) {
  
  ## submission by Eric Kristoff, kristoe@yahoo.com
  ## 2014-05-18
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  options(digits=6)
  
  fileSet <- list.files(directory,"*.csv")

  
  if(length(fileSet)<1) {
    stop("No files to analyze")
  }
  
  id1 <- 1
  id2 <- length(fileSet)
  
  fileNobs <- 0
  targetMonitors <- vector()
  jc <- vector('numeric')
  goodMonitors <- 0
  
  
  for(k in id1:id2) {
    goodObs <- complete(directory,k)
    
    
    if(goodObs[1,2]>threshold) {

      
      targetMonitors <- c(targetMonitors,goodObs[1,1])
      goodMonitors <- goodMonitors+1
    }
    
  }

  
  if(goodMonitors == 0) {
    #stop("Cannot meet threshold")
  } else
  
  {

    t <- 0
    for(i in targetMonitors[])
      
    {
      
      fooSulfates <- vector()
      fooNitrates <- vector()
      
      dataFile = read.table(paste(directory,"/",fileSet[i], sep=""),sep=",",header=TRUE)
      
      for(j in 1:length(dataFile[,1]))
      {
        dataSulfates <- dataFile[j,2,drop=TRUE]
        dataNitrates <- dataFile[j,3,drop=TRUE]
        if(!is.na(dataSulfates) && !is.na(dataNitrates))
        {
          fileNobs <- fileNobs + 1
          fooSulfates <- c(dataFile[j,2,drop=TRUE],fooSulfates)
          fooNitrates <- c(dataFile[j,3,drop=TRUE],fooNitrates)
        }
      }   

      t <- t+1
      jc[t] <- round(cor(fooSulfates,fooNitrates,use="complete.obs"),5)

      rm(fooSulfates)
      rm(fooNitrates)
      
    }  
  }
  jc  
}
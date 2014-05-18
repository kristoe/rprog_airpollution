complete <- function(directory, id = 1:332) {
  
  ## submission by Eric Kristoff, kristoe@yahoo.com
  ## 2014-05-18
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  N <- ((1e3) * 2)

  fooGood <- data.frame(id=rep(NA,N), nobs=rep(NA,N))
  numGood <- 0
    
  for(i in id)
  {
    fileName <- i
    
    if(i<100) {
      fileName <- paste("0",i, sep="")
    }
    if (i<10) {
      fileName <- paste("00",i, sep="")
    }
    
    fileNobs <- 0
    
    dataFile = read.table(paste(directory,"/",fileName,".csv", sep=""),sep=",",header=TRUE)
    
    for(j in 1:length(dataFile[,1]))
    {
      dataItem1 <- dataFile[j,2,drop=TRUE]
      dataItem2 <- dataFile[j,3,drop=TRUE]
      if(!is.na(dataItem1) && !is.na(dataItem2))
      {
        fileNobs <- fileNobs + 1
      }

    }
    
    if(fileNobs >=1)
    {
      numGood <- numGood+1
      fooGood[numGood,] <- c(i,fileNobs)  
    }
      

  }
  fooBad <- is.na(fooGood[,1])
  
  x <- subset(fooGood,!fooBad)
  
  x  

}
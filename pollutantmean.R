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

  dataFrame <- data.frame()
  for (i in id) {
    ## change id to match the fileindex in the 3 digit format.
    csvfile <- paste(directory, "/", sprintf("%03d.csv", i), sep = "")
    csvfile
    dataFrame <- rbind(dataFrame, read.csv(file= csvfile, sep=",", header = TRUE, stringsAsFactors=F))
  } 
  
  mean(dataFrame[[pollutant]], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
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
  
  dataFrame <- data.frame()
  complete <- data.frame()
  for (i in id) {
    ## change id to match the fileindex in the 3 digit format.
    csvfile <- paste(directory, "/", sprintf("%03d.csv", i), sep = "")
    dataFrame <- read.csv(file= csvfile, sep=",", header = TRUE, stringsAsFactors=F)
    complete <- rbind(complete, c(i, NROW(na.omit(dataFrame))))
  }
  colnames(complete) <- c("id", "nobs")
  complete
}
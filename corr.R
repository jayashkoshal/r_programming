corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source("complete.R")
  
  dataSet <- complete(directory)
  
  id <- 1:332
  dataFrame <- data.frame()
  correlation <- numeric()
  for (i in id) {
    ## change id to match the fileindex in the 3 digit format.
    csvfile <- paste(directory, "/", sprintf("%03d.csv", i), sep = "")
    dataFrame <- read.csv(file= csvfile, sep=",", header = TRUE, stringsAsFactors=F)
    if (NROW(na.omit(dataFrame)) > threshold) {
      correlation <- c(correlation, cor(dataFrame$sulfate, dataFrame$nitrate, use = "complete.obs"))
    }
  } 
  as.numeric(correlation)
}
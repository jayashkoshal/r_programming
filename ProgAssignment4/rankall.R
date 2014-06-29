rankall <- function(outcome, ranking = "best") {
  ## Read outcome data
  outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome.df <- outcome.df[c(2, 7, 11, 17, 23)]
  outcome.df[, c(3, 4, 5)] <- sapply(outcome.df[, c(3, 4, 5)], as.numeric)

  ## Check that state and outcome are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  states <- sort(unique(outcome.df$State))
  ranks <- data.frame(hospital=NA, state=NA)
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, ranking), states[i])
  }
  
  ranks
}
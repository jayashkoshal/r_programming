best <- function(state, outcome) {
  ## Read outcome data
  outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  if (!(state %in% unique(outcome.df$State))) {
    stop("Invalid state")
  }
  
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome.state <- outcome.df[outcome.df$State == state, ]
  outcome.state[, c(11, 17, 23)] <- sapply(outcome.state[, c(11, 17, 23)], as.numeric)
  ## sort according to name
  outcome.state <- outcome.state[order(outcome.state[, 2]), ]
  
  if (outcome == "heart attack") {
    best <- outcome.state[which.min(outcome.state[, 11]), "Hospital.Name"]
  }
  else if (outcome == "heart failure") {
    best <- outcome.state[which.min(outcome.state[, 17]), "Hospital.Name"]
  }
  else {
    best <- outcome.state[which.min(outcome.state[, 23]), "Hospital.Name"]
  }
  
  best
}
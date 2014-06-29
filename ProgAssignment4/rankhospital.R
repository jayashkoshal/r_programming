rankhospital <- function(state, outcome, ranking) {
  ## Read outcome data
  outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome.df <- outcome.df[c(2, 7, 11, 17, 23)]
  outcome.df[, c(3, 4, 5)] <- sapply(outcome.df[, c(3, 4, 5)], as.numeric)
  ## Check that state and outcome are valid
  if (!(state %in% unique(outcome.df$State))) {
    stop("Invalid state")
  }
  
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Invalid outcome")
  }
  
  outcome.state <- outcome.df[outcome.df$State == state, ]
  
  if (outcome == "heart attack") {
    outcome.state <- outcome.state[order(outcome.state[, 3], outcome.state[, 1]), ]
    outcome.state <- outcome.state[!is.na(outcome.state[, 3]), ]
  } else if (outcome == "heart failure") {
    outcome.state <- outcome.state[order(outcome.state[, 4], outcome.state[, 1]), ]
    outcome.state <- outcome.state[!is.na(outcome.state[, 4]), ]
  } else {
    outcome.state <- outcome.state[order(outcome.state[, 5], outcome.state[, 1]), ]
    outcome.state <- outcome.state[!is.na(outcome.state[, 5]), ]
  }
  
  if (ranking == "best") {
    ranking <- 1L
  } else if (ranking == "worst") {
    ranking <- nrow(outcome.state)
  } else {
    ranking <- as.numeric(ranking)
  }
  
  outcome.state[ranking, 1]
}
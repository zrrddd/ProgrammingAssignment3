# Name: Rachel
# Time: 06/27/2021

library(stringr)
options(warn=-1)

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
   stop("invalid outcome") 
  }
  
  ## Return hospital name in that state with lowest 30-day death
  this.state.data <- data[data$State == state,]
  this.state.data <- this.state.data[order(this.state.data$Hospital.Name), ]
  
  outcome <- (lapply(str_split(outcome, " "), str_to_title))[[1]]
  outcome <- paste(outcome, collapse = '.')
  col.name <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome
                    , sep="")

  min.index <- which.min(this.state.data[,col.name])
  return (this.state.data[min.index, "Hospital.Name"])
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")


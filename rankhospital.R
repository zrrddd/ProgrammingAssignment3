library(stringr)
setwd("/Users/ruidi/Downloads/rprog_data_ProgAssignment3-data")
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome") 
  }  
  
  ## Return hospital name in that state with the given rank
  this.state.data <- data[data$State == state,]
  outcome <- (lapply(str_split(outcome, " "), str_to_title))[[1]]
  outcome <- paste(outcome, collapse = '.')
  col.name <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome
                    , sep="")
  
  this.state.data.valid <- this.state.data[!this.state.data[col.name]=="Not Available", ]
  this.state.data.valid[col.name] <- lapply(this.state.data.valid[col.name], as.numeric)

  this.state.data.valid <- this.state.data.valid[order(this.state.data.valid[col.name], this.state.data.valid$Hospital.Name), ]
  ## 30-day death rate
  rownames(this.state.data.valid) <- NULL
  
  if (num == 'best') {
    return (this.state.data.valid[1, "Hospital.Name"])
  } else if (num == 'worst') {
    return (this.state.data.valid[nrow(this.state.data.valid), "Hospital.Name"])
  } else {
    return (this.state.data.valid[num, "Hospital.Name"])
  }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
             
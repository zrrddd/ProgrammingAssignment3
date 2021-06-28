library(stringr)
setwd("/Users/ruidi/Downloads/rprog_data_ProgAssignment3-data")

rankall <- function(outcome, num="best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome") 
  }  
  
  ## For each state, find the hospital of the given rank
  states <- unique(data$State)
  # print(states)
  hospitals <- c()
  for (state in states) {
    temp <- rankhospital(state, outcome, num)
    hospitals <- c(hospitals, temp)
  }
  
  ## Return a dataframe with the hospital names and the state name
  data.frame(hospital=hospitals, state=states)
}

head(rankall("heart attack", 20), 10)



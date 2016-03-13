# This function takes in a state abbreviation (USA) and an outcome from the choices
# heart attack
# heart failure
# pneumonia
# and returns the hospital name of the lowest mortality rate for that condition in the state.
# Ties return the first alphabetial option.

best <- function(state, outcome) {
  
  # Read in outcome data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # Check for valid state abbreviation input using built in data set state.abb
  if(!(state %in% state.abb)) {
    stop("invalid state")
  }
  
  # Check for valid outcome input
  if(outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") {
    stop("invalid outcome")
  }
  
  # Set column number for the selected outcome
  if(outcome=="heart attack") {
    thisCol = 11
  } else if(outcome=="heart failure") {
    thisCol = 17
  } else {
    thisCol = 23
  }
  
  # Subset the chosen state and convert to numeric values
  data <- subset(data, State==state)
  data[,thisCol] <- suppressWarnings(as.numeric(data[,thisCol]))
  
  # find the minimum mortality rate and return the matching rows sorted by name
  minimum<-min(data[,thisCol], na.rm = TRUE)
  minimums<-data[which(data[,thisCol] == minimum), ]
  order(minimums$Hospital.Name)

  # return the hospital name with the lowest mortality rate
  return(minimums$Hospital.Name[1])
}

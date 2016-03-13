rankhospital <- function(state, outcome, num = "best") {
  
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
  # Remove rows with NA values
  data<-data[!is.na(data[,thisCol]),]
  
  data<-data[order(data[,thisCol], data$Hospital.Name),]
  
  
  if(num=="best") {
    return(data$Hospital.Name[1])
  } else if (num=="worst") {
    return(data$Hospital.Name[length(data$Hospital.Name)])
  } else {
    return(data$Hospital.Name[num])
  }
  
}

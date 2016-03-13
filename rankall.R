# This function takes in a rank, and an outcome from the choices
# heart attack
# heart failure
# pneumonia
# and returns the hospital name of the rank in mortality rate for that condition in each state.
# Ties return the first alphabetial option.
# states without a hospital of that rank return NA

rankall <- function(outcome, num = "best") {
  
  # Read in outcome data file, create list of all possible states in data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  states <- unique(data[,7])
  states <- states[order(states)]
  
  # Check for valid outcome input
  if(outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") {
    stop("invalid outcome")
  }
  
  # Check for valid num input, options are best, worst or a positive integer
  if(num!="best" && num!="worst") {
    if(num < 0) {
      stop("invalid rank number, options are 'best', 'worst', or a positive number")
    }
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
  state_data <- split(data, data$State)
  sorted_data <- lapply(state_data, function(x) {
    x[,thisCol]<-suppressWarnings(as.numeric(x[,thisCol]))
    x<-x[!is.na(x[,thisCol]),]
    x<-x[order(x[,thisCol], x$Hospital.Name),]
  })
  
  # Return the list of hospitals for the chosen rank
  if(num=="best") {
    data_list <- sapply(sorted_data, function(x){x$Hospital.Name[1]})
  } else if(num=="worst") {
    data_list <- sapply(sorted_data, function(x){x$Hospital.Name[length(x$Hospital.Name)]})
  } else {
    data_list <- sapply(sorted_data, function(x){x$Hospital.Name[num]})
  }
  
  # Convert the list to a common format with all states for merging
  data_frame <- data.frame(hospital=data_list, state=names(data_list), row.names=names(data_list))
  state_frame <- data.frame(state=states, row.names=states)

  # Merge the list of all states and the list of ranked hospitals
  # Reformat and add state row names
  out_data <- merge(data_frame, state_frame, by="state", all=TRUE, row.names=(states))
  out_data <- out_data[,c(2,1)]
  row.names(out_data) <- states
  
  # Return the data frame
  out_data

}

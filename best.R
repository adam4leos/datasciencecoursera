best <- function(state, outcome) {
  ## init readable values
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## validate outcome
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## read data
  data <- read.csv("meddata/outcome-of-care-measures.csv", colClasses = "character")
  
  ## grab needed columns
  data <- data[c(2, 7, 11, 17, 23)]
  ## give names to the needed columns
  names(data) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Validate the state string
  states <- unique(data[, 2])
  
  ## validate state
  if (!state %in% states) stop("invalid state")
  
  ## cleaning data to needed state and without na
  cleanData <- data[data$state == state & data[outcome] != 'Not Available', ]
  ## grabbing values
  values <- cleanData[, outcome]
  
  ## returns hospital name in that state with lowest 30-day death rate
  cleanData[which.min(values), ]$name
}
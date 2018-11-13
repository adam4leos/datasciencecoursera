rankhospital <- function(state, outcome, num) {
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
  
  ## changing the order of the data by name and outcome
  cleanData[outcome] <- as.data.frame(sapply(cleanData[outcome], as.numeric))
  cleanData <- cleanData[order(cleanData$name, decreasing = FALSE), ]
  cleanData <- cleanData[order(cleanData[outcome], decreasing = FALSE), ]
  
  ## grabbing values
  values <- cleanData[, outcome]
  
  ## finding needed row
  if (num == "best") {
    rowNum <- which.min(vals)
  } else if (num == "worst") {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ## Returns hospital name in that state with lowest 30-day death rate
  cleanData[rowNum, ]$name
}
rankall <- function(outcome, num = "best") {
  ## init readable values
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## validate outcome
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## validate num value
  if (num != "best" && num != "worst" && num%%1 != 0) stop("invalid num")
  
  ## read data
  data <- read.csv("meddata/outcome-of-care-measures.csv", colClasses = "character")
  
  ## grab needed columns
  data <- data[c(2, 7, 11, 17, 23)]
  
  ## give names to the needed columns
  names(data) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  
  ## cleaning data to be without na
  cleanData <- data[data[outcome] != 'Not Available', ]
  
  ## changing the order of the data by name and outcome
  cleanData[outcome] <- as.data.frame(sapply(cleanData[outcome], as.numeric))
  cleanData <- cleanData[order(cleanData$name, decreasing = FALSE), ]
  cleanData <- cleanData[order(cleanData[outcome], decreasing = FALSE), ]
  
  ## Helper functiont to process the num argument
  getHospByRank <- function(df, s, n) {
    df <- df[df$state==s, ]
    vals <- df[, outcome]
    
    if (n == "best") {
      rowNum <- which.min(vals)
    } else if (n == "worst") {
      rowNum <- which.max(vals)
    } else {
      rowNum <- n
    }
    
    df[rowNum, ]$name
  }
  
  ## For each state, find the hospital of the given rank
  states <- cleanData[, 2]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  
  for (st in states) {
    hosp <- getHospByRank(cleanData, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  }
  
  ## returns a data frame of hospital names with state name
  newdata[order(newdata['state'], decreasing = FALSE), ]
}
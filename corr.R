corr <- function(directory, threshold = 0) {
  completes <- subset(complete(directory, 1:332), nobs > threshold )
  
  correlations <- vector()
  
  for(file_id in completes$id ) {
    # file name should always be a three digits number, following the files given,
    # so padding with zeroes when id is less then 100
    filename = sprintf("%03d.csv", file_id)
    # since directory is passed, creating a filepath dynamically by combining filename and directory
    filepath <- paste(directory, filename, sep = "/")
    # reading a file
    file_data <- read.csv(filepath)
    # cleaning observations
    clean_observations <- file_data[complete.cases(file_data),]
    
    # if observations amount fits threshold, updating correlations
    if (nrow(clean_observations) >= threshold) {
      correlations <- c(correlations, cor(clean_observations$nitrate, clean_observations$sulfate))
    }
  }
  
  # returning correlations
  correlations
}
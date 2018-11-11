complete <- function(directory, id = 1:332) {
  ids <- vector()
  observations_amount <- vector()
  
  for (file_id in id) {
    # file name should always be a three digits number, following the files given,
    # so padding with zeroes when id is less then 100
    filename = sprintf("%03d.csv", file_id)
    # since directory is passed, creating a filepath dynamically by combining filename and directory
    filepath <- paste(directory, filename, sep = "/")
    # reading a file
    file_data <- read.csv(filepath)
    # cleaning observations
    clean_observations <- file_data[complete.cases(file_data),]
    # adding iterable id to the vector of ids
    ids <- c(ids, file_id)
    # adding iterable observations amount to the vetor of observations amount
    observations_amount <- c(observations_amount, nrow(clean_observations))
  }
  
  # returning a data frame of each id with it observations amount
  data.frame(id = ids, nobs = observations_amount)
}
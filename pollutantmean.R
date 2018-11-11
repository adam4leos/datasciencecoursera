# function to find a mean of pollutant values of given file ids in given directory
pollutantmean <- function(directory, pollutant, id = 1:332) {
  # initializing empty vetor for storing values
  total_values <- vector()
  
  # iterating trought file ids, reading one at the time
  for (file_id in id) {
    # file name should always be a three digits number, following the files given,
    # so padding with zeroes when id is less then 100
    filename = sprintf("%03d.csv", file_id)
    # since directory is passed, creating a filepath dynamically by combining filename and directory
    filepath <- paste(directory, filename, sep = "/")
    # reading a file
    file_data <- read.csv(filepath)
    # grabbing the data of needed pollutant and only which are not NA
    clean_values <- subset(file_data[,pollutant], !is.na(file_data[,pollutant,]))
    # extending total vector of values by new clean values
    total_values <- c(total_values, clean_values)
  }
  
  # returning a mean of all values
  sum(total_values) / length(total_values)
}
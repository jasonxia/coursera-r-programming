corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  nobsData <- complete(directory)
  ids <- with(nobsData, id[nobs > threshold]) 
  sapply(filenames(directory, ids), correlation, USE.NAMES = FALSE) 
}

correlation <- function(file) {
  data <- read.csv(file)
  cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs")
}
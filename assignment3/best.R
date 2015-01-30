best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  outcomeColIndex <- c(11, 17, 23)
  i <- outcomeColIndex[match(outcome, outcomes)]
  
  hospitals <- data[data$State == state, c(2, i)]
  hospitals[, 2] <- suppressWarnings(as.numeric(as.character(hospitals[, 2])))
  hospitals <- na.omit(hospitals)
  names(hospitals) <- c("name", "deaths")
  minDeaths <- min(hospitals$deaths)
  candidates <- hospitals[hospitals$deaths == minDeaths, ]$name
  
  as.character(sort(candidates)[1])
}

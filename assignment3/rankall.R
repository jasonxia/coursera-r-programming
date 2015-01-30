rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  data <- read.csv("data/outcome-of-care-measures.csv")
  
  outcomeColIndex <- c(11, 17, 23)
  i <- outcomeColIndex[match(outcome, outcomes)]
  
  hospitals <- data[, c(2, 7, i)]
  hospitals[, 3] <- suppressWarnings(as.numeric(as.character(hospitals[, 3])))
  hospitals <- na.omit(hospitals)
  names(hospitals) <- c("name", "state", "rate")

  results <- NULL
  
  for(state in levels(hospitals$state)) {
    results <- rbind(results, rank(hospitals[hospitals$state == state, ], state, num))
  }
  
  as.data.frame(results)  
}

rank <- function(hospitals, state, num) {
  if (num == "best") {
    n <- 1
  } else if (num == "worst") {
    n <- nrow(hospitals)
  } else {
    n <- as.numeric(num)
    if (is.na(num)) {
      stop("invalid num")
    } else if (num > nrow(hospitals)) {
      return(c(state = state, hospital = NA))
    }
  }

  hospital <- hospitals[order(hospitals$rate, hospitals$name), c(1, 2)][n, ]
  c(state = state, hospital = as.character(hospital$name))
}

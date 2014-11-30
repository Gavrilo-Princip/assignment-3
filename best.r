## This code reads through the "outcome-of-care-measures.csv" data file
## and returns a charater vector with the name of a Hospital with the 
## best 30-day mortality for a specified State and outcome.

	best <- function(state, outcome) {

		## Read source data data set and assign to variable "data"
 		data <- read.csv("outcome-of-care-measures.csv")

		## Assign outcome category names to variable labels
 		category <- c('heart attack', 'heart failure', 'pneumonia')
	
		## assign index value to outcome
		indices <- c(11, 17, 23)

	
		## Test for valid State
		if (!state %in% data$State) 
			stop("invalid state")

		## Test for valid outcome
		if (!outcome %in% category) 
			stop("invalid outcome")
  
  
		## Match outcome and category data to a common index
		i <- indices[match(outcome, category)]
  

		## Organize hospital by State and index
		hospitals <- data[data$State == state, c(2, i)]
  		hospitals[, 2] <- as.numeric(as.character(hospitals[, 2]))
 
		## Omit NA instances from data
 		hospitals <- na.omit(hospitals)

		## Associate hospital name and deaths to State index
  		names(hospitals) <- c("name", "deaths")
  		min_deaths <- min(hospitals$deaths)

		## Build a temporary data vector for sorting to find best Hospital 
  		temp <- hospitals[hospitals$deaths == min_deaths, ]$name
  
	return(as.character(sort(temp)[1]))
}

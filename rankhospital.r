## This program will create a function called "rankhospital" that takes 3 arguments:
## the 2- charater State name, an outcome and the ranking of a hospital in that state and outcome 
## indexed by (num) and will display the Hospital name

	
	rankhospital <- function(state, outcome, num = "best") {
    		## Read outcome data
		full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
   		## Test outcome variable for valid category
		column <- if (outcome == "heart attack") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
			} else if (outcome == "heart failure") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
			} else if (outcome == "pneumonia") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
			} else {
				stop("invalid outcome")
		}

		## Search for results by State
		data_by_state <- full_data[full_data$State == state, c("Hospital.Name", column)]

		## test for valid State
		if (nrow(data_by_state) == 0) {
			stop("invalid state")	
		}

  
		## Access data by State
  		data_by_state[,2] <- as.numeric(data_by_state[,2])
			## Sort data by State
			sorted_data_by_state <- order(data_by_state[column], data_by_state$Hospital.Name, na.last=NA)
    
		## Test for Best, Worst or any rank in between    
		if (num == "best") {
        		as.character(data_by_state$Hospital.Name[sorted_data_by_state[1]])
    			} else if (num == "worst") {
       			as.character(data_by_state$Hospital.Name[sorted_data_by_state[length(sorted_data_by_state)]])
    			} else if (is.numeric(num)) {
       			as.character(data_by_state$Hospital.Name[sorted_data_by_state[num]])
    			} else {
        		stop("invalid num")
    		}
	}

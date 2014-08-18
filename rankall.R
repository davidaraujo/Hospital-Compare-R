rankall <- function(outcome, num="best") {
  
  ## Read outcome data
  carecsv_orig <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  result_df <- data.frame(hospital=character(), state=character())
  
  ## Get all the States
  states_list <- sort(unique(carecsv$State))
  #print(states_list)
  
  ## Iterate over the States List
  for (state in states_list) {
    carecsv <- carecsv_orig[carecsv_orig$State == state,]
    
    if (outcome == "heart attack") {
      # pos 11 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
      carecsv[,11] <- as.numeric(carecsv[,11]) 
      sort_by <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (outcome == "heart failure") {
      # pos 17 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      carecsv[,17] <- as.numeric(carecsv[,17])
      sort_by <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else if (outcome == "pneumonia") {
      # pos 23 is Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      carecsv[,23] <- as.numeric(carecsv[,23])
      sort_by <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    else {
      stop("invalid outcome")
    }
    
    carecsv <- na.omit(carecsv)
    # order first by the outcome and then by Hospital.Name alphabetic
    result <- carecsv[order(carecsv[,sort_by], carecsv[,'Hospital.Name']), ]
    
    if (num == "best") {
      result_df <- rbind(result_df , data.frame('hospital' = result[1,"Hospital.Name"], 'state' = state)) 
     }
    else if (num == "worst") {
      result_df <- rbind(result_df , data.frame('hospital' = result[nrow(result),"Hospital.Name"], 'state' = state))
    }
    else {
      result_df <- rbind(result_df , data.frame('hospital' = result[num,"Hospital.Name"], 'state' = state))
    }
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(result_df)
}


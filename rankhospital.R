rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  carecsv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  carecsv <- carecsv[carecsv$State == state,]
  attack <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  failure <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  pneumonia <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  if (nrow(carecsv) == 0) {
    stop("invalid state")
  }
  if (outcome != "heart attack" || outcome != "heart failure" || outcome != "pneumonia") {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    
    # pos 11 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    carecsv[,11] <- as.numeric(carecsv[,11])
    carecsv <- na.omit(carecsv)
    result <- carecsv[order(carecsv[attack],decreasing=F),]
    
    if (num == "best") {
      return(result[1,"Hospital.Name"])  
    }
    else if (num == "worst") {
      return(result[nrow(result),"Hospital.Name"])
    }
    else
      return(result[num,"Hospital.Name"])
  }
  
  else if (outcome == "heart failure") {
    # pos 17 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    carecsv[,17] <- as.numeric(carecsv[,17])
    carecsv <- na.omit(carecsv)
    result <- carecsv[order(carecsv[failure],decreasing=F),]
    
    if (num == "best") {
      return(result[1,"Hospital.Name"])  
    }
    else if (num == "worst") {
      return(result[nrow(result),"Hospital.Name"])
    }
    else
      return(result[num,"Hospital.Name"])
  }
  
  else if (outcome == "pneumonia") {
    # pos 23 is Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    carecsv[,23] <- as.numeric(carecsv[,23])
    carecsv <- na.omit(carecsv)
    result <- carecsv[order(carecsv[pneumonia],decreasing=F),]
    
    if (num == "best") {
      return(result[1,"Hospital.Name"])  
    }
    else if (num == "worst") {
      return(result[nrow(result),"Hospital.Name"])
    }
    else
      return(result[num,"Hospital.Name"])
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}



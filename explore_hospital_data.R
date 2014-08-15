## exercice 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
colnames(outcome)
ncol(outcome) 

# histogram of the 30-day death rates from heart attack 
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[, 11])


## exercice 2
best <- function(state, outcome) {
  ## Read outcome data
  carecsv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  carecsv <- carecsv[carecsv$State == state,]
  
  if (outcome == "heart attack") {
    #heart = carecsv[,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "State")]
    #heart[,2] <- as.numeric(heart[,2])
    
    # pos 11 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    carecsv[,11] <- as.numeric(carecsv[,11])
    carecsv <- na.omit(carecsv)
    rec <- carecsv[which.min(carecsv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    print(rec$Hospital.Name)
    print(rec$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }
  else if (outcome == "heart failure") {
    # pos 17 is Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    carecsv[,17] <- as.numeric(carecsv[,17])
    carecsv <- na.omit(carecsv)
    rec <- carecsv[which.min(carecsv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    print(rec$Hospital.Name)
    print(rec$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }
  else if (outcome == "pneumonia") {
    # pos 23 is Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    carecsv[,23] <- as.numeric(carecsv[,23])
    carecsv <- na.omit(carecsv)
    rec <- carecsv[which.min(carecsv$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    print(rec$Hospital.Name)
    print(rec$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
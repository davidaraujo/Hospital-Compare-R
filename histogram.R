## exercice 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
colnames(outcome)
ncol(outcome) 

# histogram of the 30-day death rates from heart attack 
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[, 11])

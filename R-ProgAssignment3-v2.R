getwd()
setwd("D:/Users/kploessl/Documents/R/R files/R-Prog-Assignment-3/R-Prog-Assignment-3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


## You may get a warning about NAs being introduced; that is okay
hist(outcomeData[, 23])
names(outcomeData)

install.packages("sqldf")
library(sqldf)


## Read outcome data
setwd("D:/Users/kploessl/Documents/R/R files/R-Prog-Assignment-3/R-Prog-Assignment-3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
keepVars <- c(
  "Hospital.Name",
  "State",
  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
)
#keep only columns needed
outcomeData <- outcome[keepVars]
#streamline column names
names(outcomeData)[3]<-paste("HeartAttack_MortalityRate")
names(outcomeData)[4]<-paste("HeartFailure_MortalityRate")
names(outcomeData)[5]<-paste("Pneumonia_MortalityRate")
#make rates numeric
outcomeData[, 3] <- as.numeric(outcomeData[, 3])
outcomeData[, 4] <- as.numeric(outcomeData[, 4])
outcomeData[, 5] <- as.numeric(outcomeData[, 5])


best <- function(state, outcome) {
## Check that outcome is valid
  outcomeInput <- if (outcome == 'heart failure'){"HeartFailure_MortalityRate"} 
    else if (outcome == 'heart attack'){"HeartAttack_MortalityRate"} 
    else if (outcome == 'pneumonia'){"Pneumonia_MortalityRate"}
    else {
      errorMsg <- paste("Error in best(", dQuote(state), ",", dQuote(outcome), ") : invalid outcome", sep="") 
      return(errorMsg)
    }
## Check that state is valid
  if(length(subset(outcomeData, State==state, select=State))==0){
    errorMsg <- paste("Error in best(", dQuote(state), ",", dQuote(outcome), ") : invalid state", sep="")
    return(errorMsg)
  }
## subset for state
  resultSet <- subset(outcomeData, State==state)
## select hosptital name and outcome mortality rate
  keepVars2 <- c("Hospital.Name", resultSet[[outcomeInput]])
## keep only columns needed
  resultSet <- resultSet[keepVars2]
## update column name to simplify
  names(resultSet)[2]<-paste("MortalityRate")
## sort data
  resultSet <- resultSet[,order(MortalityRate,Hospital.Name)]
## choose top hospital
  result <- as.character(resultSet[1,1])
## Return hospital name in that state with lowest 30-day death
  return(result)
## rate
}

source("best.R")
best("TX", "heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
Error in best("NY", "hert attack") : invalid outcome


sqldf("select [Hospital.Name] from outcomeData where [State] =  "BB" AND [Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack] > 0 order by [Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack] , [Hospital.Name] limit 10", 
      row.names = FALSE
      )


state="BB"
outcome="heart failure"

if(length(subset(outcomeData, State==state, select=State))==0)
  
if(TRUE){
  errorMsg <- paste("Error in best(", dQuote(state), ",", dQuote(outcome), ") : invalid state",sep="")
  print(errorMsg)
  return errorMsg
}


getwd()
setwd("D:/Users/kploessl/Documents/R/R files/R-Prog-Assignment-3/R-Prog-Assignment-3")
outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomeData[, 11] <- as.numeric(outcomeData[, 11])
outcomeData[, 17] <- as.numeric(outcomeData[, 17])
outcomeData[, 23] <- as.numeric(outcomeData[, 23])

## You may get a warning about NAs being introduced; that is okay
hist(outcomeData[, 23])
names(outcomeData)

install.packages("sqldf")
library(sqldf)


best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  outcomePrefix <- "[Hospital.30.Day.Death..Mortality..Rates.from."
  outcomeInput <- if (outcome == 'heart failure'){
      paste(outcomePrefix, "Heart.Failure]", sep="")
    } else if (outcome == 'heart attack'){
      paste(outcomePrefix, "Heart.Attack]", sep="")
    } else if (outcome == 'pneumonia'){
      paste(outcomePrefix, "Pneumonia]", sep="")  
    }
  #stateInput <- dQuote(state)
  if(length(subset(outcomeData, State==state, select=State))==0){
    errorMsg <- paste("Error in best(", dQuote(state), ",", dQuote(outcome), ") : invalid state", sep="")
    return(errorMsg)
  }
  sqlStr <- paste(
    "select [Hospital.Name] from outcomeData where State =",
    sQuote(state),
    "AND",
    outcomeInput,
    "> 0 order by",
    outcomeInput,
    ",",
    "[Hospital.Name]",
    "limit 10",
    sep=" "
  )
  print(sqlStr)
  resultSet <- sqldf(sqlStr, row.names = FALSE)
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


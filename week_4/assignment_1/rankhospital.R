rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(!(state %in% df$State)) {
                stop("invalid state")
        }
        
        rates_name <- if(outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if(outcome == "pneumonia") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        df <- df[df$State == state, c("Hospital.Name", "State", rates_name)]
        df[, rates_name] <- as.numeric(df[, rates_name])
        df <- na.omit(df)
        df <- df[order(df[rates_name], df$Hospital.Name), ]
        rank <- if(num == "best") {
                1
        } else if(num == "worst") {
                nrow(df)
        } else {
                num
        }
        df[rank, "Hospital.Name"]
}
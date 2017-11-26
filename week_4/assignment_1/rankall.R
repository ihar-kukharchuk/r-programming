rankall <- function(outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        rates_name <- if(outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if(outcome == "pneumonia") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        df <- df[, c("Hospital.Name", "State", rates_name)]
        df[, rates_name] <- as.numeric(df[, rates_name])
        df <- na.omit(df)
        stats <- data.frame()
        for(i in split(df, df$State)) {
                state_df <- df[df$State == i$State, ]
                state_df <- state_df[order(state_df[rates_name], state_df$Hospital.Name), ]
                rank <- if(num == "best") {
                        1
                } else if(num == "worst") {
                        nrow(state_df)
                } else {
                        num
                }
                stats <- rbind(stats,
                               data.frame(state_df[rank, "Hospital.Name"],
                                          unique(i$State)))
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        colnames(stats) <- c("hospital", "state")
        stats
}
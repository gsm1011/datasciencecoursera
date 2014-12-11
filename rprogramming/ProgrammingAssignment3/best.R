library(dplyr)

# Finding best hospital in a state
best <- function(state, outcome) {
    dat <- read.csv("data/outcome-of-care-measures.csv", 
                    na.strings = c("Not Available"),
                    colClasses="character")
    # head(outcome)
    
    dat[, 11] <- as.numeric(dat[, 11])
    
    # Check state. 
    if(sum(state.abb == state) != 1) {
        stop("invalid state")
    }
    # Check outcome.
    if(regexpr("(heart attack|heart failure|pneumonia)", outcome, perl = TRUE) == -1) {
        stop("invalid outcome")
    }
    
    dat <- filter(dat, State == state)
    
    if (outcome == "heart attack") {
        
        rate <- select(dat, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        names(rate) <- c("Hospital.Name", "Mortality_Rate_Heart_Attack")
        rate <- filter(rate, rate$Mortality_Rate_Heart_Attack != "Not Available",
                       rate$Mortality_Rate_Heart_Attack != "NA")
        rate <- rate[order(rate$Mortality_Rate_Heart_Attack, rate$Hospital.Name),]
        
    } else if (outcome == "heart failure") {
        
        rate <- select(dat, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        names(rate) <- c("Hospital.Name", "Mortality_Rate_Heart_Failure")
        rate <- filter(rate, rate$Mortality_Rate_Heart_Failure != "Not Available",
                       rate$Mortality_Rate_Heart_Failure != "NA")
        rate <- rate[order(as.numeric(rate$Mortality_Rate_Heart_Failure), rate$Hospital.Name),]
        
    } else {
        
        rate <- select(dat, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        names(rate) <- c("Hospital.Name", "Mortality_Rate_Pneumonia")
        rate <- filter(rate, rate$Mortality_Rate_Pneumonia != "Not Available",
                       rate$Mortality_Rate_Pneumonia != "NA")
        rate <- rate[order(as.numeric(rate$Mortality_Rate_Pneumonia), rate$Hospital.Name),]
        
    }
    
    head(rate)
    rate[1,]$Hospital.Name
}
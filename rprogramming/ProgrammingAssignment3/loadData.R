library(dplyr)

# Loading data. 
loadData <- function() {
    
    outcome <- read.csv("data/outcome-of-care-measures.csv", 
                        na.strings = c("Not Available"),
                        colClasses="character")
    # head(outcome)
    
    outcome[, 11] <- as.numeric(outcome[, 11])
    # hist(outcome[, 11], col = "red")
    
    outcome
}

# Ranking hospital by outcome in a state

# Ranking hospital in all states. 


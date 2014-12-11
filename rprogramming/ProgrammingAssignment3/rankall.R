library(dplyr)

rankall <- function(outcome, num = "best") {
    dat <- read.csv("data/outcome-of-care-measures.csv", 
                    na.strings = c("Not Available"),
                    colClasses="character")
    # head(outcome)
    
    dat[, 11] <- as.numeric(dat[, 11])
    
    states <- sort(unique(dat$State))
    
    # Check outcome.
    if(regexpr("(heart attack|heart failure|pneumonia)", outcome, perl = TRUE) == -1) {
        stop("invalid outcome")
    }
    
    if (outcome == "heart attack") {
        
        rate <- select(dat, Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        names(rate) <- c("Hospital.Name", "State", "mortality_rate")
        rate <- filter(rate, rate$mortality_rate != "Not Available",
                       rate$mortality_rate != "NA")
        rate <- rate[order(rate$mortality_rate, rate$State, rate$Hospital.Name),]
        
    } else if (outcome == "heart failure") {
        
        rate <- select(dat, Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        names(rate) <- c("Hospital.Name", "State", "mortality_rate")
        rate <- filter(rate, rate$mortality_rate != "Not Available",
                       rate$mortality_rate != "NA")
        rate <- rate[order(as.numeric(rate$mortality_rate), rate$State, rate$Hospital.Name),]
        
    } else {
        
        rate <- select(dat, Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        names(rate) <- c("Hospital.Name", "State", "mortality_rate")
        rate <- filter(rate, rate$mortality_rate != "Not Available",
                       rate$mortality_rate != "NA")
        rate <- rate[order(as.numeric(rate$mortality_rate), rate$State, rate$Hospital.Name),]
        
    }

    rate <- select(rate, Hospital.Name, State)#, mortality_rate)
    names(rate) <- c("hospital", "state")#, "mortality_rate")
    
    # get the result.
    if(num == "best") {
        res <- list()
        for (s in states) {
            r <- rate[rate$state == s,][1,]
            if (ncol(r) == 0) {
                r <- c("NA", s)
            }
            res <- rbind(res, r)
        }
        #res
    } else if (num == "worst") {
        res <- list()
        for (s in states) {
            r <- rate[rate$state == s,]
            r <- tail(r, n=1)
            if (ncol(r) == 0) {
                r <- c("NA", s)
            }
            res <- rbind(res, r)
        }
        # res
    } else {
        num <- as.numeric(num)
        res <- list()
        for (s in states) {
            state_rank <- rate[rate$state == s,]
            if (nrow(state_rank) < num) {
                r <- c("<NA>", s)
            } else {
                r <- state_rank[num,]
            }
            names(r) <- c("hospital", "state")
            res <- rbind(res, r)
        }
    }
    rownames(res) <- states
    res
}
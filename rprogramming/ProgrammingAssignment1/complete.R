

complete <- function(directory, id = 1:332) {
  
  nobs <- rep(1, length(id))
  idx <- 1
  
  for (i in id) {
    f <- paste(c(directory, "/", sprintf("%03d", i), ".csv"), sep="", collapse = "")
    if(!file.exists(f)) {
      nobs[idx] <- 0
      idx <- idx + 1
      next
    }
    
    d <- read.csv(f, header=TRUE)
    nobs[idx] <- sum(complete.cases(d))
    idx <- idx + 1
  }
  
  as.data.frame(cbind(id, nobs))
  #return res
}

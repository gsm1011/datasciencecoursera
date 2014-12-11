# This is the R code for programming assignment one, part one. 

pollutantmean <- function(directory, pollutant, id = 1:332) {
  s = 0
  cnt = 0
  for (i in id) {
    f <- sprintf("%03d", i)
    
    f <- paste(c(directory, "/", f, ".csv"), sep="", collapse = "")
        
    if(!file.exists(f)) {
      next 
    }
    
    data <- read.csv(f, header=TRUE)
    
    if (pollutant == "sulfate") {
      s <- s + sum(data$sulfate, na.rm=TRUE)
      cnt <- cnt + sum(!is.na(data$sulfate))
      
    } else if (pollutant == "nitrate") {
      s <- s + sum(data$nitrate, na.rm=TRUE)  
      cnt <- cnt + sum(!is.na(data$nitrate))
    } else { next }
    
  }
  
  as.numeric(sprintf("%.3f", s / cnt))
}

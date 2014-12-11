corr <- function(directory, threshold = 0) {
  fs <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  res <- numeric() 
  
  for (f in fs) {
    d <- read.csv(f, header = TRUE)
    ccl <- complete.cases(d)
    cc <- sum(ccl)
    
    if(cc > threshold) {
      corre <- cor(d$sulfate[ccl], d$nitrate[ccl])
      res <- append(res, corre)
    }
  }

  return(as.numeric(res))
}

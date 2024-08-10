library(tidyverse) # Library

ff3 <- function(x){ # Fama & French 3 Factor Model
  
  temp <- tempfile() # Assign temporal file
  
  p <- sprintf("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/%s",
               "F-F_Research_Data_Factors_daily_CSV.zip")
  
  download.file(p, temp) # Download file
  
  f <- unzip(temp) # Unzip
  
  f <- read_csv(f, skip = 3) # Read CSV file
  
  f <- data.frame(f) # Make data frame & Change dates format
  
  f[,1] <- format(strptime(f[,1], format = "%Y%m%d"), "%Y-%m-%d") 
  
  D <- NULL # Split second column into 4 separate ones
  
  for (n in 1:nrow(f)){ # 
  
    D <- rbind.data.frame(D, unlist(strsplit(as.character(f[n,2]), "\\,"))) }
  
  D <- data.frame(f[,1], D) # Join Dates with data
  
  D <- D[-1,] # Reduce first row 
  
  colnames(D) <- c("Dates","MRKT", "SMB", "HML", "RF") # Column names
  
  R <- data.frame(rownames(x), x) # Join Dates with Portfolio Returns data
  
  colnames(R) <- c("Dates", "Returns") # Assign column names
  
  M <- merge(R, D, by = "Dates") # Merge Portfolio and Fama & French Data
  
  for (n in 2:6){ M[,n] <- as.numeric(M[,n]) } # Make all data numeric
  
  M[,2] <- M[,2] - M[,6] # Difference between Portfolio Returns and Risk Free
  
  colnames(M)[2] <- "RPRF" # Assign Column Name
  
  summary(lm(M$RPRF ~ M$MRKT + M$SMB + M$HML)) # Run regression
}
ff3(returns_df) # Test

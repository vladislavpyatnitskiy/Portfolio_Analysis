lapply(c("timeSeries", "tidyverse"), require, character.only = T) # Libs 

ff5 <- function(x){ # Fama & French 5 Factor Model
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
  
  # Add newly generated variable to data frame
  r <- rbind(r, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
  
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  r <- as.timeSeries(r) # Make it time series
  
  temp <- tempfile() # Assign temporal file
  
  p <- sprintf("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/%s",
               "F-F_Research_Data_5_Factors_2x3_daily_CSV.zip")
  
  download.file(p, temp) # Download file
  
  f <- unzip(temp) # Unzip
  
  f <- read_csv(f, skip = 3) # Read CSV file
  
  f <- data.frame(f) # Make data frame & Change dates format
  
  f[,1] <- format(strptime(f[,1], format = "%Y%m%d"), "%Y-%m-%d") 
  
  colnames(f) <- c("Dates","MRKT","SMB","HML","RMW","CMA","RF") # Column names
  
  R <- data.frame(rownames(r), r) # Join Dates with Portfolio Returns data
  
  colnames(R) <- c("Dates", "Returns") # Assign column names
  
  M <- merge(R, f, by = "Dates") # Merge Portfolio and Fama & French Data
  
  for (n in 2:8){ M[,n] <- as.numeric(M[,n]) } # Make all data numeric
  
  M[,2] <- M[,2] - M[,8] # Difference between Portfolio Returns and Risk Free
  
  colnames(M)[2] <- "RPRF" # Assign Column Name
  
  summary(lm(M$RPRF ~ M$MRKT + M$SMB + M$HML + M$RMW + M$CMA)) # Run regression
}
ff5(df_portfolio) # Test

lapply(c("timeSeries", "tidyverse"), require, character.only = T) # Libs 

ff3 <- function(x){ # Fama & French 3 Factor Model
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define dataframe with value zero
  
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
  
  R <- data.frame(rownames(r), r) # Join Dates with Portfolio Returns data
  
  colnames(R) <- c("Dates", "Returns") # Assign column names
  
  M <- merge(R, D, by = "Dates") # Merge Portfolio and Fama & French Data
  
  for (n in 2:6){ M[,n] <- as.numeric(M[,n]) } # Make all data numeric
  
  M[,2] <- M[,2] - M[,6] # Difference between Portfolio Returns and Risk Free
  
  colnames(M)[2] <- "RPRF" # Assign Column Name
  
  summary(lm(M$RPRF ~ M$MRKT + M$SMB + M$HML)) # Run regression
}
ff3(df_portfolio) # Test

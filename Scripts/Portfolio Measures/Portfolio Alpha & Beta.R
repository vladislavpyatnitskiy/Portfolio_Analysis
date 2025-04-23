lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to get alpha and beta of portfolio
p.coefficients <- function(x, benchmark = "^GSPC"){
  
  P <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  P1 <- t(P) # Transpose 
  
  colnames(P1) <- rownames(P) # Make dates as column names 
  
  R <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(P1)){ df2p <- P1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
  
    # Add newly generated variable to data frame
    R <- rbind(R, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
  
  colnames(R) <- "Returns" # Give name to column
  rownames(R) <- rownames(P) # Return dates to index
  
  x <- as.timeSeries(R) # Make it time series
  
  s <- rownames(x)[1] # First date
  e <- rownames(x)[nrow(x)] # Last date
  
  p.names <- rownames(x) # Subset dates from data set
  
  x <- data.frame(p.names, x) # Join dates with logs
  
  colnames(x) <- c("Date", "Portfolio") # Rename columns once again
  
  rownames(x) <- seq(nrow(x)) # Index for data set and join index as row names
  
  p <- NULL # Create an empty variable
  
  for (a in benchmark){ p <- cbind(p,getSymbols(a,from=s,to=e,src="yahoo",
                                                auto.assign=F)[,4]) }
  
  p <- p[apply(p,1,function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- benchmark # Put the tickers in data set
  
  r <- diff(log(as.timeSeries(p))) # Make it time series & calculate returns
  
  r[1,] <- 0 # Equal first value to zero
  
  r.names <- rownames(r) # Subset dates from data set
  
  r <- data.frame(r.names, r) # Join dates with logs
  
  colnames(r)[colnames(r) == 'r.names'] <- 'Date' # Rename column with Dates
  
  rownames(r) <- seq(nrow(p)) # Index numbers for data set & join as row names
  
  i <- as.timeSeries(merge(x,r,by = "Date")) # Join & make time series
  
  # Beta & Alpha
  B = round(apply(i[,1],2,function(col) ((lm((col)~i[,2]))$coefficients[2])),2)
  A = round(apply(i[,1],2,function(col) ((lm((col)~i[,2]))$coefficients[1])),4)
  
  sprintf("Alpha is %s, Beta is %s", format(as.numeric(A), scientific = F), B)
}
p.coefficients(df_portfolio) # Test

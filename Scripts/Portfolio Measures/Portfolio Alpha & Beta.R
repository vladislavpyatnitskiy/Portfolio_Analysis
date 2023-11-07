# Function to get alpha and beta of portfolio
p.coefficients <- function(x, benchmark = "^GSPC", string = F){
  
  s <- rownames(x)[1] # First date
  
  e <- rownames(x)[nrow(x)] # Last date
  
  p.names <- rownames(x) # Subset dates from data set
  
  x <- data.frame(p.names, x) # Join dates with logs
  
  colnames(x) <- c("Date", "Portfolio") # Rename columns once again
  
  rownames(x) <- seq(nrow(x)) # Index for data set and join index as row names
  
  p <- NULL # Create an empty variable
  
  for (a in benchmark) # Loop for data extraction
    
    p <- cbind(p,getSymbols(a,from=s,to=e,src="yahoo",auto.assign=F)[,4])
  
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
  B<-round(apply(i[,1],2,function(col) ((lm((col)~i[,2]))$coefficients[2])),2)
  A <- round(apply(i[,1],2,
                   function(col) ((lm((col)~i[,2]))$coefficients[1]))*100,2)
  
  if (isTRUE(string)) { sprintf("Portfolio Alpha is %s %%, Beta is %s", A, B)
    } else { return(rbind(A,B)) } # Choose either string or table
}
p.coefficients(returns_df, string = T) # Test

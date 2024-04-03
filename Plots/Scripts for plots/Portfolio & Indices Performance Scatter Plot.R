# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

# Scatter plot of portfolio's securities
p.scatter.plt.indices <- function(x, benchmark = "^GSPC", benchnames="S&P 500",
                                  main = NULL, xlab = NULL, ylab = NULL){
  
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
  s <- rownames(r)[1] # Start Date
  e <- rownames(r)[nrow(r)] # End Date
  
  portfolio_r_nms <- rownames(r) # Subset dates from data set
  
  r <- data.frame(portfolio_r_nms, r) # Join dates with logs
  
  colnames(r) <- c("Date", "Portfolio") # Rename columns once again
  
  rownames(r) <- index(portfolio_r_nms) # Create index numbers for data set
  
  p <- NULL # Create an empty variable
  
  for (Ticker in benchmark){ # Loop for data extraction
    
    p <- cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
  
  p <- p[apply(p,1,function(x) all(!is.na(x))),] # NA off
  
  colnames(p) <- benchnames # Put the tickers in data set
  
  p <- diff(log(as.timeSeries(p))) # Time Series Returns
  
  p[1,] <- 0 # Equal first return to 0
  
  indices_r_nms <- rownames(p) # Subset dates from data set
  
  p <- data.frame(indices_r_nms, p) # Join dates with logs
  
  colnames(p)[colnames(p) == 'indices_r_nms'] <- 'Date' # Rename column Dates
  
  rownames(p) <- index(indices_r_nms) # Create index numbers for data set
  
  i <- as.timeSeries(merge(r, p, by = "Date")) # Merge and make time series
  
  i <- t(apply(i, 2, function(col) c((exp(sum(col))-1) * 100, sd(col) * 1000)))
  
  # Plot
  ggplot(data.frame(i), mapping = aes(x = i[,2], y = i[,1])) + geom_point() +
    theme_minimal() + geom_text_repel(aes(label=rownames(i))) +
    labs(title=main,x=xlab,y=ylab)
}
# Test
p.scatter.plt.indices(df_portfolio, benchmark = c("^GSPC", "^DJI", "^IXIC"),
                      benchnames = c("S&P 500", "Dow Jones", "NASDAQ"),
                      main = "Portfolio & Indices Performance",
                      xlab = "Risk (Standard Deviation)", ylab = "Return (%)")

m.squared <- function(x, spx = "^GSPC", tr = "^TNX"){ # M squared for portfolio
  
  y <- c(tr, spx) # Join treasuries and index data
  
  s <- rownames(x)[1] # Assign first date
  
  e <- rownames(x)[nrow(x)] # Assign last date
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  i <- diff(log(p[,spx])) # make logs
  
  i[1,] <- 0 # assign first value which is NA
  
  rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free
  
  x <- merge(x,i)[-nrow(x),] # Join portfolio and index data
  
  msqr <- (exp(sum(x[,1])) - rf) * sd(x[,2])/sd(x[,1]) - (exp(sum(x[,2]) - rf))
  
  names(msqr) <- "M ^ 2" # Give name
  
  msqr # Display value
}
m.squared(returns_df) # Test

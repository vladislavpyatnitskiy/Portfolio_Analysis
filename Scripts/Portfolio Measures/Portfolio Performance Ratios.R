# Function to display performance ratios for portfolio
p.perform.ratios <- function(x, tr = "^TNX", spx = "^GSPC"){
  
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
  
  rf <- apply(p[,tr], 2, function(col) mean(col)) # Risk Free
  
  x <- merge(x,i)[-nrow(x),] # Join portfolio and index data
  
  r <- apply(x,2, function(col) c((exp(sum(col)) - 1) * 100, sd(col) * 1000))
  
  b <- apply(x[,1], 2, function(col) ((lm((col)~x[,2]))$coefficients[2]))
  
  r.mean <- apply(r, 2, function(col) mean(col)) # Calculate return
  
  sharpe <- (r[1,1] - rf) / r[2,1] # Sharpe
  
  treynor <- (r[1,1] - rf) / b / 100 # Treynor
    
  excess.r <- r[1] - r.mean[1] # Sortino
    
  Sortino <- (mean(excess.r) - rf) / mean((excess.r[excess.r < 0]) ^ 2) ^ .5
    
  perf.v <- cbind.data.frame(sharpe, treynor, Sortino) # All measures
    
  colnames(perf.v) <- c("Sharpe", "Treynor", "Sortino") # Column names
  
  return(perf.v) # Display values
}
p.perform.ratios(returns_df)

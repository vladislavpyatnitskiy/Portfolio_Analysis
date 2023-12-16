IR <- function(x, spx = "^GSPC"){ # Information Ratio
  
  y <- c(spx) # Join treasuries and index data
  
  s <- rownames(x)[1] # Assign first date
  
  e <- rownames(x)[nrow(x)] # Assign last date
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  # Make time series, logs, join portfolio and index data
  x <- merge(x,diff(log(as.timeSeries(p)[,spx])))[-nrow(x),]
  
  #(exp(sum(x[,1])) - exp(sum(x[,2][-1,]))) / sd(x[,1][-1,] - x[,2][-1,])
  mean(x[,1][-1,] - x[,2][-1,]) / sd(x[,1][-1,] - x[,2][-1,])
}
IR(returns_df) # Test

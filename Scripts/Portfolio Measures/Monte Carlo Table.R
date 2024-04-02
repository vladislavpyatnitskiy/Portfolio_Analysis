lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T) # Libraries

p.monte.table <- function(x, ndays, n){ # Table for Monte values
  
  tickers <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  p <- NULL # Assign empty variable
  
  for (a in tickers){ p<-cbind(p,getSymbols(a,src="yahoo",auto.assign=F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Reduce rows with NA
  
  colnames(p) <- tickers # Assign tickers and calculate returns
  
  l <- NULL # Empty variable & calculate returns
  
  for (m in 1:ncol(p)){ r <- as.numeric(diff(log(as.timeSeries(p[,m]))) + 1)
    
    r[1] <- 1 # Assign first observation as 1
    set.seed(0) # Calculate various scenarios of Stock Performance
    
    # Mimic Historical Performance using log returns
    v <- data.table(apply(replicate(n, expr=round(sample(r, ndays, replace=T),
                                                  2)), 2, cumprod))
    v$days <- 1:nrow(v)
    v <- melt(v, id.vars = "days") # Add to matrix
    
    l <- rbind(l, t(as.matrix(summary((v$value[v$days == ndays] - 1)*100)))) }
    
  rownames(l) <- tickers # Assign tickers for row names
  
  l # Display
}
p.monte.table(df_portfolio, 1000, 100) # Test

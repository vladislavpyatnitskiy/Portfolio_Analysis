lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

p.portion <- function(x, y, q = 1){ # potential security portion in portfolio
  
  l <- NULL
  
  for (n in 1:length(x)){ m <- q[n]
    
    if (isTRUE(weekdays(as.Date(Sys.Date())) == "Monday")){ 
      
      d <- as.Date(Sys.Date()) - 2 }
  
    if (isTRUE(weekdays(as.Date(Sys.Date())) == "Sunday")){ 
      
      d <- as.Date(Sys.Date()) - 1 }
  
    p <- getSymbols(x[n], from = d, src = "yahoo", auto.assign = F)[,4]
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    p <- as.timeSeries(p) # Make it time series
    
    l <- rbind(l, round(as.numeric(p*m/(as.numeric(y[nrow(y),ncol(y)]) + p*m)),
                        4) * 100) }
    
  colnames(l) <- "Weight (%)"
  rownames(l) <- x
  
  l # Display
}
p.portion(x = c("AMZN"), y = df_portfolio, q = c(1)) # Test

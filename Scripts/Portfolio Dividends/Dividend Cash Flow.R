p.df.dividends.cash.flow <- function(x, full=F, tax = 0){ # Dividend Cash Flow
  
  # Reduce observations without dividend cash flow
  x <- as.data.frame(x[rowSums(x[,3 * seq(ncol(x) %/% 3, from = 1)]) > 0,])
  
  l <- NULL # Data frame with columns if Date, Ticker, Dividend, Number and Sum
  
  for (n in 1:nrow(x)){ for (m in 0:((ncol(x) - 4) / 3)){ 
    
      if (isTRUE(x[n, (m * 3) + 1] != 0) && isTRUE(x[n, (m * 3) + 2] != 0) &&
          isTRUE(x[n, (m * 3) + 3] != 0)){
        
        l <- rbind.data.frame(l, cbind(rownames(x)[n], colnames(x)[(m * 3) + 1],
                                       x[n, (m * 3) + 1], x[n, (m * 3) + 2],
                                       x[n, (m * 3) + 3])) } } } 
  
  colnames(l) <- c("Date", "Ticker", "Dividend", "Number", "Total") # Columns
  
  for (n in (ncol(l) - 2):ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Turn numeric
  
  if (isTRUE(tax != 0)){ l$`After Tax` <- l[,ncol(l)] * (1 - tax) } # Tax
  
  l$`Cumulative Total` <- cumsum(l[,ncol(l)]) # Column with Cumulative Total
  
  l # Display
}
p.df.dividends.cash.flow(df_portfolio_dividend, full = F, tax = 0.1) # Test

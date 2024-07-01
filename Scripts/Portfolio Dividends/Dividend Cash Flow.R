p.df.dividends.cash.flow <- function(x, full = F){ # Dividend Cash Flow
  
  # Reduce observations without dividend cash flow
  x <- as.data.frame(x[rowSums(x[,3 * seq(ncol(x) %/% 3, from = 1)]) > 0,])
  
  l <- NULL # Data frame with columns if Date, Ticker, Dividend, Number and Sum
  s <- NULL
  
  for (n in 1:nrow(x)){ for (m in 1:ncol(x)){ while ((m + 2) != ncol(x)){
      
        if (isTRUE(x[n,m] != 0) && isTRUE(x[n,m + 1] != 0) &&
            isTRUE(x[n,m + 2] != 0)){
          
          s <- c(s, colnames(x)[m])
          
          l <- rbind.data.frame(l, cbind(rownames(x)[n],colnames(x)[m], x[n,m],
                                         x[n,m + 1], x[n,m + 2])) } 
    
        break } } } # Break operation when went through all number of columns 
  
  colnames(l) <- c("Date", "Ticker", "Dividend", "Number", "Total") # Columns
  
  for (n in (ncol(l)-2):ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Turn  numeric
  
  l$`Cumulative Total` <- cumsum(l[,ncol(l)]) # Column with Cumulative Total
  
  l # Display
}
p.df.dividends.cash.flow(df_portfolio_dividend, full = F) # Test

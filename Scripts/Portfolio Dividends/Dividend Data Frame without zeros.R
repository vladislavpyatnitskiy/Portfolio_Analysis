p.df.dividend.no.zeros <- function(x){ # Data Frame without zeros in total sum
  
  x <- x[rowSums(x[,3 * seq(ncol(x) %/% 3, from = 1)]) > 0,] # Only > 0
  
  x # Display
}
p.df.dividend.no.zeros(df_portfolio_dividend) # Test

p.df.dividend.no.zeros <- function(x){ # Data Frame without zeros in total sum
  
  x <- x[rowSums(x[,3 + 3 * seq(31, from = 0)]) > 0,] # Only > 0
  
  x # Display
}
p.df.dividend.no.zeros(df_portfolio_dividend) # Test

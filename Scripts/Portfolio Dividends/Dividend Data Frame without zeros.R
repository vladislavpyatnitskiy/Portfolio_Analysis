p.df.dividend.no.zeros <- function(x){ # Data Frame without zeros in total sum
  
  x[rowSums(x[,3 * seq(ncol(x) %/% 3, from = 1)]) > 0,] # Display only > 0
}
p.df.dividend.no.zeros(df_portfolio_dividend) # Test

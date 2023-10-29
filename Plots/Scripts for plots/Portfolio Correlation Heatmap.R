# Function to generate heatmap for portfolio's correlations
p.heatmap.plt <- function(data, s.d=as.Date(Sys.Date())-365, e.d=Sys.Date()){
  
  p.tickers <- colnames(data[,1 + 3 * seq(31, from = 0)]) # ticker names
  
  a.prices <- NULL # Create an empty variable
  
  for (Ticker in p.tickers) # Loop for data extraction
    
    # When neither start date nor end date are defined
    a.prices <- cbind(a.prices,
                      getSymbols(Ticker,
                                 from = s.d,
                                 to = e.d,
                                 src = "yahoo",
                                 auto.assign=FALSE)[,4])
  # Eliminate NAs
  a.prices<-a.prices[apply(a.prices,1,function(x) all(!is.na(x))),] 
  
  colnames(a.prices) <- p.tickers # Put the tickers in data set
  
  asset_Returns <- diff(log(as.timeSeries(a.prices)))[-1,] # Calculate returns
  
  m.correlation = as.matrix(asset_Returns) # Convert data into matrix
  
  c.correlation = ncol(m.correlation) # Get number of columns
  
  new_cor <- cor(m.correlation) # Calculate correlation coefficients
  
  # Create appropriate colour for each pair of correlation for heatmap
  k.c <- round((10 * length(unique(as.vector(new_cor))))/2)
  corrColorMatrix <- rgb(c(rep(0, k.c), seq(0, 1, length = k.c)),
                         c(rev(seq(0,1,length=k.c)), rep(0,k.c)), rep(0,2*k.c))
  # Display heatmap
  image(x = 1:c.correlation,y = 1:c.correlation,z = new_cor[, c.correlation:1],
        col = corrColorMatrix, axes = FALSE, main = "", xlab = "", ylab = "")
  
  # Add labels for both axis
  axis(2, at = c.correlation:1, labels = colnames(m.correlation), las = 2)
  axis(1, at = 1:c.correlation, labels = colnames(m.correlation), las = 2)
  
  title(main = "Portfolio Correlations Heatmap") # Add title for heatmap
  
  box() # Box heatmap
  
  # Add correlation values as text strings to each heatmap cell
  x = y = 1:c.correlation
  n_x = n_y = length(y)
  xoy = cbind(rep(x, n_y), as.vector(matrix(y, n_x, n_y, byrow = TRUE)))
  coord_for_corr = matrix(xoy, length(y) ^ 2, 2, byrow = FALSE)
  X_for_corr = t(new_cor)
  for (i in 1:c.correlation ^ 2) {
    text(coord_for_corr[i, 1], coord_for_corr[c.correlation ^ 2 + 1 - i, 2],
         round(X_for_corr[coord_for_corr[i,1],coord_for_corr[i,2]],digits = 2),
         col = "white", cex = size) }
}
# Test
p.heatmap.plt(data = df_portfolio, size =.9)

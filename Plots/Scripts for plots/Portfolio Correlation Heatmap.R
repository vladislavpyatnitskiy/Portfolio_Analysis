lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to generate heatmap for portfolio's correlations
p.heatmap.plt <- function(x, s = as.Date(Sys.Date()) - 365, e = Sys.Date(),
                          size = .9, main = NULL){
  
  p <- NULL # Create an empty variable and get stock price data
  
  for (a in colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)])
    
    p <- cbind(p, getSymbols(a, from=s, to=e, src="yahoo", auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x)%/%3+1)]
  
  m.correlation = as.matrix(diff(log(as.timeSeries(p)))[-1,]) # raturns' matrix 
  
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
  
  title(main = main) # Add title for heatmap
  
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
p.heatmap.plt(x = df_portfolio, size =.9, 
              main = "Portfolio Correlations Heatmap") # Test

lapply(c("ggplot2","data.table","timeSeries"),require,character.only=T) # Libs

p.monte.carlo <- function(x, d, n){ # Monte Function
  
  x <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Take columns with Total Sum
  
  x1 <- t(x) # Transpose x1 and x
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (m in 2:ncol(x1)){ D <- x1[,(m - 1):m] # x1 # Select two periods
  
    s <- D[apply(D, 1, function (row) all(row !=0 )),] # Remove zeros & NA
  
  # Add newly generated variable to data frame
  r <- rbind(r, log(as.numeric(colSums(s)[2]) / as.numeric(colSums(s)[1]))) }
  
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  r <- as.timeSeries(r) # Make it time series
  
  r <- as.numeric(r + 1) # Calculate returns
  r[1] <- 1 # Assign first observation as 1
  set.seed(0) # Calculate various scenarios of Stock Performance
  
  # Mimic Historical Performance using log returns
  p <- data.table(apply(replicate(n, expr = round(sample(r, d, replace=T), 2)),
                        2, cumprod))
  p$days <- 1:nrow(p)
  p <- melt(p, id.vars = "days")
  
  # Make Line Charts with all scenarios
  monte_graph <- ggplot(p, aes(x=days, y=(value - 1) * 100, col=variable)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Portfolio Performance by Monte Carlo") +
    xlab("Days Invested") + 
    ylab("Return (%)")
  
  list(monte_graph, summary((p$value[p$days == d] - 1) * 100),
       mean((p$value[p$days] - 1) * 100 < 0)) # plot & stats & mean
}
p.monte.carlo(df_portfolio, 1000, 100) # Test

p.monte.carlo <- function(c, ndays, n){ # Monte Function
  
  r <- as.numeric(c + 1) # Calculate returns
  r[1] <- 1 # Assign first observation as 1
  set.seed(0) # Calculate various scenarios of Stock Performance
  
  # Mimic Historical Performance using log returns
  p <- data.table(apply(replicate(n,expr=round(sample(r,ndays,replace=T),
                                               2)),2,cumprod))
  p$days <- 1:nrow(p)
  p <- melt(p, id.vars = "days")
  
  # Make Line Charts with all scenarious
  monte_graph <- ggplot(p, aes(x=days,y=(value - 1) * 100, col=variable)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Portfolio Performance by Monte Carlo") +
    xlab("Days Invested") + 
    ylab("Portfolio Return (%)")
  
  monte_summary <- summary((p$value[p$days == ndays] - 1) * 100) # Stats
  
  monte_mean <- mean((p$value[p$days] - 1) * 100 < 0) # Expected Return
  
  list(monte_graph, monte_summary, monte_mean) # plot & stats
}
p.monte.carlo(returns_df, 1000, 100) # Test

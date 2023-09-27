# Libraries
lapply(c("quantmod",
         "timeSeries",
         "ggplot2",
         "ggrepel"),
       require,
       character.only = TRUE)

# Function to create scatter plot for portfolio's securities
scatter_plot_portfolio <- function(x){
  
  # Select securities columns
  x <- x[,1 + 3 * seq(31, from = 0)]
  
  # Empty variable to contain values
  scatter_plot_df <- NULL
  
  # For each security in data frame
  for (n in 1:ncol(x)){
    
    # Define column
    security <- x[,n]
    
    # Define rows with zeros
    security1 <- apply(security, 1, function(row) all(row !=0 ))
    
    # Reduce rows with zeros
    security2 <- security[security1,]
    
    # Calculate logs
    security2 <- diff(log(security2))[-1,]
    
    # Calculate standard deviation
    security_sd <- sd(security2) * 1000
    
    # Calculate return
    security_return <- (exp(sum(security2)) - 1) * 100
    
    # Join standard deviation with return
    values_for_scatter <- cbind(security_sd, security_return)
    
    # Give column names to data frame
    colnames(values_for_scatter) <- c("Risk", "Return")
    
    # Give row names to data frame
    rownames(values_for_scatter) <- colnames(security)
    
    # Join measures for all tickers
    scatter_plot_df <- rbind(scatter_plot_df, values_for_scatter) 
  }
  
  # Convert to data frame for ggplot2
  scatter_plot_df <- as.data.frame(scatter_plot_df)
  
  # Plot
  ggplot(scatter_plot_df, 
         mapping = aes(x = scatter_plot_df[,1],
                              y = scatter_plot_df[,2])) +
    geom_point() +
    geom_text_repel(aes(label = rownames(scatter_plot_df))) +
    labs(title = "Securities Performance",
         x = "Security Risk (Standard Deviation)",
         y = "Security Return (%)") +
    geom_smooth(method='lm', se=FALSE, col = "red")
}
# Test
scatter_plot_portfolio(df_portfolio)

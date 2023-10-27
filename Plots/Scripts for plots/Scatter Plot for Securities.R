# Libraries
lapply(c("quantmod", "timeSeries", "ggplot2", "ggrepel"),
       require, character.only = TRUE)

# Scatter plot of portfolio's securities
p.scatter.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL){
  
  # Select securities columns
  x <- x[,1 + 3 * seq(31, from = 0)]
  
  # Empty variable to contain values
  df.scatter <- NULL
  
  # For each security in data frame
  for (n in 1:ncol(x)){ s <- x[,n]
    
    # Clean data to reduce NA and calculate return for ownership period  
    s.adj <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    # Join standard deviation with return
    v.scatter <- cbind(sd(s.adj) * 1000, (exp(sum(s.adj)) - 1) * 100)
    
    # Give column and row names to data frame
    colnames(v.scatter) <- c("Risk", "Return")
    rownames(v.scatter) <- colnames(s)
    
    # Join measures for all tickers
    df.scatter <- rbind.data.frame(df.scatter, v.scatter) }
  
  # Plot
  ggplot(df.scatter, 
         mapping = aes(x = df.scatter[,1],
                       y = df.scatter[,2])) +
    geom_point() +
    geom_text_repel(aes(label = rownames(df.scatter))) +
    labs(title = main,
         x = xlab,
         y = ylab) +
    geom_smooth(method='lm', se=FALSE, col = "red")
}
# Test
p.scatter.plt(df_portfolio, main = "Securities Performance",
              xlab = "Risk (Standard Deviation)", ylab = "Return (%)")

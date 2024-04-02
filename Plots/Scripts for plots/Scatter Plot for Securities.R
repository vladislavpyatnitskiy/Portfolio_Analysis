lapply(c("quantmod", "ggplot2", "ggrepel"), require, character.only = T) # Libs

p.scatter.plt <- function(x){ # Scatter plot of portfolio's securities
  
  x <- x[,1+3*seq(ncol(x)%/%3,from=0)][,1:(ncol(x)%/%3)]
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each security in data frame
  
    # Clean data to reduce NA and calculate return for ownership period  
    j <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    d <- rbind.data.frame(d, cbind(sd(j)*1000, (exp(sum(j))-1)*100)) } # Join
  
  # Plot
  ggplot(d, mapping = aes(x = d[,1], y = d[,2])) + geom_point() +
    geom_text_repel(aes(label = colnames(x))) + theme_minimal() +
    theme(plot.title = element_text(hjust = .5)) +
    labs(title = "Performance of Portfolio Securities",
         x = "Risk (Standard Deviation)", y = "Return (%)")
}
p.scatter.plt(df_portfolio) # Test

# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

# Scatter plot of portfolio's securities
p.scatter.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Select securities columns
  
  df.scatter <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each security in data frame
    
    # Clean data to reduce NA and calculate return for ownership period  
    s.adj <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    # Join standard deviation and return
    v.scatter <- cbind(sd(s.adj) * 1000, (exp(sum(s.adj)) - 1) * 100)
    
    rownames(v.scatter) <- colnames(s) # Give row names to data frame
    
    df.scatter <- rbind.data.frame(df.scatter, v.scatter) } # Join measures
  
  # Plot
  ggplot(df.scatter, mapping = aes(x = df.scatter[,1], y = df.scatter[,2])) +
    geom_point() + theme_minimal() + geom_smooth(method='lm',se=F,col="red") +
    geom_text_repel(aes(label=rownames(df.scatter))) +
    labs(title=main,x=xlab,y=ylab)
}
# Test
p.scatter.plt(df_portfolio, main = "Securities Performance",
              xlab = "Risk (Standard Deviation)", ylab = "Return (%)")

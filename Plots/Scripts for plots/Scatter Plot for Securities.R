# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

# Scatter plot of portfolio's securities
p.scatter.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL){
  
  x <- x[,1 + 3 * seq(31, from = 0)] # Select securities columns
  
  d <- NULL # Empty variable to contain values
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each security in data frame
  
    # Clean data to reduce NA and calculate return for ownership period  
    j <- diff(log(s[apply(s, 1, function(row) all(row !=0 )),]))[-1,]
    
    d <- rbind.data.frame(d, cbind(sd(j)*1000, (exp(sum(j))-1)*100)) } # Join
    
  # Plot
  ggplot(d, mapping = aes(x=d[,1], y=d[,2])) + geom_point() + theme_minimal() +
    geom_smooth(method = 'lm', se = F, col = "red") +
    geom_text_repel(aes(label=colnames(x))) + labs(title=main,x=xlab,y=ylab)
}
# Test
p.scatter.plt(df_portfolio, main = "Securities Performance",
              xlab = "Risk (Standard Deviation)", ylab = "Return (%)")

# library
library(treemap)

# Function to plot portfolio treemap
p.treemap <- function(x){ v <- x[,3 + 3 * seq(31, from = 0)] # Total sum
  
  colnames(v) <- colnames(x[,1 + 3 * seq(31, from = 0)]) # Names of the assets
  
  g <- c(colnames(v)) # Vector to contain names of securities
  
  v <- c(v[nrow(v),]) # Create vector to contain total sum of securities
  
  r <- x[,3 + 3 * seq(31, from = 0)]/x[,2 + 3 * seq(31, from = 0)] # Mean Sum
  
  l <- NULL # Create empty variable to contain values
  
  for (n in 1:ncol(r)){ s <- r[,n] # Return calculation
    
    l <- rbind(l, round((exp(sum(diff(log(s[!is.nan(s)]))[-1]))-1)*100,2)) }
  
  rownames(l) <- colnames(v) # Give column names
  
  p.data <- data.frame(g, v, l) # Join vectors into data set
  
  p.data$label <- sprintf("%s: %s %%", p.data$g, p.data$l) # Values in squares
  
  # Plot
  treemap(p.data,
          index=c("label"),
          vSize="v",
          type="index",
          vColor="l",
          title = "Portfolio Securities",
          title.legend = "Return")
}
# Test
p.treemap(df_portfolio)

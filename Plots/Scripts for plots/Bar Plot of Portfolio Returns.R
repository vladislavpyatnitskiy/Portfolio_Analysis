library("ggplot2") # Library

p.bar.plt.r <- function(x){ # Bar Plot of Portfolio Returns
  
  x <- data.frame(as.Date(rownames(x)), x) # Join it with main data set
  
  rownames(x) <- seq(nrow(x)) # Create sequence for index column
  
  D <- NULL # Define variable to contain values
  
  for (n in 2:ncol(x)){ s <- x[,n] # Convert daily data to monthly
  
    v <- round(tapply(s, format(as.Date(x[,1]),"%Y-%m"), sum), 4) * 100
    
    df.rownames <- rownames(v) # Take dates from index column
    
    v <- data.frame(df.rownames, v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[1] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(D)){ D <- v } else { D <- merge(x = D, y = v, by = "Date") } }
    
  D <- as.data.frame(D) # Convert to data frame format
  
  colnames(D)[2] <- "Returns" # Rename column to Returns
  
  D$fill <- ifelse(D$Returns < 0, "red3", "green4") # Colour column
  
  ggplot(D, aes(x = Date, y = Returns)) + theme_minimal() +
    geom_bar(position = "stack", stat = "identity", fill = D$fill) + 
    labs(title="Bar Plot of Portfolio Returns", x="Months", y="Returns (%)")
}
p.bar.plt.r(returns_df) # Test

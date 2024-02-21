library("ggplot2") # Library

p.bar.plt.r <- function(x){ # Bar Plot of Portfolio Returns
  
  r.rownames <- rownames(x) # Take dates from index column
  
  r.rownames <- as.Date(r.rownames) # Make it in date format
  
  x <- data.frame(r.rownames, x) # Join it with main data set
  
  rownames(x) <- seq(nrow(x)) # Create sequence for index column
  
  p.df <- NULL # Define variable to contain values
  
  for (n in 2:ncol(x)){ s <- x[,n] # Loop to make monthly data
  
    # Convert daily data to monthly
    v <- round(tapply(s, format(as.Date(x[,1]),"%Y-%m"), sum) ,4) * 100
    
    df.rownames <- rownames(v) # Take dates from index column
    
    v <- data.frame(df.rownames, v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[colnames(v) == 'df.rownames'] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(p.df)){ p.df<-v } else { p.df <- merge(x=p.df,y=v,by="Date")} }
    
  p.df <- as.data.frame(p.df) # Convert to data frame format
  
  colnames(p.df)[2] <- "Returns" # Rename column to Returns
  
  p.df$fill <- ifelse(p.df$Returns < 0, "red3", "green4") # Colour column
  
  ggplot(p.df, aes(x = Date, y = Returns, fill = fill)) + theme_minimal() +
    geom_bar(position = "stack", stat = "identity") + 
    labs(title = "Bar Plot of Portfolio Returns", x = "Months",
         y = "Returns (%)") +
    
    if (p.df[1,3] == "green4"){ # Make positive bars green and negative red
      
      scale_fill_manual(values = c("green4", "red3"), guide = F) } else {
        
      scale_fill_manual(values = c("red3", "green4"), guide = F) }
}
p.bar.plt.r(returns_df) # Test

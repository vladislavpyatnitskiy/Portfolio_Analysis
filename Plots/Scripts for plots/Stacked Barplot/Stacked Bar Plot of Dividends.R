lapply(c("ggplot2", "tidyverse"), require, character.only = T) # Libraries

# Stacked Bar Plot of Portfolio Securities Dividends
p.bar.plt.stack.dividend <- function(x, portion = F, c = "$US"){ 
  
  d <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Columns with total sum
  
  # Take column names with prices to put instead total sum column names
  colnames(d) <- colnames(x[,1+3*seq(ncol(x) %/% 3,from=0)])[-(ncol(x) %/% 3+1)]
  
  d <- d[rowSums(d) > 0,][,colSums(d) > 0] # Only > 0
  
  d <- data.frame(as.Date(rownames(d)), d) # Join date format & main data set
  
  rownames(d) <- seq(nrow(d)) # Create sequence for index column
  
  D <- NULL # Define variable to contain values
  
  for (n in 2:ncol(d)){ v <- tapply(d[,n], format(as.Date(d[,1]),"%Y-%m"), sum)
    
    v <- data.frame(rownames(v), v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[1] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(D)){ D <- v } else { D <- merge(x = D, y = v, by = "Date") } }
  
  D <- as.data.frame(D) # Convert to data frame format
  
  colnames(D) <- colnames(d) # Give column names
  
  colnames(D)[colnames(D) == colnames(D[1])] <- 'Date' # Rename again
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975") # Colour set 
  
  # Convert for better read by ggplot
  D <- D %>% pivot_longer(cols=-Date, names_to="Stock", values_to="Quantity")
  
  if (isTRUE(portion)){ # Plot showing stakes of dividends for each month
    
    ggplot(D, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "fill", stat = "identity") + 
      labs(title = "Stacked Bar Plot of Portfolio Securities Dividends",
           x = "Months", y = "Stakes (%)", fill = "Securities") +
      scale_fill_manual(values = C)   
    
  } else { # Generate plot showing amount of dividends for each month
    
    ggplot(D, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "stack", stat = "identity") + 
      labs(title = "Stacked Bar Plot of Portfolio Securities Dividends",
           x = "Months", y = sprintf("Amount in %s", c), fill = "Securities") +
      scale_fill_manual(values = C) }
}
p.bar.plt.stack.dividend(df_portfolio_dividend, portion = F) # Test

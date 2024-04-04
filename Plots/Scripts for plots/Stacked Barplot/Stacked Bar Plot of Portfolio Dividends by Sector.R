lapply(c("ggplot2", "tidyverse", "rvest"), require, character.only = T) # Libs

# Stacked Bar Plot of Portfolio Securities Dividends
p.bar.plt.stack.dividend.sector <- function(x, portion = F){ 
  
  f.df <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Columns with total sum
  
  # Take column names with prices to put instead total sum column names
  colnames(f.df) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  f.df <- f.df[rowSums(f.df) > 0,][,colSums(f.df) > 0] # Only > 0
  
  rwnms <- rownames(f.df) # Take dates from index column
  
  rwnms <- as.Date(rwnms) # Make it in date format
  
  f.df <- data.frame(rwnms, f.df) # Join it with main data set
  
  rownames(f.df) <- seq(nrow(f.df)) # Create sequence for index column
  
  p.df <- NULL # Define variable to contain values
  
  for (n in 2:ncol(f.df)){ # Convert daily data to monthly
    
    v <- tapply(f.df[,n], format(as.Date(f.df[,1]), "%Y-%m"), sum)
    
    rwmns_ds <- rownames(v) # Take dates from index column
    
    v <- data.frame(rwmns_ds, v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[colnames(v) == 'rwmns_ds'] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(p.df)){ p.df<-v } else { p.df <- merge(x=p.df,y=v,by="Date")} }
    
  p.df <- as.data.frame(p.df) # Convert to data frame format
  
  colnames(p.df) <- colnames(f.df) # Give column names
  
  colnames(p.df)[colnames(p.df) == colnames(p.df[1])] <- 'Date' # Rename again
  
  rownames(p.df) <- p.df[,1] #
  
  p.df <- t(p.df[,-1]) #
  
  y <- NULL # Create list
  
  for (n in 1:length(rownames(p.df))){ s <- rownames(p.df)[n] 
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", s, s)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    h <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    y <- rbind(y, h[grep("Sector", h) + 1]) } # Add to list
    
  colnames(y) <- "Sector" # 
  rownames(y) <- rownames(p.df) # Assign tickers
  
  p.dates <- colnames(p.df) # Assign dates as column dates
  
  l <- NULL # Create time series with sector data
  
  for (n in 1:length(colnames(p.df))){ s <- as.data.frame(p.df[,n])
    
    pie.df <- data.frame(y, s) # Join data
    
    colnames(pie.df)[2] <- "Prices" # Assign column names
    
    pie.df <- aggregate(Prices ~ Sector, data=pie.df, sum) # Conditional sum
    
    if (is.null(l)){ l <- pie.df } else { l <- merge(l,pie.df,by="Sector") } }
    
  rownames(l) <- l[,1] # Assign Sector info as row names
  
  l <- l[,-1] # Reduce sector info as it is now in row names
  
  colnames(l) <- p.dates # Assign dates as column names
  
  l <- t(l) # Transpose so dates are now row names
  
  i <- data.frame(rownames(l), l) # Join Dates with time series
  
  colnames(i)[colnames(i) == colnames(i[1])] <- 'Date' # Rename again
  
  # Colour set for plot
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975")
  
  # Convert for better read by ggplot
  i <- i %>% pivot_longer(cols=-Date, names_to="Stock", values_to="Quantity")
  
  if (isTRUE(portion)){ # Plot showing stakes of dividends for each month
    
    ggplot(i, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "fill", stat = "identity") + 
      labs(title="Stacked Bar Plot of Portfolio Dividends by Sector",
           x = "Months", y = "Stakes (%)", fill = "Sectors") +
      scale_fill_manual(values = C)   
    
  } else { # Generate plot showing amount of dividends for each month
    
    ggplot(i, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "stack", stat = "identity") + 
      labs(title="Stacked Bar Plot of Portfolio Dividends by Sector",
           x = "Months", y = "Amount in $US", fill = "Sectors") +
      scale_fill_manual(values = C) }
}
p.bar.plt.stack.dividend.sector(df_portfolio_dividend, portion = F) # Test

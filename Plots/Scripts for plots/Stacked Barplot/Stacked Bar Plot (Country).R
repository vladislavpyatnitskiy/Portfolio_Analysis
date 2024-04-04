lapply(c("ggplot2", "tidyverse", "rvest"), require, character.only = T) # Libs

p.bar.plt.stack.country <- function(x, portion=F){ # Market Cap Stacked Bar
  
  f.df <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Columns with total sum
  
  # Take column names with prices to put instead total sum column names
  colnames(f.df) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  rwnms <- rownames(f.df) # Take dates from index column
  
  rwnms <- as.Date(rwnms) # Make it in date format
  
  f.df <- data.frame(rwnms, f.df) # Join it with main data set
  
  rownames(f.df) <- seq(nrow(f.df)) # Create sequence for index column
  
  p.df <- NULL # Define variable to contain values
  
  for (n in 2:ncol(f.df)){ # Convert daily data to monthly
    
    v <- tapply(f.df[,n], format(as.Date(f.df[,1]), "%Y-%m"), median)
    
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
  
  for (n in 1:length(rownames(p.df))){ v <- rownames(p.df)[n]  # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", v, v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    c <- tab %>% html_nodes('p') # Find country character in elements
    
    l <- strsplit(toString(c), "<br>")[[1]][length(strsplit(toString(c),
                                                            "<br>")[[1]])-4]
    y <- rbind.data.frame(y, l) } # Data Frame  
    
  colnames(y) <- "Country" # 
  rownames(y) <- rownames(p.df) # Assign tickers
  
  p.dates <- colnames(p.df) # Assign dates as column dates
  
  l <- NULL # Create time series with sector data
  
  for (n in 1:length(colnames(p.df))){ s <- as.data.frame(p.df[,n])
    
    pie.df <- data.frame(y, s) # Join data
    
    colnames(pie.df)[2] <- "Prices" # Assign column names
    
    pie.df <- aggregate(Prices ~ Country, data=pie.df, sum) # Conditional sum
    
    if (is.null(l)){ l <- pie.df } else { l <- merge(l,pie.df,by="Country") } }
    
  rownames(l) <- l[,1] # Assign Sector info as row names
  
  l <- l[,-1] # Reduce sector info as it is now in row names
  
  colnames(l) <- p.dates # Assign dates as column names
  
  l <- t(l) # Transpose so dates are now row names
  
  i <- data.frame(rownames(l), l) # Join Dates with time series
  
  colnames(i)[colnames(i) == colnames(i[1])] <- 'Date' # Rename again
  
  i <- i %>% pivot_longer(cols=-Date, names_to="Stock", values_to="Quantity")
  
  if (isTRUE(portion)){ # Plot showing stakes of securities for each month
    
    ggplot(i, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "fill", stat = "identity") + 
      labs(title = "Stacked Bar Plot of Portfolio Securities by Country",
           x = "Months", y = "Stakes (%)", fill = "Securities")
    
  } else { # Generate plot showing amount of securities for each month
    
    ggplot(i, aes(x = Date, y = Quantity, fill = Stock)) + theme_minimal() +
      geom_bar(position = "stack", stat = "identity") + 
      labs(title = "Stacked Bar Plot of Portfolio Securities by Country",
           x = "Months", y = "Amount in $US", fill = "Securities") }
}
p.bar.plt.stack.country(df_portfolio, portion = T) # Test

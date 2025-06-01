lapply(c("ggplot2", "tidyverse", "rvest", "xml2", "httr"),
       require, character.only = T) # Libs

p.bar.plt.stack.sector <- function(x, portion=F){ # Stacked Bar Plot by Sectors
  
  D <- x[,3 * seq(ncol(x) %/% 3, from = 1)] # Columns with total sum
  
  # Take column names with prices to put instead total sum column names
  colnames(D) <- colnames(x[,1+3*seq(ncol(x)%/%3,from=0)])[-(ncol(x)%/%3+1)]
  
  rwnms <- rownames(D) # Take dates from index column
  
  rwnms <- as.Date(rwnms) # Make it in date format
  
  D <- data.frame(rwnms, D) # Join it with main data set
  
  rownames(D) <- seq(nrow(D)) # Create sequence for index column
  
  DF <- NULL # Define variable to contain values
  
  for (n in 2:ncol(D)){ v = tapply(D[,n],format(as.Date(D[,1]),"%Y-%m"),median)
    
    rwmns_ds <- rownames(v) # Take dates from index column
    
    v <- data.frame(rwmns_ds, v) # Join with new data set
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[colnames(v) == 'rwmns_ds'] <- 'Date' # Name column as Date
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(DF)){ DF = v } else { DF = merge(x=DF, y=v, by="Date") } }
  
  DF <- as.data.frame(DF) # Convert to data frame format
  
  colnames(DF) <- colnames(D) # Give column names
  
  colnames(DF)[1] <- 'Date' # Rename again
  
  rownames(DF) <- DF[,1] #
  
  DF <- t(DF[,-1]) #
  
  y <- NULL # Create list
  
  for (n in 1:length(rownames(DF))){ s <- rownames(DF)[n] 
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    R <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s),
             add_headers(`User-Agent` = B))
    
    f <- read_html(content(R, as = "text", encoding = "UTF-8")) %>%
      html_nodes('div') %>% html_nodes('dl') %>% html_nodes('dd') %>%
      html_nodes('strong') %>% html_text() %>% .[1] 
    
    y <- rbind.data.frame(y, f) } 
    
  colnames(y) <- "Sector" # 
  rownames(y) <- rownames(DF) # Assign tickers
  
  p.dates <- colnames(DF) # Assign dates as column dates
  
  l <- NULL # Create time series with sector data
  
  for (n in 1:length(p.dates)){ SUM <- data.frame(y, as.data.frame(DF[,n])) 
    
    colnames(SUM)[2] <- "Prices" # Assign column names
    
    SUM <- aggregate(Prices ~ Sector, data = SUM, sum) # Conditional sum
    
    if (is.null(l)){ l <- SUM } else { l <- merge(l, SUM, by = "Sector") } }
    
  rownames(l) <- l[,1] # Assign Sector info as row names
  
  l <- l[,-1] # Reduce sector info as it is now in row names
  
  colnames(l) <- p.dates # Assign dates as column names
  
  l <- t(l) # Transpose so dates are now row names
  
  i <- data.frame(rownames(l), l) # Join Dates with time series
  
  colnames(i)[1] <- 'Date' # Rename again
  
  i <- i %>% pivot_longer(cols=-Date, names_to="Stock", values_to="Quantity")
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a")
  
  if (portion){ l <- c("fill", "Stakes") } # Portions
  
  else { l <- c("stack", "Amount in $US") } # By Value
  
  ggplot(
    i,
    aes(
      x = Date,
      y = Quantity,
      fill = Stock)
    ) +
    theme_minimal() +
    geom_bar(position = l[1], stat = "identity") +
    scale_fill_manual(values = C) +
    labs(
      title = "Stacked Bar Plot of Portfolio Securities by Sectors",
      x = "Months",
      y = l[2],
      fill = "Securities"
      ) 
}
p.bar.plt.stack.sector(df_portfolio, portion = T) # Test

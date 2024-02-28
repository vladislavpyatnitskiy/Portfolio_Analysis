p.calendar <- function(x){ # Table of Portfolio Returns
  
  f.df <- x   # Assign first log as 0
  
  r.rownames <- rownames(f.df) # Take dates from index column
  
  r.rownames <- as.Date(r.rownames) # Make it in date format
  
  f.df <- data.frame(r.rownames, f.df) # Join it with main data set
  
  rownames(f.df) <- seq(nrow(f.df)) # Create sequence for index column
  
  p.df <- NULL # Define variable to contain values
  
  for (n in 2:ncol(f.df)){ s <- f.df[,n] # Loop to make monthly data
  
  # Convert daily data to monthly
  v <- round(tapply(s, format(as.Date(f.df[,1]),"%Y-%m"), sum) ,4) * 100
  
  df.rownames <- rownames(v) # Take dates from index column
  
  v <- data.frame(df.rownames, v) # Join with new data set
  
  rownames(v) <- seq(nrow(v)) # Generate sequence for index column
  
  colnames(v)[colnames(v) == 'df.rownames'] <- 'Date' # Name column as Date
  
  # If defined empty variable is still empty # Put new dataset there
  if (is.null(p.df)){ p.df<-v } else { p.df <- merge(x=p.df,y=v,by="Date")} }
  
  p.df <- as.data.frame(p.df) # Convert to data frame format
  
  colnames(p.df) <- colnames(f.df) # Give column names
  
  colnames(p.df)[colnames(p.df) == colnames(p.df[1])] <- 'Date'
  
  ticker <- sprintf("%s (%%)", colnames(p.df)[2])
  
  d.y <- as.data.frame(substr(p.df[,1],1,4))
  d.m <- as.data.frame(substr(p.df[,1],6,7))
  
  p.df <- p.df[,-1]
  
  p.df <- data.frame(d.y, d.m, p.df)
  
  colnames(p.df) <- c("Year", "Month", ticker)
  
  #p.df
  l <- NULL # Data Frame for Joined Year columns
  
  for (m in 1:length(unique(p.df[,1]))){ # 
    
    l1 <- as.data.frame(sort(unique(p.df[,2]))) # Sort months ascendingly
    
    l2 <- p.df[p.df[,1] == unique(p.df[,1])[m],] # First unique year
    
    colnames(l1) <- "Month" # Give column name to Data Frame of months
    
    l3 <- merge(l2, l1, by = "Month", all = T) # Merge months Data Frames
    
    l3 <- l3[,c("Year","Month",colnames(p.df)[3])] # Column names to DF
    
    if (isTRUE(any(is.na(l3)))){ # Give name to year observation with NA
      
      l3[is.na(l3[,1]),][,1] <- p.df[p.df[,1]==unique(p.df[,1])[m],][1,1]}
    
    l3 <- l3[,-1] # Delete Year Column 
    
    # Assign Year number name as column name of returns
    colnames(l3)[2] <- p.df[p.df[,1] == unique(p.df[,1])[m],][1,1]
    
    # Data Frame with months
    m1 <- data.frame(l3[,1], c("January", "February", "March", "April",
                               "May", "June", "July", "August", "September",
                               "October", "November", "December"))
    
    colnames(m1) <- c("Month", "Months") # Column names for numbers and names
    
    l4 <- merge(m1, l3, by = "Month") # Join Months and returns by numbers
    
    l4 <- l4[,-1] # Reduce column with number of months instead of names
    
    m2 <- l4[,1] # Assign month column to new variable
    
    l4 <- as.data.frame(l4[,-1]) # Reduce excessive month column
    
    rownames(l4) <- m2 # Months as row names and year as column name
    colnames(l4) <- p.df[p.df[,1] == unique(p.df[,1])[m],][1,1] 
    
    if (is.null(l)){ l <- l4 } else { l <- cbind(l, l4) } } # Join
  
  # Median and Mean for each month
  l$Median <- round(apply(l, 1, median, na.rm = T), 2)
  l$Mean <- round(apply(l[,1:(ncol(l)-1)], 1, mean, na.rm = T), 2)
  
  # Sum, Median and Mean for each year
  l[nrow(l) + 1,] = round(apply(l, 2, sum, na.rm = T), 2)
  l[nrow(l) + 1,] = round(apply(l[1:(nrow(l)-1),], 2, median, na.rm = T), 2)
  l[nrow(l) + 1,] = round(apply(l[1:(nrow(l)-2),], 2, mean, na.rm = T), 2)
  
  rownames(l)[(nrow(l) - 2):nrow(l)] <- c("Sum", "Median", "Mean") # Names
  
  l[nrow(l), ncol(l)] <- "XX"
  l[nrow(l) - 1, ncol(l)] <- "XX"
  l[nrow(l), ncol(l) - 1] <- "XX"
  l[nrow(l) - 1, ncol(l)-1] <- "XX"
  l[nrow(l) - 2, ncol(l) - 1] <- "XX"
  l[nrow(l) - 2, ncol(l)] <- "XX"
  
  l
}
p.calendar(returns_df) # Test

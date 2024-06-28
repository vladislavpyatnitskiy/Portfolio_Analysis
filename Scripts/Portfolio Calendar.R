library("timeSeries") # Library

p.calendar <- function(x){ # Calendar of Portfolio Returns
  
  x <- x[,3*seq(ncol(x)%/%3,from=1)] # Subtract total values for each stock
  
  x1 <- t(x) # Transpose data frame
  
  colnames(x1) <- rownames(x) # Make dates as column names x1 and x
  
  r <- as.data.frame(0) # Define data frame with value zero
  
  # Loop for portfolio log returns calculation
  for (n in 2:ncol(x1)){ df2p <- x1[,(n-1):n] # x1 # Select two periods
  
    s <- df2p[apply(df2p, 1, function(row) all(row !=0 )),] # Remove zeros & NA
    
    r <- rbind(r, log(colSums(s)[2]/colSums(s)[1])) } # Add value to data frame
  
  colnames(r) <- "Returns" # Give name to column
  rownames(r) <- rownames(x) # Return dates to index
  
  r <- as.timeSeries(r) # Make it time series
  
  r <- data.frame(rownames(r), r) # Subtract dates and Join it with data set
  
  rownames(r) <- seq(nrow(r)) # Create sequence for index column
  
  D <- NULL # Define variable to contain values
  
  for (n in 2:ncol(r)){ # Loop to convert daily data to monthly
    
    v <- round(tapply(r[,n], format(as.Date(r[,1]), "%Y-%m"), sum), 4) * 100
    
    v <- data.frame(rownames(v), v) # Take dates from index column and join
    
    rownames(v) <- seq(nrow(v)) # Generate sequence for index column
    
    colnames(v)[1] <- 'Date' # Name columns as Date and ticker name
    colnames(v)[2] <- colnames(x)[n]
    
    # If defined empty variable is still empty # Put new dataset there
    if (is.null(D)){ D <- v } else { D <- merge(x = D, y = v, by = "Date") } }
    
  ticker <- sprintf("%s (%%)", colnames(D)[2]) # Column name for return
    
  Y <- as.data.frame(substr(D[,1], 1, 4)) # Subtract Year Value
  M <- as.data.frame(substr(D[,1], 6, 7)) # Subtract Month Value
    
  D <- D[,-1] # Reduce first column containing both year and month values
    
  D <- data.frame(Y, M, D) # Join data with Years, Months and Returns
    
  colnames(D) <- c("Year", "Month", ticker) # Columns with Year, Month & Return
    
  C <- NULL # Data Frame for Joined Year columns
    
  # Firstly, Sort months in an ascending way
  for (m in 1:length(unique(D[,1]))){ l <- as.data.frame(sort(unique(D[,2]))) 
      
    y <- D[D[,1] == unique(D[,1])[m],] # Subtract unique years
      
    colnames(l) <- "Month" # Give column name to Data Frame of months
      
    d <- merge(y, l, by = "Month", all = T) # Merge months Data Frames
      
    d <- d[,c("Year", "Month", colnames(D)[3])] # Column names to DF
      
    if (isTRUE(any(is.na(d)))){ # Give name to year observation with NA
        
      d[is.na(d[,1]),][,1] <- D[D[,1] == unique(D[,1])[m],][1,1] }
      
    d <- d[,-1] # Delete Year Column 
      
    # Assign Year number name as column name of returns
    colnames(d)[2] <- D[D[,1] == unique(D[,1])[m],][1,1]
      
    m1 <- data.frame(d[,1], month.name) # Data Frame with months
      
    colnames(m1) <- c("Month", "Months") # Column names for numbers and names
      
    l4 <- merge(m1, d, by = "Month") # Join Months and returns by numbers
      
    l4 <- l4[,-1] # Reduce column with number of months instead of names
      
    m2 <- l4[,1] # Assign month column to new variable
      
    l4 <- as.data.frame(l4[,-1]) # Reduce excessive month column
      
    rownames(l4) <- m2 # Months as row names and year as column name
    colnames(l4) <- D[D[,1] == unique(D[,1])[m],][1,1] 
      
    if (is.null(C)){ C <- l4 } else { C <- cbind(C, l4) } } # Join Years
    
  C$Median <- round(apply(C, 1, median, na.rm = T), 2) # Monthly Median
  C$Mean <- round(apply(C[,1:(ncol(C)-1)], 1, mean, na.rm=T), 2) # Monthly Mean
  
  g <- list(c(0, 1), c(0, 1, 2), list(sum, median, mean)) # List with values
  
  for (n in 1:length(g[[3]])){ # Calculate Sum, Median and Mean for each year
    
    C[nrow(C) + 1,] = round(apply(C[1:(nrow(C) - g[[2]][n]),], 2, g[[3]][[n]],
                                  na.rm = T), 2) }
  
  for (n in 1:length(g[[3]])){ for (m in 1:length(g[[1]])){ # Cut unnecessary
    
      C[nrow(C) - g[[2]][n], ncol(C) - g[[1]][m]] <- "" } } # Values
  
  rownames(C)[(nrow(C) - 2):nrow(C)] <- c("Sum", "Median", "Mean") # Names
  
  C # Display
}
p.calendar(df_portfolio) # Test

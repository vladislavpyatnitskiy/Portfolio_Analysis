# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Function creating Portfolio DataFrame using nested list fron
# Securities info entry.R 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

p.dataframe.builder <- function(x){ x <- as.data.frame(do.call(rbind, x))
  
  df <- NULL # Variable to contain values
  
  for (m in seq(x[,1])){ # For each asset
    
    v <- x[[1]][[m]] # Ticker
    s <- x[[2]][[m]] # Start date
    e <- x[[3]][[m]] # End date
    q <- x[[4]][[m]] # Asset number
    l <- length(s) # Length of Dates and Numbers
    
    ts <- NULL # Create variable to contain data of single asset
    
    for (n in seq(l)){ # Extend time series and add asset number
      
      ts <- rbind(ts, data.frame(Date=seq.Date(from=as.Date(s[[n]]),
                                               to = as.Date(e[[n]]),by="day"),
                                 Number = q[[n]]))}
    # Get data 
    p <- getSymbols(v,from=s[[1]],to=e[[l]],src="yahoo",auto.assign=F)[,4]
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # NA off
    
    colnames(p) <- v # Put the tickers in data set
    
    # Subset dates from data set and transform them into a date format
    dates_fr_yh <- as.Date(rownames(as.timeSeries(p)))
    
    # Merge asset values with its dates
    ds_from_yahoo <- data.frame(dates_fr_yh, as.timeSeries(p))
    
    # Change column name to Date
    colnames(ds_from_yahoo)[colnames(ds_from_yahoo) == 'dates_fr_yh'] <- 'Date'
    
    # Create index numbers for data set and join as row names
    rownames(ds_from_yahoo) <- seq(nrow(ds_from_yahoo))
    
    # merge actual ownership period with
    f.df <- merge(x = ds_from_yahoo, y = ts, by = c("Date"))
    
    f.df$total_sum <- f.df[,2] * f.df$Number # Total sum of asset
    
    # Add Ticker to Number
    colnames(f.df)[colnames(f.df) == 'Number'] <- sprintf("%s Number", v)
    
    # Add Ticker to Total
    colnames(f.df)[colnames(f.df) == 'total_sum'] <- sprintf("%s Total", v)
    
    # If it is first column, define it to new name or merge with previous
    if (is.null(df)){ df = f.df } else { df <- merge(x = df, y = f.df,
                                                     by = "Date", all = T) } }
  
  df[is.na(df)] <- 0 # Substitute NA with Zero values
  
  rownames(df) <- df$Date # Put Dates in index
  
  df <- df[,-1] # Subset Time Series from data set 
  
  # Total amount of investments
  df$Total <- rowSums(df[,c(seq(ncol(df) / 3) * 3)], na.rm = T)
  
  df <- as.timeSeries(df) # Make it time series
  
  return(df) # Display values
}
p.dataframe.builder(nest.df) # Test

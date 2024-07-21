positions.bulider.df <- function(x){ # Data Frame of Positions of Nested list
  
  L <- NULL #
  
  for (n in 1:length(x)){ l <- NULL # For each ticker with info 
    
    for (m in 1:length(x[[n]][[2]])){ # for all positions change
      
      if (isTRUE(length(x[[n]][[2]]) >= 2) && isTRUE(m >= 2)){ # changes > 2
        
        if (x[[n]][[4]][[m]] - x[[n]][[4]][[m-1]] > 0){ # positive change
        
          l <- rbind.data.frame(l, cbind(x[[n]][[2]][[m]], x[[n]][[3]][[m]],
                                         x[[n]][[4]][[m]]-x[[n]][[4]][[m-1]]))
        
        } else { # When next number of position is fewer than previous
          
          l <- rbind.data.frame(l,
                                cbind(x[[n]][[2]][[m]], x[[n]][[3]][[m]],
                                      x[[n]][[4]][[m-1]] - x[[n]][[4]][[m]])) }
      
        } else { l <- rbind.data.frame(l, cbind(x[[n]][[2]][[m]],
                                                x[[n]][[3]][[m]],
                                                x[[n]][[4]][[m]])) } }
  
    L <- rbind.data.frame(L, cbind(x[[n]][[1]], l)) } # Join Ticker with Data
  
  colnames(L) <- c("Ticker", "Start Date", "End Date", "Number") # Column Names
  
  L # Display
}
positions.bulider.df(nest.df) # Test

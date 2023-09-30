# library
library(treemap)

# Function to plot portfolio treemap
portfolio_treemap3 <- function(x){
  
  # Take total values
  t_m_values <- x[,3 + 3 * seq(31, from = 0)]
  
  # Give them names of the assets
  colnames(t_m_values) <- colnames(x[,1 + 3 * seq(31, from = 0)])
  
  # Create vector to contain names of securities
  t_m_group <- c(colnames(t_m_values))
  
  # Create vector to contain total sum of securities
  t_m_value <- c(t_m_values[nrow(t_m_values),])
  
  #
  names_for_treemap <- colnames(x[,1 + 3 * seq(31, from = 0)])
  
  # Subset values with secuirities' values
  new_s_v <- x[,3 + 3 * seq(31, from = 0)] / x[,2 + 3 * seq(31, from = 0)]
  
  # Create empty variable to contain values
  values_for_brplt <- NULL
  
  # For each column in data set
  for (n in 1:ncol(new_s_v)){
    
    # Values for security
    security <- new_s_v[,n]
    
    # Find zeros in data set
    security2 <- security[!is.nan(security)]
    
    # Calculate logs
    security2 <- diff(log(security2))[-1]
    
    # Calculate return for the ownership period
    security2 <- round((exp(sum(security2)) - 1) * 100, 2)
    
    # Put in newly created variable
    values_for_brplt <- rbind(values_for_brplt, security2)
  }
  
  # Give column names
  rownames(values_for_brplt) <- colnames(t_m_values)
  
  # Join vectors into data frame
  t_m_data <- data.frame(t_m_group, t_m_value, values_for_brplt)
  
  # Values in rectangles
  t_m_data$label <- sprintf("%s: %s %%", t_m_data$t_m_group,
                          t_m_data$values_for_brplt)
  
  # Plot
  treemap(t_m_data,
          index=c("label"),
          vSize="t_m_value",
          type="index",
          vColor="values_for_brplt",
          title = "Portfolio Securities",
          title.legend = "Return")
}
# Test
portfolio_treemap3(df_portfolio)

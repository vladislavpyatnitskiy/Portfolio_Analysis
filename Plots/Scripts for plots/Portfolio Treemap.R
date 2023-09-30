# library
library(treemap)

# Function to plot portfolio treemap
portfolio_treemap <- function(x){
  
  # Take total values
  t_m_values <- x[,3 + 3 * seq(31, from = 0)]
  
  # Give them names of the assets
  colnames(t_m_values) <- colnames(x[,1 + 3 * seq(31, from = 0)])
  
  # Create vector to contain names of securities
  t_m_group <- c(colnames(t_m_values))
  
  # Create vector to contain total sum of securities
  t_m_value <- c(t_m_values[nrow(t_m_values),])
  
  # Join vectors into data frame
  t_m_data <- data.frame(t_m_group, t_m_value)
  
  # Plot
  treemap(t_m_data,
          index="t_m_group",
          vSize="t_m_value",
          type="index")
}
# Test
portfolio_treemap(df_portfolio)

# Libraries
lapply(c("ggplot2", "tidyverse"), require, character.only = TRUE)

# Function to generate Stacked Bar Plot
stacked_barplot <- function(x){
  
  # Subset columns with total sum of securities
  df_exp_forPort1 <-  x[,3 + 3 * seq(31, from = 0)]
  
  # Take column names with prices to put instead total sum column names
  colnames(df_exp_forPort1) <- colnames(x[,1 + 3 * seq(31, from = 0)])  
  
  # Take dates from index column
  rwnms <- rownames(df_exp_forPort1)
  
  # Make it in date format
  rwnms <- as.Date(rwnms)
  
  # Join it with main data set
  df_exp_forPort1 <- data.frame(rwnms, df_exp_forPort1)
  
  # Create sequence for index column
  index_for_s_bplt <- seq(nrow(df_exp_forPort1))
  
  # Put them in index column
  rownames(df_exp_forPort1) <- index_for_s_bplt
  
  # Define variable to contain values
  df_f_mly_pct <- NULL
  
  # For each column with prices
  for (n in 2:ncol(df_exp_forPort1)){
    
    # Define variable for loop
    security <- df_exp_forPort1[,n]
    
    # Convert daily data to monthly
    values_for_df <- tapply(security,
                            format(as.Date(df_exp_forPort1[,1]),
                                   "%Y-%m"), median)
    
    # Take dates from index column
    rwmns_ds <- rownames(values_for_df)
    
    # Join with new data set
    values_for_df <- data.frame(rwmns_ds, values_for_df)
    
    # Generate sequence for index column
    inx <- seq(nrow(values_for_df))
    
    # Put it in index column
    rownames(values_for_df) <- inx
    
    # Rename Date column to Date
    colnames(values_for_df)[colnames(values_for_df) ==
                              'rwmns_ds'] <- 'Date'
    
    # If defined empty variable is still empty
    if (is.null(df_f_mly_pct)) {
      
      # Put new dataset there
      df_f_mly_pct <- values_for_df
      
    } else {
      
      # Or else join with current one
      df_f_mly_pct <- merge(x = df_f_mly_pct,
                            y = values_for_df,
                            by = "Date")}
  }
  # Convert to data frame format
  df_f_mly_pct <- as.data.frame(df_f_mly_pct)
  
  # Give column names
  colnames(df_f_mly_pct) <- colnames(df_exp_forPort1)
  
  # Rename date column again
  colnames(df_f_mly_pct)[colnames(df_f_mly_pct) ==
                           colnames(df_f_mly_pct[1])] <- 'Date'
  
  # Calculate percentages
  df_1_pct <- data.frame(df_f_mly_pct[,1],
                         df_f_mly_pct[,2:ncol(df_f_mly_pct)] /
                           rowSums(df_f_mly_pct[,2:ncol(df_f_mly_pct)]))
  
  # Rename date column once again
  colnames(df_1_pct)[colnames(df_1_pct) == colnames(df_1_pct[1])] <- 'Date'
  
  # Transpose
  df_2_pct <- t(df_1_pct)
  
  # Put dates to column names
  colnames(df_2_pct) <- df_2_pct[1,]
  
  # Reduce dates from main data set
  df_2_pct <- df_2_pct[-1,]
  
  # Convert to data frame again
  df_2_pct <- as.data.frame(df_2_pct)
  
  # Convert for better read by ggplot
  df2_upd = monthly_df2 %>% rownames_to_column("x") %>% gather(year, value, -x)
  
  # Colour set for plot
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b",
               "#bd5975")
  
  # Generate plot
  ggplot(df2_upd, aes(x = year, y = value, fill = x)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values=colors37) + geom_col() +
    labs(title = "Stacked Bar Plot",
         x = "Months",
         y = "Stakes",
         fill = "Securities")
}  
# Test
stacked_barplot(df_portfolio)

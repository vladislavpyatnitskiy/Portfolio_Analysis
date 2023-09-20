# Start dates
start_date_port4 <- list("2022-07-09", "2022-09-10")

# End dates
end_date_port4 <- list("2022-09-09" , "2023-07-09")

# Number of stocks 
number_of_assets4 <- list(2, 3)

# Combine all info in list
exp_linked_list3 <- list("FL",
                      start_date_port4,
                      end_date_port4,
                      number_of_assets4)
# Start dates
start_date_port20 <- list("2022-06-15", "2022-09-02")

# End dates
end_date_port20 <- list("2022-09-01" , "2023-08-06")

# Number of stocks 
number_of_assets20 <- list(4, 1)

# Combine all info in list
exp_linked_list4 <- list("WERN",
                         start_date_port20,
                         end_date_port20,
                         number_of_assets20)

# Create nested list
full_nested_list <- list(exp_linked_list3, exp_linked_list4)

# Transform nested list to data frame
df_nested_list <- as.data.frame(do.call(rbind, full_nested_list))

# Give column names to data frame
colnames(df_nested_list) <- c("Ticker", "Start Date", "End Date", "Number")

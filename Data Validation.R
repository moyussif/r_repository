library(dplyr)

# Merge the two data frames, highlighting the source of each row
# We will assume a perfect merge based on ID
merged_data <- merge(data_p1, data_p2, by = "ID", all = TRUE, suffixes = c("_P1", "_P2"))

# Filter for rows where any column has a difference
differences_found <- merged_data %>%
  filter(
    Name_P1 != Name_P2 |
      Age_P1 != Age_P2 |
      Score_P1 != Score_P2
  )

# Display the rows with differences, showing both entries
print(differences_found)

===========================================================================================
# Assuming data is in CSV files
df1 <- read.csv("data_person_A.csv")
df2 <- read.csv("data_person_B.csv")

# Merge using a common ID column
merged_data <- merge(df1, df2, by = "ID", all = TRUE, suffixes = c("_A", "_B"))

# Example: Comparing a column named "Value"
differences <- merged_data[merged_data$Value_A != merged_data$Value_B | 
                             is.na(merged_data$Value_A) != is.na(merged_data$Value_B), ]

# Identify rows where an entry from person A is missing
missing_A <- merged_data[is.na(merged_data$Value_A) & !is.na(merged_data$Value_B), ]

# Identify rows where an entry from person B is missing
missing_B <- merged_data[!is.na(merged_data$Value_A) & is.na(merged_data$Value_B), ]

===========================================================================================
  
  
install.packages("arsenal")
# Data from Person 1
data_p1 <- data.frame(
  ID = c(1, 2, 3, 4),
  Name = c("Alice", "Bob", "Charlie", "David"),
  Age = c(24, 30, 25, 28),
  Score = c(95, 88, 92, 76)
)

# Data from Person 2 (with some differences and different row order)
data_p2 <- data.frame(
  ID = c(4, 2, 1, 3),
  Name = c("David", "Bob", "Alice", "Charlie"),
  Age = c(28, 30, 24, 26), # Age difference for Charlie
  Score = c(76, 88, 95, 99) # Score difference for Charlie
)


#Compare the data frames:
#Use the comparedf() function, specifying the ID column as the key to match rows.

library(arsenal)

comparison_result <- comparedf(data_p1, data_p2, by = "ID")

# View a summary of the differences
summary(comparison_result)

# Print the specific differences detected by the package
print(comparison_result)
# The 'Differences detected' section will show the old and new values, and row numbers.





  
  
  
  
  
  
  
  
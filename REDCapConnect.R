#
rm(list=ls())
gc(reset = TRUE)
#-------------------------------------------------------------------------------
library(pak)
library(tidyREDCap)
library(redcapAPI)
library(readxl)
library(readr)
library(writexl)
#Connect to Import Instrument---------------------------------------------------
pak::pak("keyring")
keyring::key_set("lfstudy_key")

tidyREDCap::import_instruments(
  url = "https://redcap.noguchi.ug.edu.gh/api/", 
  token = keyring::key_get("lfstudy_key")
)

#To enter your computer’s password to access the key.---------------------------
rcon <- redcapAPI::redcapConnection(
  url = 'https://redcap.noguchi.ug.edu.gh/api/', 
  token = keyring::key_get("lfstudy_key")
)
#
redcap <- redcapAPI::exportRecords(rcon)

#save RDS(redcap, file = "LF_study.rds")
write_xlsx(redcap, "LF_Jun25.xlsx")

#Set Working Directorate--------------------------------------------------------
setwd("C:\Users\User\OneDrive - University of Ghana\moyussif@NMIMR\NMIMR\4Official\mod_r")

LFdata <- read_excel("LF_Jun25.xlsx")

str(LFdata)


# # #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+                             DATA VALIDATION                                  +
================================================================================
#              To check data entries by two people and see the differences  

library(readxl)
library(readr)
data1 <- read_excel("C:/Users/User/Desktop/Data1.xlsx")
data2 <- read_excel("C:/Users/User/Desktop/Data2.xlsx")

================================================================================ Option 1
library(dplyr)
# Merge the two data frames, highlighting the source of each row
# We will assume a perfect merge based on ID
merged_data <- merge(data1, data2, by = "ID", all = TRUE, suffixes = c("P1", "P2"))

# Filter for rows where any column has a difference
differences_found <- merged_data %>%
  filter(
    data1$Name != data2$Name |
      data1$Age != data2$Age |
      data1$Subject!= data2$Subject |
      data1$Score != data2$Score
  )

# Display the rows with differences, showing both entries
print(differences_found)

================================================================================ Option 2
#Compare the data frames:
#Use the comparedf() function, specifying the ID column as the key to match rows.
library(readxl)
library(readr)
data1 <- read_excel("C:/Users/User/Desktop/Data1.xlsx")
data2 <- read_excel("C:/Users/User/Desktop/Data2.xlsx")


library(arsenal)

comparison_result <- comparedf(data1, data2, by = "ID")

# View a summary of the differences
summary(comparison_result)

# Print the specific differences detected by the package
print(comparison_result)
# The 'Differences detected' section will show the old and new values, and row numbers.    


--------------------------------------------------------------------------------
                 HANDLING  DUPLICATE DATA
--------------------------------------------------------------------------------
  
  BF_data=data.frame(name=c("Daniel","Gabriella","John","Paul",
                            "Julius","Gabriella","Paul"),
                     maths=c(7,8,8,9,10,8,9),
                     science=c(5,7,6,8,9,7,8),
                     history=c(7,7,7,7,7,7,7))

BF_data

#base functions
duplicated(BF_data)
sum(duplicated(BF_data))  

unique(BF_data) 

#Dplyr package
library(dplyr)
distinct(BF_data) 


# # #
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                Merge Data
--------------------------------------------------------------------------------
library(readxl)
library(readr)
library(writexl)
library(tidyverse)
library(dplyr)

df2 <- read_excel("C:/Users/User/Desktop/Epid.csv")
print(df1)
df1 <- read_excel("C:/Users/User/Desktop/Lab.csv")
print(df2)

names(df1)
names(df2)
joined_df <- left_join(df1, df2, by = "SAMPLE_ID")
View(joined_df)
str(joined_df)

write_xlsx(joined_df, "Mergedoo.xlsx")

#  #  #  
  ============================
  ============================
  
  
  age_sex_summary <- LFdata %>%
  filter(fts_test_result == "Positive") %>%   # keep only positive FTS results
  mutate(
    Age_Group = case_when(
      age < 10 ~ "<10",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 40 ~ "30-40",
      age > 40 ~ ">40",
      TRUE ~ NA_character_
    ),
    Age_Group = factor(
      Age_Group,
      levels = c("<10", "10-19", "20-29", "30-40", ">40")
    )
  ) %>%
  group_by(Age_Group, sex) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

ggplot(age_sex_summary,
       aes(x = Age_Group, y = Count, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    x = "Age Group (Years)",
    y = "Number of Positive FTS Participants",
    fill = "Sex"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
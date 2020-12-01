# Load necessary libraries
library(tidyverse)
library(readxl)
library(stringr)

# read in data
sheets <- excel_sheets("Dataset/RTIS_Migrant Study_11May.xlsx" ) # prints list of sheets
data <- read_xlsx("Dataset/RTIS_Migrant Study_11May.xlsx", sheet = "Complete") %>% as.data.frame() # converting to dataframe

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences
data_d4_s1 <- data %>%
  select("responseid", starts_with(c("s1", "d4")))

# renaiming columns
new_names <- c("responseid", "What is your current work status?",
               "What is the name of your country of birth?")

colnames(data_d4_s1) <- new_names

glimpse(data_d4_s1)  # cols 2 is double and col 3 is character

# changing class type of cols 2 and 3
data_d4_s1$`What is your current work status?` <- as.factor(data_d4_s1$`What is your current work status?`)
data_d4_s1$`What is the name of your country of birth?` <- as.factor(data_d4_s1$`What is the name of your country of birth?`)
glimpse(data_d4_s1) # change successful 

# factor recoding column 2
data_d4_s1 <- data_d4_s1 %>% 
  mutate(`What is your current work status?` = fct_recode(`What is your current work status?`,
         "Employed full-time" = "1",
         "Employed part-time" = "2",
         "Self employed" = "3",
         "Business owner" = "4",
         "Unemployed and looking for work" = "5",
         "Not in, or looking for, paid work" = "6"))
# Grouping
data_d4_s1 <- data_d4_s1 %>%
  group_by(`What is the name of your country of birth?`,
           `What is your current work status?`) %>%
  count()

# Plotting
ggplot(data = data_d4_s1, aes(x = `What is your current work status?`, y = n,
  fill = `What is the name of your country of birth?`)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

# starting d4 and d20

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences
data_d4_d20 <- data %>%
  select("responseid", starts_with(c("d4", "d20")))

# renaiming columns
new_names_2 <- c("responseid", "Country of birth","Current job",
               "Job2", "Job3", "Job4")
colnames(data_d4_d20) <- new_names_2

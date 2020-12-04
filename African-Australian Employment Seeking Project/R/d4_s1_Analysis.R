# Library and data reading ----

# Load necessary libraries
library(tidyverse)
library(readxl)
library(stringr)

# read in data
sheets <- excel_sheets("Dataset/RTIS_Migrant Study_11May.xlsx" ) # prints list of sheets
data <- read_xlsx("Dataset/RTIS_Migrant Study_11May.xlsx", sheet = "Complete") %>% as.data.frame() # converting to dataframe

# Country of birth and Current work status ----

## Data prep ----
# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences
data_d4_s1 <- data %>%
  select("responseid", starts_with(c("s1", "d4")))

# renaiming columns
new_names <- c("responseid", "Current work status",
               "Country of birth")

colnames(data_d4_s1) <- new_names

glimpse(data_d4_s1)  # cols 2 is double and col 3 is character

# changing class type of cols 2 and 3
data_d4_s1$`Current work status` <- as.factor(data_d4_s1$`Current work status`)
data_d4_s1$`Country of birth` <- as.factor(data_d4_s1$`Country of birth`)
glimpse(data_d4_s1) # change successful 

# factor recoding column 2
data_d4_s1 <- data_d4_s1 %>% 
  mutate(`Current work status` = fct_recode(`Current work status`,
         "Employed full-time" = "1",
         "Employed part-time" = "2",
         "Self employed" = "3",
         "Business owner" = "4",
         "Unemployed and looking for work" = "5",
         "Not in, or looking for, paid work" = "6"))

## Grouping and Plotting ----
# Grouping
data_d4_s1 <- data_d4_s1 %>%
  group_by(`Country of birth`,
           `Current work status`) %>%
  count(name = "Total")

# check levels in country of birth column
levels(data_d4_s1$`Country of birth`)

# Collapse country names
data_d4_s1 <- data_d4_s1 %>%
  mutate(`Country of birth` = fct_collapse(`Country of birth`,
                                           `DR Congo` = c("Congo DRC", "Democratic Republic of Congo",
                                                        "D.R. Congo", "Republic democratic of Congo",
                                                        "Congo",  "Congo, DR", "DEM. REP . CONGO", 
                                                        "DR Congo", "Congo dr", "D. R Congo", 
                                                        "Democratic republic of Congo", "DRC"),
                                           `South Sudan` = c("South Sudan", "South sudan")))

# Plotting
ggplot(data = data_d4_s1, aes(x = `Current work status`, y = Total,
  fill = `Country of birth`)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

# Country of birth and Qualifications required at each of your jobs ----

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences
data_d4_d20 <- data %>%
  select("responseid", starts_with(c("d4", "d20")))

# renaiming columns
new_names_2 <- c("responseid", "Country of birth","CurrentJob",
               "Job2", "Job3", "Job4")
colnames(data_d4_d20) <- new_names_2

# mutate columns to factor
data_d4_d20 <- data_d4_d20 %>%
  mutate_if(is.double, as.factor)

# change country of birth to factor
data_d4_d20$`Country of birth` <- as.factor(data_d4_d20$`Country of birth`)

# Collapse country names
data_d4_d20 <- data_d4_d20 %>%
  mutate(`Country of birth` = fct_collapse(`Country of birth`,
                                           DR_Congo = c("Congo DRC", "Democratic Republic of Congo",
                                                        "D.R. Congo", "Republic democratic of Congo",
                                                        "Congo",  "Congo, DR", "DEM. REP . CONGO", 
                                                        "DR Congo", "Congo dr", "D. R Congo", 
                                                        "Democratic republic of Congo", "DRC"),
                                           South_Sudan = c("South Sudan", "South sudan")))


# change factor values
data_d4_d20 <- data_d4_d20 %>%
  mutate_at(.vars = vars(CurrentJob, Job2, Job3, Job4),
            .funs = fct_recode,
            "Yes" = "1",
            "No" = "2")

# pivoting data into longer format

data_d4_d20 <- data_d4_d20 %>%
  pivot_longer(!c(responseid, `Country of birth` ), names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response, `Country of birth`) %>%
  count(name = "total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column

# Plotting
ggplot(data = data_d4_d20, aes(x = question, y = total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Country of birth`) +
  coord_flip()





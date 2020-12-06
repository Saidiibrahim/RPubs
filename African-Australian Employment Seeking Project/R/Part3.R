# Load necessary packages
library(tidyverse)
library(readxl)
library(stringr)

# read in data
sheets <- excel_sheets("Dataset/RTIS_Migrant Study_11May.xlsx" ) # prints list of sheets
data <- read_xlsx("Dataset/RTIS_Migrant Study_11May.xlsx", sheet = "Complete") %>% as.data.frame() # converting to dataframe

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences

#D30_3 experience in applying for a job ------
data_d30 <- data %>%
  select("responseid", starts_with("d30_"))
# renaming column names. Possible with rename_with() and regex?
new_name_5 <- c("responseid", "The process was transparent", 
               "The process was conducted efficiently",
               "The process was fair",
               "I was provided adequate opportunity to seek feedback on my application")

colnames(data_d30) <- new_name_5

# lets look at the data structure
glimpse(data_d30)

# convert all columns, except first one, to factor
data_d30 <- data_d30 %>%
  mutate_if(is.double, as.factor)

# glimpse again
glimpse(data_d30)

# get number of levels for every factor variable
data_d30 %>%
  summarise_if(is.factor, nlevels)

# change factor levels by hand
data_d30 <- data_d30 %>%
  mutate_at(.vars = vars(`The process was transparent`,
                         `The process was conducted efficiently`,
                         `The process was fair`,
                         `I was provided adequate opportunity to seek feedback on my application`),
            .funs = fct_recode,
            "Strongly disagree" = "1",
            "Disagree" = "2",
            "Neither agree nor disagree" = "3" ,
            "Agree" = "4",
            "Strongly agree" = "5",
            "No answer" = "6")


# pivoting data into longer format

data_d30 <- data_d30 %>%
  pivot_longer(!responseid, names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response) %>%
  count(name = "Total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column

# Plotting
ggplot(data = data_d30, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()



# D38 country of highest level of education and D20 ------
data_d38_d20 <- data %>%
  select("responseid", starts_with(c("d38", "d20")))
# renaming column names. Possible with rename_with() and regex?
new_name_6 <- c("responseid", "Country of highest education", 
                "Current job", "Job2", "Job3", "Job4")

colnames(data_d38_d20) <- new_name_6

# glimpse
glimpse(data_d38_d20)

# convert all columns, except first one, to factor
data_d38_d20 <- data_d38_d20 %>%
  mutate_if(is.double, as.factor)

# glimpse
glimpse(data_d38_d20)

data_d38_d20$`Country of highest education` <- as.factor(data_d38_d20$`Country of highest education`)

# glimpse
glimpse(data_d38_d20)

# Collapse country names
data_d38_d20 <- data_d38_d20 %>%
  mutate(`Country of highest education` = fct_collapse(`Country of highest education`,
                                           `Australia` = c("Australia", "Australian", "AUSTRALIA", "Tafe",
                                                           "Australia- UoA"),
                                           `DR Congo` = c("Congo DRC", "DR Congo",
                                                          "Republic democratic of Congo",
                                                          "Democratic Republic of Congo",
                                                          "DRC", "Dr congo")))



data_d38_d20 <- data_d38_d20 %>%
  mutate_at(.vars = vars(`Current job`, Job2, Job3, Job4),
            .funs = fct_recode,
            "Yes" = "1",
            "No" = "2")
# pivoting data into longer format
data_d38_d20 <- data_d38_d20 %>%
  pivot_longer(!c(responseid, `Country of highest education`), names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response, `Country of highest education`) %>%
  count(name = "Total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column


# Plotting
ggplot(data = data_d38_d20, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  #facet_wrap(~ `Country of highest education`) +
  coord_flip()

# D20 are your qualifications required at each of your jobs and D24a_5 ----
data_d20_d24a <- data %>%
  select("responseid", starts_with(c("d20_", "d24a_")))
# renaming column names. Possible with rename_with() and regex?
#new_name_7 <- c("responseid", "The process was transparent", 
#                "The process was conducted efficiently",
#                "The process was fair",
#                "I was provided adequate opportunity to seek feedback on my application")



















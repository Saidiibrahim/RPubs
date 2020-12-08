# Library load and Data prep --------
# Load necessary packages
library(tidyverse)
library(readxl)
library(stringr)

# read in data
sheets <- excel_sheets("Dataset/RTIS_Migrant Study_11May.xlsx" ) # prints list of sheets
data <- read_xlsx("Dataset/RTIS_Migrant Study_11May.xlsx", sheet = "Complete") %>% as.data.frame() # converting to dataframe

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences

# D31 To what extent do you think each of the following have influenced your employment seeking experiences ------
data_d31 <- data %>%
  select("responseid", starts_with("d31_"))

data_d31_ref <- data %>%
  select("responseid", starts_with("d31_")) # will use this to check colnames changes 

# renaming column names. Possible with rename_with() and regex?
new_names <- c("responseid", "Job_opportunities_in_South_Australia", 
               "The_strength_of_your_personal_social_networks",
               "Your_knowledge_about_Australian_culture",
               "Employers_knowledge_about_your_culture",
               "Your_former_employment_experience",
               "Your_level_of_qualification_being_too_high",
               "Your_level_of_qualification_being_to_low",
               "The_country_you_received_your_qualification_in", "Your_race","Your_religion",
               "Your_ethnicity", "Your_gender", "Your_disability", "Your_language_or_accent",
               "Employers_perceptions_about_Africans_in_Australia",
               "Your_limited_prior_employment_interview_experience", "Your_age","Your_visa_status",
               "Your_name", "Your_country_of_birth")

colnames(data_d31) <- new_names # converts column names to those above
#Check if one of the column is numeric
is.numeric(data_d31$Your_country_of_birth) # returns TRUE (str(data_31) returns numeric for all columns)
# convert all columns, except first one, to factor
data_d31 <- data_d31 %>%
  mutate_if(is.double, as.factor)

glimpse(data_d31) # now all columns are factors

# convert responseid column to double
data_d31$responseid <- as.numeric(data_d31$responseid)

glimpse(data_d31) # now all columns are factors

# get number of levels for every factor variable
data_d31 %>%
  summarise_if(is.factor, nlevels)

# get levels of one column
levels(data_d31$Job_opportunities_in_South_Australia)

# change factor levels by hand
data_d31 <- data_d31 %>%
  mutate_at( .vars = vars(Job_opportunities_in_South_Australia, 
                          The_strength_of_your_personal_social_networks,
                          Your_knowledge_about_Australian_culture,
                          Employers_knowledge_about_your_culture,
                          Your_former_employment_experience,
                          Your_level_of_qualification_being_too_high,
                          Your_level_of_qualification_being_to_low,
                          The_country_you_received_your_qualification_in, Your_race,Your_religion,
                          Your_ethnicity, Your_gender, Your_disability, Your_language_or_accent,
                          Employers_perceptions_about_Africans_in_Australia,
                          Your_limited_prior_employment_interview_experience, Your_age,Your_visa_status,
                          Your_name, Your_country_of_birth),
             .funs = fct_recode, 
             "Not at all" = "1",
             "Slightly" = "2",
             "Moderately" = "3",
             "Strongly" = "4",
             "Very strongly" = "5",
             "No answer" = "6")

# pivoting data into longer format

data_d31 <- data_d31 %>%
  pivot_longer(!responseid, names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response) %>%
  count(name = "Total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column

# Plotting
ggplot(data = data_d31, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

# D4 country of birth and each of the sub-questions in D31 -----

data_d31_d4 <- data %>%
  select("responseid", starts_with(c("d31_", "d4")))

# renaming column names. Possible with rename_with() and regex?
new_names_3 <- c("responseid", "Job opportunities in South Australia", 
               "The strength of your personal social networks",
               "Your knowledge about Australian culture",
               "Employers knowledge about your culture",
               "Your former employment experience",
               "Your level of qualification being too high",
               "Your level of qualification being to low",
               "The country you received your qualification in", "Your race","Your religion",
               "Your ethnicity", "Your gender", "Your disability", "Your language or accent",
               "Employers perceptions about Africans in Australia",
               "Your limited prior employment interview experience", "Your age","Your visa status",
               "Your name", "Your country of birth", "Country of birth")

colnames(data_d31_d4) <- new_names_3 # converts column names to those above
#Check if one of the column is numeric
is.numeric(data_d31_d4$`Your country of birth`) # returns TRUE (str(data_31) returns numeric for all columns)
# convert all columns, except first one, to factor
data_d31_d4 <- data_d31_d4 %>%
  mutate_if(is.double, as.factor)

glimpse(data_d31_d4) # now all columns are factors

# convert responseid column to double
data_d31_d4$responseid <- as.numeric(data_d31_d4$responseid)
data_d31_d4$`Country of birth` <- as.factor(data_d31_d4$`Country of birth`)

glimpse(data_d31_d4) # now all columns are factors

# get number of levels for every factor variable
data_d31_d4 %>%
  summarise_if(is.factor, nlevels)

# get levels of one column
levels(data_d31$Job_opportunities_in_South_Australia)

# change factor levels by hand
data_d31_d4 <- data_d31_d4 %>%
  mutate_at( .vars = vars(`Job opportunities in South Australia`, 
                          `The strength of your personal social networks`,
                          `Your knowledge about Australian culture`,
                          `Employers knowledge about your culture`,
                          `Your former employment experience`,
                          `Your level of qualification being too high`,
                          `Your level of qualification being to low`,
                          `The country you received your qualification in`, `Your race`,`Your religion`,
                          `Your ethnicity`, `Your gender`, `Your disability`, `Your language or accent`,
                          `Employers perceptions about Africans in Australia`,
                          `Your limited prior employment interview experience`, `Your age`,`Your visa status`,
                          `Your name`, `Your country of birth`),
             .funs = fct_recode, 
             "Not at all" = "1",
             "Slightly" = "2",
             "Moderately" = "3",
             "Strongly" = "4",
             "Very strongly" = "5",
             "No answer" = "6")

# pivoting data into longer format

data_d31_d4 <- data_d31_d4 %>%
  pivot_longer(!c(responseid, `Country of birth`), names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response, `Country of birth`) %>%
  count(name = "Total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column

# Collapse country names
data_d31_d4 <- data_d31_d4 %>%
  mutate(`Country of birth` = fct_collapse(`Country of birth`,
                                           `DR Congo` = c("Congo DRC", "Democratic Republic of Congo",
                                                          "D.R. Congo", "Republic democratic of Congo",
                                                          "Congo",  "Congo, DR", "DEM. REP . CONGO", 
                                                          "DR Congo", "Congo dr", "D. R Congo", 
                                                          "Democratic republic of Congo", "DRC"),
                                           `South Sudan` = c("South Sudan", "South sudan")))

# Plotting
ggplot(data = data_d31_d4, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Country of birth`) +
  coord_flip()

# S1 What is your current work status and each of the sub-questions in D31 -----
data_d31_s1 <- data %>%
  select("responseid", starts_with(c("d31_", "s1")))

# renaming column names. Possible with rename_with() and regex?
new_names_4 <- c("responseid", "Job opportunities in South Australia", 
                 "The strength of your personal social networks",
                 "Your knowledge about Australian culture",
                 "Employers knowledge about your culture",
                 "Your former employment experience",
                 "Your level of qualification being too high",
                 "Your level of qualification being to low",
                 "The country you received your qualification in", "Your race","Your religion",
                 "Your ethnicity", "Your gender", "Your disability", "Your language or accent",
                 "Employers perceptions about Africans in Australia",
                 "Your limited prior employment interview experience", "Your age","Your visa status",
                 "Your name", "Your country of birth", "Current work status")

colnames(data_d31_s1) <- new_names_4 # converts column names to those above
#Check if one of the column is numeric
is.numeric(data_d31_s1$responseid) # returns TRUE (str(data_31) returns numeric for all columns)
# convert all columns, except first one, to factor
data_d31_s1 <- data_d31_s1 %>%
  mutate_if(is.double, as.factor)

glimpse(data_d31_s1) # now all columns are factors

# convert responseid column to double
#data_d31_d4$responseid <- as.numeric(data_d31_d4$responseid)
#data_d31_d4$`Country of birth` <- as.factor(data_d31_d4$`Country of birth`)

#glimpse(data_d31_d4) # now all columns are factors

# get number of levels for every factor variable
data_d31_s1 %>%
  summarise_if(is.factor, nlevels)

# get levels of one column
levels(data_d31_s1$`Job opportunities in South Australia`)

# change factor levels by hand
data_d31_s1 <- data_d31_s1 %>%
  mutate_at( .vars = vars(`Job opportunities in South Australia`, 
                          `The strength of your personal social networks`,
                          `Your knowledge about Australian culture`,
                          `Employers knowledge about your culture`,
                          `Your former employment experience`,
                          `Your level of qualification being too high`,
                          `Your level of qualification being to low`,
                          `The country you received your qualification in`, `Your race`,`Your religion`,
                          `Your ethnicity`, `Your gender`, `Your disability`, `Your language or accent`,
                          `Employers perceptions about Africans in Australia`,
                          `Your limited prior employment interview experience`, `Your age`,`Your visa status`,
                          `Your name`, `Your country of birth`),
             .funs = fct_recode, 
             "Not at all" = "1",
             "Slightly" = "2",
             "Moderately" = "3",
             "Strongly" = "4",
             "Very strongly" = "5",
             "No answer" = "6")

# factor re-coding s1 column
data_d31_s1 <- data_d31_s1 %>% 
  mutate(`Current work status` = fct_recode(`Current work status`,
                                            "Employed full-time" = "1",
                                            "Employed part-time" = "2",
                                            "Self employed" = "3",
                                            "Business owner" = "4",
                                            "Unemployed and looking for work" = "5",
                                            "Not in, or looking for, paid work" = "6"))

# pivoting data into longer format
data_d31_s1 <- data_d31_s1 %>%
  pivot_longer(!c(responseid, `Current work status`), names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response, `Current work status`) %>%
  count(name = "Total") %>%
  filter(!is.na(response))   # remove rows with NA values in response column


# Plotting
ggplot(data = data_d31_s1, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Current work status`) +
  coord_flip()

# Age, recency of arrival and the variables in d31 -----
data_age_arrival_d31 <- data %>%
  select(starts_with(c("s0", "d1b", "d31_")))

# renaming column names. Possible with rename_with() and regex?
age_arrival_names <- c("Age group", "Arrival" , "Job opportunities in South Australia", 
                 "Strength of personal social networks",
                 "Knowledge about Australian culture",
                 "Employers knowledge about your culture",
                 "Previous employment experience",
                 "Level of qualification being too high",
                 "Level of qualification being too low",
                 "The country you received your qualification in", "Race","Religion",
                 "Ethnicity", "Gender", "Disability", "Language or accent",
                 "Employers perceptions about Africans in Australia",
                 "Limited prior employment interview experience", "Age","Visa status",
                 "Name", "Country of birth")

colnames(data_age_arrival_d31) <- age_arrival_names

# glimpse
glimpse(data_age_arrival_d31)

data_age_arrival_d31 <- data_age_arrival_d31 %>%
  mutate_if(is.double, as.factor)

# glimpse
glimpse(data_age_arrival_d31)

# change factor levels by hand
data_age_arrival_d31 <- data_age_arrival_d31 %>%
  mutate_at( .vars = vars(`Job opportunities in South Australia`, 
                          `Strength of personal social networks`,
                          `Knowledge about Australian culture`,
                          `Employers knowledge about your culture`,
                          `Previous employment experience`,
                          `Level of qualification being too high`,
                          `Level of qualification being too low`,
                          `The country you received your qualification in`, `Race`,`Religion`,
                          `Ethnicity`, `Gender`, `Disability`, `Language or accent`,
                          `Employers perceptions about Africans in Australia`,
                          `Limited prior employment interview experience`, `Age`,`Visa status`,
                          `Name`, `Country of birth`),
             .funs = fct_recode, 
             "Not at all" = "1",
             "Slightly" = "2",
             "Moderately" = "3",
             "Strongly" = "4",
             "Very strongly" = "5",
             "No answer" = "6")

# change factor levels by hand
data_age_arrival_d31 <- data_age_arrival_d31 %>%
  mutate_at( .vars = vars(`Age group`),
             .funs = fct_recode, 
             "Under 18" = "1",
             "18 - 19" = "2",
             "20 - 24" = "3",
             "25 - 29" = "4",
             "30 - 39" = "5",
             "40 - 49" = "6",
             "50 - 59" = "7",
             "60 - 67" = "8",
             "67 or older" = "9")


# pivoting data into longer format
data_age_arrival_d31 <- data_age_arrival_d31 %>%
  select(`Age group`, `Arrival`, `Strength of personal social networks`,
         `Knowledge about Australian culture`, `Employers knowledge about your culture`,
         `Race`, `Ethnicity`, `Language or accent`,
         `Employers perceptions about Africans in Australia`, `Country of birth`) %>%
  pivot_longer(!c(`Age group`, `Arrival`), names_to = "question", values_to = "response") %>%
  group_by(`Age group`, `Arrival`, question, response) %>%
  count(name = "Total") %>%
  filter(!is.na(response) & response != "No answer")   # remove rows with NA values in response column

# Plotting
ggplot(data = data_age_arrival_d31, aes(x = question, y = Total, fill = response)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~`Age group`) +
  coord_flip()

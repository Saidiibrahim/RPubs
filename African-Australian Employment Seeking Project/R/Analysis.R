# Load neccessary packages
library(tidyverse)
library(readxl)
library(stringr)

# read in data
sheets <- excel_sheets("Dataset/RTIS_Migrant Study_11May.xlsx" ) # prints list of sheets
data <- read_xlsx("Dataset/RTIS_Migrant Study_11May.xlsx", sheet = "Complete") %>% as.data.frame() # converting to dataframe

# read in just d31 columns : To what extent do you think each of the 
#following have influenced your employment seeking experiences

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
data_d31_recode <- data_d31 %>%
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

data_d31_pivot <- data_d31_recode %>%
  pivot_longer(!responseid, names_to = "question", values_to = "response") %>%
  select(!responseid) %>%   # remove the responseid column
  group_by(question, response) %>%
  count(name = "total") %>%
  filter(!is.na(response)) %>%   # remove rows with NA values in response column
  mutate(per = round(100*total/nrow(data_d31_pivot),2))

# Plotting
ggplot(data = data_d31_pivot, aes(x = question, y = per, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()





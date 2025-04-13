library(tidyverse)
#rm(list = ls())

# Sample dataset
df <- tibble(ID = 1:8,
             Age_Category = c("Young", "Middle", "Old", "Young", "Middle", "Old", "Young", "Middle"),
             Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
             Symptoms = c("Fever, Cough", "Cough, Headache", "Fever, Headache", "Fever, Cough, Headache", 
                          "Cough", "Fever", "Cough, Headache", "Fever, Cough"))

# Function to one-hot encode a column
one_hot_encode <- function(data, column_name) {
  data %>%
    separate_rows(!!sym(column_name), sep = ", ") %>%  # Split into multiple rows
    mutate(Value = 1) %>%
    pivot_wider(names_from = !!sym(column_name), values_from = Value, values_fill = 0)  # Convert to wide format
}

# One-hot encode Symptoms
df_symptoms <- one_hot_encode(df, "Symptoms")

df_encoded <- df %>% select(ID, Age_Category, Gender) %>%
  left_join(df_symptoms %>% select(-Age_Category, -Gender), by = "ID")  # Remove duplicate columns

df_encoded

## Logistic Regression

glm_model <- glm(Fever ~ Age_Category +Gender, data = df_encoded, family = binomial)
summary(glm_model)


tbl_regression(glm_model,exponentiate = TRUE)




### A great way to clean Multiple Responses data

library(stringr)
library(readxl)
library(tidyverse)
str_squish("  Hello    world   R   is   great!  ")

text_dat <- read_excel("D:/2.Statlab/Domestic C/Dr. Rashed Sir/Fahmida Akter_fahmida-2017914088@pharmacy.du.ac.bd/Copy of Personal_Level_screening_for_COPD(1).xlsx",
                       sheet = "Sheet6", skip = 1)

glimpse(text_dat)


text_dat$`Which medication you are taking for COPD (upated)?`<-str_squish(text_dat$`Which medication you are taking for COPD (upated)?`)


text_dat %>% select(`Which medication you are taking for COPD (upated)?`) 


#library(tidyr)
#df_clean <- df %>%
#  separate_rows(responses, sep = ",\\s*")  # Splits by comma and optional space



text_dat %>% 
  separate_rows(`Which medication you are taking for COPD (upated)?`, sep = ",\\s*") %>% 
  count(`Which medication you are taking for COPD (upated)?`
        ) %>% knitr::kable()

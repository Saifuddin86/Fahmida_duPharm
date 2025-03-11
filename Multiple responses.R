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

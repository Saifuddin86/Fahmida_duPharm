## --------------------------------------------------------------------------------------------------
#rm(list = ls())



## --------------------------------------------------------------------------------------------------
library(readxl)
data0<- read_excel("Copy of Personal_Level_screening_for_COPD(1).xlsx", 
    sheet = "Sheet6",skip = 1)
data0$`Educational qualification`<-factor(data0$`Educational qualification`,
                                          levels = c("No formal education",
                                                     "Secondary level",
                                                     "Undergraduate",
                                                     "Graduate",
                                                     "Postgraduate"))

#View(data0)

library(tidyverse)
library(knitr)
library(gtsummary)

#glimpse(data0)






## --------------------------------------------------------------------------------------------------
##| label: tbl-tfrequency
##| tbl-cap: Demographic and other characteristics of participants

#glimpse(data0)

data0[,c(3:10,15,16,12,14)] %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label =list(`Do you have any member in your family have experienced the same condition?`="Do you have any member in your family have experienced the same condition? (Yes)" ))


## --------------------------------------------------------------------------------------------------
#| label: tbl-tsumstat
#| tbl-cap: Summary statistics of duarion of bronchitis or chronic coughing with sputum from the chest (in months)


data0 %>%select(`How many months in the last year have you had bronchitis or chronic coughing with sputum from the chest?`,`For how many years you had bronchitis or chronic coughing with sputum from the chest?`) %>% gather(key = "Variable",value = "Duration") %>% 
  group_by(Variable) %>%
  summarise(
    Min = min(Duration, na.rm = TRUE),
    Max = max(Duration, na.rm = TRUE),
    Mean = mean(Duration, na.rm = TRUE),
    SD = sd(Duration, na.rm = TRUE)
  )->s1

s1 %>% kable(digits = 2)



## --------------------------------------------------------------------------------------------------
#| label: fig-fighist
#| fig-cap: "Frequency histogram of duration of having bronchitis or chronic coughing "
#| fig-subcap:
#|   - "Variable 1"
#|   - "Variable 2"
#| layout-ncol: 2


par(mar = c(6, 6, 4, 2))  # Adjust as needed
par(mgp = c(4, 1, 0))  # First value controls axis title distance

par(cex.lab = 1.3)  # Set larger axis labels globally
par(cex.axis=1)

hist(data0$`How many months in the last year have you had bronchitis or chronic coughing with sputum from the chest?`,breaks = 5,
     xlab = "Duration of having bronchitis or chronic coughing \n with sputum from the chest in the last year (in months)",
     main = "",col = "steelblue",ylim = c(0,59),)

hist(data0$`For how many years you had bronchitis or chronic coughing with sputum from the chest?`,breaks = 5,
     xlab = "Duration of having bronchitis or chronic \n coughing with sputum from the chest (in months) ", main = "",col = "steelblue")






## --------------------------------------------------------------------------------------------------
##| label: tbl-tvaccine_stat
##| tbl-cap: Previous history of vaccination

#data0$`Do you have previous history of vaccination? if yes, write the name of vaccine.`


data0 %>% 
  separate_rows(`Do you have previous history of vaccination? if yes, write the name of vaccine.`, sep = ",") %>%  # Split comma-separated values into rows
  count(`Do you have previous history of vaccination? if yes, write the name of vaccine.`, sort = TRUE)->vaccine_stat


vaccine_stat %>% filter(`Do you have previous history of vaccination? if yes, write the name of vaccine.`=="No") %>%
  mutate(Percent=n*100/250) %>% 
  kable(col.names = c("Do you have previous history of vaccination?","N","Percent (out of 250)"),digits = 2)



  
  


## --------------------------------------------------------------------------------------------------
vaccine_stat %>% filter(`Do you have previous history of vaccination? if yes, write the name of vaccine.`!="No") %>%
  mutate(Percent=n*100/250) %>%
  kable(col.names = c("If yes, write the name of vaccine","N","Percent (out of 250)"),digits = 2)



## --------------------------------------------------------------------------------------------------
#| label: fig-fvachistry
#| fig-cap: Distribution of vaccination history
#|
vaccine_stat %>% filter(`Do you have previous history of vaccination? if yes, write the name of vaccine.`!="No") %>%mutate(Percent=n*100/250) %>% 
  ggplot(aes(x=reorder(`Do you have previous history of vaccination? if yes, write the name of vaccine.`,Percent),y=Percent))+
  geom_col()+labs(x="Previous history of vaccination")+
  coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(angle = 0,size = 12),
        axis.title = element_text(size=14))


## --------------------------------------------------------------------------------------------------
#rm(list = ls())

data0<- read_excel("Copy of Personal_Level_screening_for_COPD(1).xlsx", 
    sheet = "Sheet6",skip = 1)

#glimpse(data0)
#view(data0)
#data0$`Which medication you are taking for COPD (upated)?`

data0 %>%separate_rows(`Which medication you are taking for COPD (upated)?`,sep = ",") %>% 
  count(`Which medication you are taking for COPD (upated)?`, name = "Frequency") %>% 
  #print(n=55)
  mutate(Percentage = Frequency*100/250) %>%
  #view() %>% 
  arrange(-Frequency)->medi_take
  
medi_take %>%  kable(digits = 2,col.names = c("Medication taking for COPD?`","Frequency","Percentage (out of 30)"))



## --------------------------------------------------------------------------------------------------
# Data preparation for multiple response:: "`Which medication you are taking for COPD?`"| Type-Binary

# Function to one-hot encode a column
one_hot_encode <- function(data, column_name) {
  data %>%
    separate_rows(!!sym(column_name), sep = ",") %>%  # Split into multiple rows
    mutate(Value = 1) %>%
    pivot_wider(names_from = !!sym(column_name), values_from = Value, values_fill = 0)  # Convert to wide format
}


###########################
#data0$`Which medication you are taking for COPD?`
library(janitor)
clean_names(data0)->data1

#data1$which_medication_you_are_taking_for_copd
#data1$do_you_have_a_previous_history_of_smoking

# One-hot encode Symptoms
df_medication <- one_hot_encode(data1,"which_medication_you_are_taking_for_copd")

df_encoded <- data1 %>% select(id, age,gender,do_you_have_a_previous_history_of_smoking) %>%
  left_join(df_medication %>% select(-age,-gender,-do_you_have_a_previous_history_of_smoking), by = "id")  # Remove duplicate columns

#df_encoded %>% view()


#df_encoded$`Oxygen therapy`

df_encoded %>% select(age,gender,do_you_have_a_previous_history_of_smoking,Corticosteroids,`Oxygen therapy`,Bronchodialators)->association_data






## --------------------------------------------------------------------------------------------------
#| label: tbl-tasso_corti
#| tbl-cap: Association between smoking status and prevelance of Corticosteroids

# Function to extract chi-square test statistic
chisq_test_stat <- function(data, variable, by, ...) {
  chisq.test(table(data[[variable]], data[[by]])) %>% 
  broom::tidy() %>%
    select(statistic, p.value)
}


association_data %>% select(Corticosteroids,do_you_have_a_previous_history_of_smoking) %>%
 # mutate_if(is.numeric,as.factor) %>% 
  tbl_summary(by=do_you_have_a_previous_history_of_smoking,
              label=list(do_you_have_a_previous_history_of_smoking~"Smoking status",
                         Corticosteroids~"Corticosteroids (Yes)"))%>%
  add_stat(fns = everything() ~ chisq_test_stat) %>% 
  modify_header(statistic = "**Chi-square**", p.value = "**p-value**") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% 
  modify_spanning_header(starts_with("stat_") ~ "**Smoking status**")  # Column variable name





## --------------------------------------------------------------------------------------------------
association_data %>% select(`Oxygen therapy`,do_you_have_a_previous_history_of_smoking) %>% 
  tbl_summary(by=do_you_have_a_previous_history_of_smoking,
              label=list(do_you_have_a_previous_history_of_smoking~"Smoking status",
                         `Oxygen therapy`~"Oxygen therapy (Yes)"))%>%
  add_stat(fns = everything() ~ chisq_test_stat) %>% 
  modify_header(statistic = "**Chi-square**", p.value = "**p-value**") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  modify_spanning_header(starts_with("stat_") ~ "**Smoking status**")  # Column variable name




## --------------------------------------------------------------------------------------------------

association_data %>% 
  select(Bronchodialators,do_you_have_a_previous_history_of_smoking) %>% 
  tbl_summary(by=do_you_have_a_previous_history_of_smoking,
              label=list(do_you_have_a_previous_history_of_smoking~"Smoking status",
                         Bronchodialators~"Bronchodilators (Yes)"))%>%
  add_stat(fns = everything() ~ chisq_test_stat) %>% 
  modify_header(statistic = "**Chi-square**", p.value = "**p-value**") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  modify_spanning_header(starts_with("stat_") ~ "**Smoking status**")  # Column variable name


## --------------------------------------------------------------------------------------------------
data0 %>% select(`Work environment`,`For how many years you had bronchitis or chronic coughing with sputum from the chest?`,`How many months in the last year have you had bronchitis or chronic coughing with sputum from the chest?`) %>% 
  #tbl_strata(strata = Gender,~.x %>% 
  tbl_summary(by=`Work environment`)%>% add_p()


## --------------------------------------------------------------------------------------------------
#rm(list = ls())


data0 %>%
  mutate(new_educ = factor(case_when(
    `Educational qualification`  %in% c('No formal education','Secondary level') ~ 'No formal education and Secondary level',
    #`Educational qualification` == 'Secondary level' ~ 'Secondary level',
    `Educational qualification` == 'Undergraduate' ~ 'Undergraduate',
    `Educational qualification` %in% c('Graduate', 'Postgraduate') ~ 'Graduate and Postgraduate',
    TRUE ~ as.character(`Educational qualification`)
  ), levels = c("No formal education and Secondary level","Undergraduate", "Graduate and Postgraduate")),
  
  `Do you have any member in your family have experienced the same condition?` =
           factor(`Do you have any member in your family have experienced the same condition?`,levels = c("No","Yes" )))->data2

table(data2$`Do you have any member in your family have experienced the same condition?`)
  
data2 %>% select(new_educ,`Do you kNow about COPD and its affect in your quality of life?`,`Do you have any member in your family have experienced the same condition?`) %>% 
  tbl_summary(by=new_educ,
              type = everything() ~ "categorical",
              label =list(`Do you have any member in your family have experienced the same condition?`~"Do you have any member in your family have experienced the same condition? (yes)"),
              sort = everything() ~ "alphanumeric",
              missing = "ifany"
              ) %>% add_p()



## --------------------------------------------------------------------------------------------------

glm(Corticosteroids~age+gender+do_you_have_a_previous_history_of_smoking,
    family="binomial", data=association_data)->glm1
#tbl_regression(glm1, exponentiate = TRUE)


glm(`Oxygen therapy`~age+gender+do_you_have_a_previous_history_of_smoking,
    family="binomial", data=association_data)->glm2
#tbl_regression(glm2,exponentiate = TRUE)



## --------------------------------------------------------------------------------------------------
#rm(list = ls())



## --------------------------------------------------------------------------------------------------
library(readxl)

Survey <- read_excel("COPD Patient Management Survey.xlsx", 
    sheet = "Doctor")

#glimpse(Survey)

Survey %>% 
  separate_rows(Specialization,sep = ",") %>% 
  count(Specialization, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Specialization","Frequency","Percentage (out of 30)"))




## --------------------------------------------------------------------------------------------------
Survey[,5:6] %>% tbl_summary()


## --------------------------------------------------------------------------------------------------


Survey %>% 
  separate_rows(`What are the most common symptoms presented by COPD patients?`,sep = ";") %>% 
  count(`What are the most common symptoms presented by COPD patients?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Most common symptoms presented by COPD patients","Frequency","Percentage (out of 30)"))



## --------------------------------------------------------------------------------------------------

Survey %>% 
  separate_rows(`write symptom if the previous answer is "others"`,sep = ",") %>% 
  count(`write symptom if the previous answer is "others"`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Symptom if the previous answer is others","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------
#glimpse(Survey)

Survey %>% 
  separate_rows(`Which diagnostic tests do you commonly use for diagnosis and monitoring COPD?`,sep = ";") %>% 
  count(`Which diagnostic tests do you commonly use for diagnosis and monitoring COPD?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Diagnostic tests do you commonly use for diagnosis and monitoring COPD","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------
<<<<<<< HEAD
=======

>>>>>>> d93807345a4f71fff924559043e0a292ea60dd22
#glimpse(Survey)

Survey %>% 
  separate_rows(`write diagnostic test if the previous answer is "others"`,sep = ", ") %>% 
  count(`write diagnostic test if the previous answer is "others"`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Diagnostic test if the previous answer is others","Frequency","Percentage (out of 30)"))



## --------------------------------------------------------------------------------------------------
#glimpse(Survey)

Survey %>% 
  separate_rows(`Which medication do you typically prescribe for COPD patients?`,sep = ";") %>% 
  count(`Which medication do you typically prescribe for COPD patients?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Medication do you typically prescribe for COPD patients?","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------
Survey %>% select(`What is the average cost of a month's worth of COPD medication for a patient (estimate in local currency)?`) %>% 
  tbl_summary()


## --------------------------------------------------------------------------------------------------
Survey %>% select(`Do you observe any differences in medication response between smokers and non-smokers?`, `If Yes, Please describe-`) %>% 
  tbl_summary()


## --------------------------------------------------------------------------------------------------
Survey %>% select(`How often do you recommend follow up tests for COPD patient (e.g. spirometry)?`,`How frequently do COPD patients require hospitalization due to exacerbations?`,`What lifestyle changes do you most commonly recommend to COPD patients?`,`Please specify if previous answer is "Others"`,`Are there any new treatments or medication for COPD that you find promising?`) %>% 
  tbl_summary()



## --------------------------------------------------------------------------------------------------
#rm(list = ls())



## --------------------------------------------------------------------------------------------------
library(readxl)

Survey2 <- read_excel("COPD Patient Management doctors Survey.xlsx", 
    sheet = "updated",skip = 1)

#glimpse(Survey)

Survey2 %>% 
  separate_rows(Specialization,sep = ",") %>% 
  count(Specialization, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Specialization","Frequency","Percentage (out of 30)"))




## --------------------------------------------------------------------------------------------------
Survey2[,3:4] %>% tbl_summary()


## --------------------------------------------------------------------------------------------------

Survey2 %>% 
  separate_rows(`What are the most common symptoms presented by COPD patients?`,sep = ";") %>% 
  count(`What are the most common symptoms presented by COPD patients?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Most common symptoms presented by COPD patients","Frequency","Percentage (out of 30)"))



## --------------------------------------------------------------------------------------------------

Survey2 %>% 
  separate_rows(`write symptom if the previous answer is "others"`,sep = ",") %>% 
  count(`write symptom if the previous answer is "others"`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(Frequency) %>% 
  kable(digits = 2,col.names = c("Symptom if the previous answer is others","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------
#glimpse(Survey)

Survey2 %>% 
  separate_rows(`Which diagnostic tests do you commonly use for diagnosis and monitoring COPD?`,sep = ";") %>% 
  count(`Which diagnostic tests do you commonly use for diagnosis and monitoring COPD?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Diagnostic tests do you commonly use for diagnosis and monitoring COPD","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------

#glimpse(Survey)

Survey2 %>% 
  separate_rows(`write diagnostic test if the previous answer is "others"`,sep = ", ") %>% 
  count(`write diagnostic test if the previous answer is "others"`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Diagnostic test if the previous answer is others","Frequency","Percentage (out of 30)"))



## --------------------------------------------------------------------------------------------------
#glimpse(Survey)

Survey2 %>% 
  separate_rows(`Which medication do you typically prescribe for COPD patients?`,sep = ";") %>% 
  count(`Which medication do you typically prescribe for COPD patients?`, name = "Frequency") %>% # Count occurrences
  mutate(Percentage = Frequency*100/30) %>%arrange(-Frequency) %>% 
  kable(digits = 2,col.names = c("Medication do you typically prescribe for COPD patients?","Frequency","Percentage (out of 30)"))


## --------------------------------------------------------------------------------------------------
Survey2$`What is the average cost of a month's worth of COPD medication for a patient (estimate in local currency)?`<-factor(Survey2$`What is the average cost of a month's worth of COPD medication for a patient (estimate in local currency)?`,
                                                                                                                            levels = c("About 5000tk","5000-10000 tk",
                                                                                                                                       "10000-15000tk","15000-20000 tk",
                                                                                                                                       "Depends on treatment plan"))

Survey2 %>% select(`What is the average cost of a month's worth of COPD medication for a patient (estimate in local currency)?`) %>% 
  tbl_summary()


## --------------------------------------------------------------------------------------------------
Survey2 %>% select(`Do you observe any differences in medication response between smokers and non-smokers?`, `If Yes, Please describe-`) %>% 
  tbl_summary()


## --------------------------------------------------------------------------------------------------
Survey2 %>% select(`How often do you recommend follow up tests for COPD patient (e.g. spirometry)?`,`How frequently do COPD patients require hospitalization due to exacerbations?`,`What lifestyle changes do you most commonly recommend to COPD patients?`,`Please specify if previous answer is "Others"`,`Are there any new treatments or medication for COPD that you find promising?`) %>% 
  tbl_summary()



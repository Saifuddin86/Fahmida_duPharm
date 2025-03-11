library(forestmodel)

library("survival")
library("dplyr")
?lung

pretty_lung <- lung %>%
  transmute(time,
            status,
            Age = age,
            Sex = factor(sex, labels = c("Male", "Female")),
            ECOG = factor(lung$ph.ecog),
            `Meal Cal` = meal.cal
  )

print(forest_model(coxph(Surv(time, status) ~ ., pretty_lung)))


pretty_lung$`Meal Cal`

lm1<-lm(`Meal Cal`~time+Age+Sex+ECOG,data = pretty_lung)


forest_model(lm1)

?forest_model()

logistic<-glm(ECOG ~Age+Sex,data = pretty_lung,family = "binomial")

forest_model(logistic)

library(gtsummary)

# Example data frame
data <- data.frame(
  group = factor(rep(c("A", "B"), each = 50)),
  outcome = factor(sample(c("Yes", "No"), 100, replace = TRUE))
)

# Summary table with chi-square test p-value
summary_table <- tbl_summary(
  data,
  by = group,
  missing = "no"
) %>%
  add_p(test = list(outcome ~ "chisq.test"))

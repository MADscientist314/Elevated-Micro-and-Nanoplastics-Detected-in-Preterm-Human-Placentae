library(tidyverse)
library(readxl)
library(mosaic)
getwd()
# Load the data
data <- as_tibble(read.table(file="Table1-allmeta_master_wrangling_ML_one-hot.csv",sep=",", header=TRUE))   
data$Gestational_Age

# Create a new column for term/preterm classification (replace with actual column name if different)
data <- data %>% 
  mutate(gestational_category = ifelse(Gestational_Age >= 37, "Term", "Preterm"))

# Define numerical and categorical variables
numerical_vars <- names(data)[sapply(data, is.numeric)]
numerical_vars <- setdiff(numerical_vars, c("gestational_age_at_delivery")) # Exclude the classification column
numerical_vars
# Initialize a results dataframe
results <- data.frame(Variable = character(), Test = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop through numerical variables for statistical tests
for (var in numerical_vars) {
  # Check normality of the variable within each group
  term_data <- data %>% filter(gestational_category == "Term") %>% pull(var)
  preterm_data <- data %>% filter(gestational_category == "Preterm") %>% pull(var)

  if (shapiro.test(term_data)$p.value > 0.05 & shapiro.test(preterm_data)$p.value > 0.05) {
    # If normally distributed, use ANOVA
    test <- "ANOVA"
    p_val <- summary(aov(data[[var]] ~ data$gestational_category))[[1]]["Pr(>F)"][[1]]
  } else {
    # If not normally distributed, use Kruskal-Wallis
    test <- "Kruskal-Wallis"
    p_val <- kruskal.test(data[[var]] ~ data$gestational_category)$p.value
  }

  # Append results to the dataframe
  results <- rbind(results, data.frame(Variable = var, Test = test, p_value = p_val))
}

results%>%
  filter(p_value<0.05)
# Save the results table
write.csv(results, "Table1_analysis_results.csv", row.names = FALSE)

# Display the first few rows of results
print(head(results))

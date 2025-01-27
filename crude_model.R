# Load necessary libraries
library(tidyverse)
as_tibble(data)
# Load the dataset
# data <- read.tcsv("count_tab_log1p.csv")
# data<-count_tab_log1p
# Convert Delivery_Term to a factor
data$Delivery_Term <- factor(data$Delivery_Term, levels = c("Term", "Preterm"))
# Select microplastic predictors
microplastics <-unique(df_clean_long$microplastic)
as_tibble(data)

microplastics <- c("N6","N66","PE","PET","PU", "PP", "PS", "ABS", "SBR", "PMMA", "PC", "PVC")


microplastics
ggboxplot(data = data,x = "Delivery_Term",y = "PU",add = "jitter")+stat_compare_means(comparisons = my_comparisons)
ggboxplot(data = data,x = "Delivery_Term",y = "PMMA",add = "jitter")+stat_compare_means(comparisons = my_comparisons)

# Construct the formula for logistic regression
formula <- as.formula(paste("Delivery_Term ~", paste(microplastics, collapse = " + ")))
formula
# Fit the logistic regression model
model <- glm(formula, data = data, family = binomial)

# Display the model summary
summary(model)

# Check model diagnostics
library(car)
vif(model) # Variance Inflation Factor to check multicollinearity
#################################################################################
# Calculate adjusted odds ratios (AOR) and 95% confidence intervals
exp_coef <- exp(coef(model)) # Adjusted Odds Ratios
confint_model <- confint(model) # Confidence intervals
exp_confint <- exp(confint_model) # Exponentiate confidence intervals

# Combine into a table
aor_table <- cbind(Adjusted_Odds_Ratio = exp_coef,
                   Lower_CI = exp_confint[, 1],
                   Upper_CI = exp_confint[, 2])

# Print the table
print(aor_table)


# Load necessary libraries
library(tidyverse)
library(car)  # For VIF calculation

# Load the dataset
# data <- read.csv("count_tab_log1p.csv")

# Convert Delivery_Term to a factor with "Term" as the reference level
data$Delivery_Term <- factor(data$Delivery_Term, levels = c("Term", "Preterm"))

data2<-data%>%select(TissueID,Delivery_Term,PriorPretermBirth.Yes)%>%distinct_all()



# Ensure Prior.preterm.birth_Yes is a factor
data$PriorPretermBirth.Yes <- factor(data$PriorPretermBirth.Yes, levels = c(0, 1))
class(data$PriorPretermBirth.Yes)
unique(df_clean_long$microplastic)
# Select microplastic predictors
microplastics <- c("N6","N66","PE","PET","PU", "PP", "PS", "ABS", "SBR", "PMMA", "PC", "PVC")

microplastics

#TODO
#add ethnicity,SDI
#factorize them

data$Ethnicity_Race_Hispanic_White)
data$Ethnicity_Race_Non.Hispanic_Black
data$Ethnicity_Race_Non.Hispanic_White
data$Hypertension_Yes
data$Type.of.labor_Spontaneous
data$Preeclampsia_Yes
data$PPROM_Yes
data$Other.Medically.Indicated_Yes
vec<-c("Ethnicity_Race_Hispanic_White",
       "Ethnicity_Race_Non.Hispanic_Black",
       "Ethnicity_Race_Non.Hispanic_White",
       "Hypertension_Yes",
       "Type.of.labor_Spontaneous",
       "Preeclampsia_Yes",
       "PPROM_Yes",
       "Other.Medically.Indicated_Yes","SDI_score","APGAR.at.1.minute")

data4<-data%>%mutate(across(c(Ethnicity_Race_Hispanic_White,
                              Ethnicity_Race_Non.Hispanic_Black,
                              Ethnicity_Race_Non.Hispanic_White,
                              Hypertension_Yes,
                              Type.of.labor_Spontaneous,
                              Preeclampsia_Yes,
                              PPROM_Yes,
                              Other.Medically.Indicated_Yes),factor))

#number
data$SDI_score
data$APGAR.at.1.minute



# Include Prior Preterm Birth as a covariate
covariates <- c(microplastics, "PriorPretermBirth.Yes",vec)
covariates

# Construct the formula for logistic regression
formula <- as.formula(paste("Delivery_Term ~", paste(covariates, collapse = " + ")))
formula

# Fit the logistic regression model
model <- glm(formula, data = data, family = binomial)

# Display the model summary
summary(model)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif_values <- vif(model)
print(vif_values)

# Calculate adjusted odds ratios (AOR) and 95% confidence intervals
exp_coef <- exp(coef(model))  # Adjusted Odds Ratios
confint_model <- confint(model)  # Confidence intervals
exp_confint <- exp(confint_model)  # Exponentiate confidence intervals

# Combine into a table
aor_table <- data.frame(
  Variable = names(exp_coef),
  Adjusted_Odds_Ratio = exp_coef,
  Lower_CI = exp_confint[, 1],
  Upper_CI = exp_confint[, 2]
)

# Print the AOR table
kable(aor_table,digits = 2)


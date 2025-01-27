# Load necessary libraries
library(tidyverse)
library(car)  # For VIF calculation

# Load the dataset
data <- read.csv("count_tab_log1p.csv")

# Convert Delivery_Term to a factor with "Term" as the reference level
data$Delivery_Term <- factor(data$Delivery_Term, levels = c("Term", "Preterm"))

data<-data%>%mutate(PriorPretermBirth.Yes=PriorTermBirth>0)
data$PriorPretermBirth.Yes

# data<-data%>%select(TissueID,Delivery_Term,PriorPretermBirth.Yes)%>%distinct_all()

# # Ensure Prior.preterm.birth_Yes is a factor
# data$PriorPretermBirth.Yes <- factor(data$PriorPretermBirth.Yes, levels = c(0, 1))
# class(data$PriorPretermBirth.Yes)
# unique(df_clean_long$microplastic)
# 
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
       "Other.Medically.Indicated_Yes",
       # "SDI_score",
       "APGAR.at.1.minute")

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

data4

# Include Prior Preterm Birth as a covariate
covariates <- c(microplastics, "PriorPretermBirth.Yes",vec)
covariates

# Construct the formula for logistic regression
formula <- as.formula(paste("Delivery_Term ~", paste(covariates, collapse = " + ")))
formula

data4


preproc<-preProcess(data4,
                    method = c("center",
                               "scale",
                               "nzv",
                               "zv",
                               "corr"),
                    verbose = T,
                    outcome = data4$Delivery_Term,  
                    cutoff = 0.85)
preproc
df_preproc<-NULL
df_preproc<-predict(preproc,data4)
glimpse(df_preproc)

#TODO fixe prior preterm birth

data4%>%select(-c(colnames(df_preproc)))

colnames(df_preproc)

microplastics <- c("N6","N66","PE","PET","PU", "PP","ABS", "SBR", "PC", "PVC")


model=NULL
# Include Prior Preterm Birth as a covariate
covariates <- c(microplastics, "PriorPretermBirth.Yes",vec)
covariates

# Construct the formula for logistic regression
formula <- as.formula(paste("Delivery_Term ~", paste(covariates, collapse = " + ")))
formula

# Fit the logistic regression model
model <- glm(formula, data = df_preproc, family = binomial)

summary(model)
library(car)
vif(model)

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
library(knitr)
kable(aor_table,digits = 2)
summary(model)

# Load necessary libraries
library(tidyverse)
library(ggpubr)
as_tibble(data)
# Load the dataset
data_tmp <- read.csv("count_tab_log1p.csv")
as_tibble(data_tmp)
# data<-count_tab_log1p
# Convert Delivery_Term to a factor
data$Delivery_Term <- factor(data$Delivery_Term, levels = c("Term", "Preterm"))
# Select microplastic predictors
microplastics <-unique(df_clean_long$microplastic)
as_tibble(data)

microplastics <- c("N6","N66","PE","PET","PU", "PP", "PS", "ABS", "SBR", "PMMA", "PC", "PVC")

data<-as_tibble(data)
data

microplastics
my_comparisons<-list(c("Term","Preterm"))
ggboxplot(data = data,x = "Delivery_Term",y = "PU",add = "jitter")+stat_compare_means(comparisons = my_comparisons)
ggboxplot(data = data,
          x = "Delivery_Term",
          y = "CumulativeMNPs",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")



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
###############################################################################

complete.cases(data)
glimpse(data)
data<-data%>%mutate(PriorPretermBirth.Yes=gsub(0,FALSE,PriorPretermBirth.Yes),
                    PriorPretermBirth.Yes=gsub(1,TRUE,PriorPretermBirth.Yes),
                    PriorPretermBirth.Yes=as.logical(PriorPretermBirth.Yes),
                    SGA=gsub("N",FALSE,SGA),
                    LGA=gsub("Y",TRUE,SGA),
                    SGA=gsub("N",FALSE,LGA),
                    LGA=gsub("Y",TRUE,LGA),
                    SGA=as.logical(SGA),
                    LGA=as.logical(LGA))
glimpse(data)
library(caret)
library(tidyverse)

data2<- data %>%
  mutate(
    across(
      .cols = where(~ all(.x %in% c(0, 1), na.rm = TRUE)),
      .fns  = as.logical
    )
  )
glimpse(data2)
data2
data3<-data2%>%select(Delivery_Term,PE:CumulativeMNPs)

preproc<-preProcess(data3,
                    method = c("center",
                               "scale",
                               "nzv",
                               "zv",
                               "corr"),
                    verbose = T,
                    outcome = data2$Delivery_Term,  
                    cutoff = 0.85)
preproc
df_preproc<-NULL
df_preproc<-predict(preproc,data3)
glimpse(df_preproc)
data3%>%select(-c(colnames(df_preproc)))

colnames(df_preproc)

microplastics <- c("N6","N66","PE","PET","PU", "PP","ABS", "SBR", "PC", "PVC")
formula <- as.formula(paste("Delivery_Term ~", paste(microplastics, collapse = " + ")))

model=NULL
formula
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

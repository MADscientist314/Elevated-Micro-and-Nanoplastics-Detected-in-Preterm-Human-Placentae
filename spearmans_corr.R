mat<-read.table("cleaned_no_variability_columns_sorted.csv",header = T,sep = ",")
colnames(mat)
not_microplastics<-mat%>%select(Gestational_Age:Infections_Group.B.strep)
not_microplastics
as_tibble(mat)

c<-mosaic::cor.test(PE~Gestational_Age,data=mat,method="spearman")
c
corme<-function(X){
  c<-mosaic::cor.test(PE~X,data=mat,method="spearman")
  print(c)
}


lapply(mat%>%select(Gestational_Age:Infections_Group.B.strep),FUN = corme)
################################################################################
# Load required libraries
library(dplyr)
library(mosaic)

# Read data
mat <- read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ",")

# Subset columns (excluding microplastics-related columns)
not_microplastics <- mat %>% select(Gestational_Age:Infections_Group.B.strep)

# Define the correlation function
corme <- function(X, Y = mat$PE) 
  {
  result <- cor.test(Y, X, method = "spearman")
  if (result$p.value<0.05)
    {
    print(result)
    return(result$p.value)  # Optionally return p-values
  }
  else(return(NULL))
}

# Apply the correlation function to all selected columns
cor_results <- lapply(not_microplastics, corme)

# Print a summary of the p-values
print(cor_results)

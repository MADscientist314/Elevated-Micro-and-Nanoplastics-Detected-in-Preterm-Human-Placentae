library(corrplot)
library(pheatmap)
####################################################
df_clean_long
# the objective of this code is to make a correlation matrix with the microplastics
# the objective of this code is to make a correlation matrix with the microplastics
mat<-as_tibble(read.table("cleaned_no_variability_columns_sorted.csv",header = T,sep = ","))
mat
tmp<-df_clean[complete.cases(df_clean),]
############################ data imputation #########################

tmp



nums<-as.matrix(tmp%>%select(where(is.numeric),-c(TissueID)))
nums<-as.matrix(tmp%>%dplyr::select(c(PE:CumulativeMNPs, Gestational_Age,PriorPretermBirth,Birth.weight)))


rownames(nums)<-tmp$TissueID


c<-cor.mtest(nums)
c
n<-cor(nums)
n
corrplot(cor(nums))

##################################################################################
Share


You said:
  df_clean.tsv
File
write code in R to make a correlation plot between the variable in this plot
ChatGPT said:
  ChatGPT

Analyzed
python
Always show details

Copy code
import pandas as pd

# Load the provided TSV file
file_path = '/mnt/data/df_clean.tsv'
df = pd.read_csv(file_path, sep='\t')

# Display the first few rows and column names to check the content
df.head(), df.columns
Result
(   TissueID         PE        PP       ABS        SBR      PMMA        PC  \
  0     13277  10.107121  0.907290       NaN   1.118835       NaN  0.973104   
  1     27915  16.096774  3.836694  0.000000   2.608871  0.633065  0.639113   
  2      8425  13.694391  3.191489  0.388781  17.446808  0.377176  0.272727   
  3      8381  21.608247  6.552577  0.000000   5.358763  0.845361  0.665979   
  4      1759  23.340040  4.426559  0.513078  19.939638  0.714286  0.678068   
  
  PVC        PU        PET  ...  Infections_Lower.UTI  Infections_None  \
  0       NaN  1.100031   2.717170  ...                     0                1   
  1  1.512097  0.427419  11.895161  ...                     0                1   
  2  2.301741  0.626692   1.816248  ...                     0                1   
  3  4.251546  1.006186  15.731959  ...                     0                0   
  4  3.661972  0.418511   1.965795  ...                     0                1   
  
  Infections_Other  Infections_Rubella  Infections_Syphilis  \
  0                 0                   0                    0   
  1                 0                   0                    0   
  2                 0                   0                    0   
  3                 0                   0                    0   
  4                 0                   0                    0   
  
  Newborn.sepsis_Proven  Newborn.sepsis_Suspect  \
  0                      0                       0   
  1                      0                       0   
  2                      0                       0   
  3                      0                       0   
  4                      0                       0   
  
  Newborn.Blood.culture_Not.done  Newborn.Blood.culture_Positive  Delivery  
  0                               0                               0   Preterm  
  1                               0                               0   Preterm  
  2                               0                               0   Preterm  
  3                               0                               0   Preterm  
  4                               0                               0   Preterm  
  
  [5 rows x 89 columns],
  Index(['TissueID', 'PE', 'PP', 'ABS', 'SBR', 'PMMA', 'PC', 'PVC', 'PU', 'PET',
         'N6', 'N66', 'CumulativeMNPs', 'Gestational_Age', 'Maternal_Age_years',
         'Gravida', 'PriorTermBirth', 'PriorPretermBirth', 'SDI_score',
         'PovertyLT100_FPL_score', 'Single_Parent_Fam_score',
         'Education_LT12years_score', 'HHNo_Vehicle_score',
         'HHRenter_Occupied_score', 'HHCrowding_score', 'Nonemployed_score',
         'Birth.weight', 'APGAR.at.1.minute', 'APGAR.at.5.minute',
         'Ethnicity_Race_Hispanic_NotReported', 'Ethnicity_Race_Hispanic_White',
         'Ethnicity_Race_Non.Hispanic_Asian',
         'Ethnicity_Race_Non.Hispanic_Black',
         'Ethnicity_Race_Non.Hispanic_White', 'Delivery_Term_Term', 'Asthma_Yes',
         'Hypertension_Yes', 'Substance.use..alcohol.or.other._Yes',
         'Mental.health.issues_Yes', 'Prior.GDMA1_Yes', 'Prior.GDMA2_Yes',
         'Maternal.smoking_Ever', 'Maternal.smoking_Never',
         'Maternal.alcohol_Ever', 'Maternal.alcohol_Never',
         'Non.reassuring.fetal.status_Yes', 'Uncontrolled.diabetes_Yes',
         'PROM_Yes', 'Gestational.diabetes_Yes', 'Oligohydramnios_Yes',
         'Chorioamnionitis_Yes', 'Type.of.labor_No.Labor',
         'Type.of.labor_Spontaneous', 'Type.of.labor_Spontaneous..Augmented',
         'Preterm.Labor_Yes', 'Other.Medically.Indicated_Yes',
         'Preeclampsia_Yes', 'PPROM_Yes', 'Indicated.Prelabor.CS_Yes',
         'Placental.Abnormality_Yes', 'Fetal.Indication_Yes',
         'Infant.gender_Male', 'Prenatal_Infections_Prenatal_Infection',
         'Infections_Bacterial.vaginosis..Group.B.strep',
         'Infections_Bacterial.vaginosis..Herpes..Group.B.strep..Other',
         'Infections_Bacterial.vaginosis..Lower.UTI..Other',
         'Infections_Bacterial.vaginosis..Other', 'Infections_Chlamydia',
         'Infections_Chorioamnionitis', 'Infections_Group.B.strep',
         'Infections_HIV', 'Infections_Hepatitis.B',
         'Infections_Hepatitis.B..Other', 'Infections_Herpes',
         'Infections_Herpes..Chorioamnionitis',
         'Infections_Herpes..Group.B.strep',
         'Infections_Herpes..Group.B.strep..Lower.UTI',
         'Infections_Herpes..Lower.UTI', 'Infections_Herpes..Lower.UTI..Other',
         'Infections_Lower.UTI', 'Infections_None', 'Infections_Other',
         'Infections_Rubella', 'Infections_Syphilis', 'Newborn.sepsis_Proven',
         'Newborn.sepsis_Suspect', 'Newborn.Blood.culture_Not.done',
         'Newborn.Blood.culture_Positive', 'Delivery'],
        dtype='object'))
The dataset contains 89 variables, including both numeric and categorical columns. I'll now generate an R code to create a correlation plot using the numeric variables. Categorical variables like "Delivery" and other binary factors will be excluded.

Here's the R code:
  
  r
Copy code
# Load required libraries
library(ggcorrplot)
library(dplyr)

# Read the data
df <- read.delim("df_clean.tsv", sep = "\t", header = TRUE)

# Select only numeric variables
numeric_vars <- df %>% select(where(is.numeric))

# Compute the correlation matrix
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Plot the correlation matrix
ggcorrplot(cor_matrix,
           method = "circle",    # Display correlations with circles
           type = "upper",       # Show only the upper part of the matrix
           lab = TRUE,           # Add correlation coefficients
           lab_size = 3,         # Font size for correlation coefficients
           title = "Correlation Plot of Numeric Variables",
           colors = c("blue", "white", "red")) # Color gradient
Steps:
  Install dependencies: Ensure you have ggcorrplot and dplyr installed (install.packages("ggcorrplot", "dplyr")).
Load data: Reads the file using read.delim (modify path as needed).
Filter numeric variables: select(where(is.numeric)) keeps only numeric columns.
Calculate correlations: Uses Pearson's correlation by default with cor() and handles missing data.
Plot: ggcorrplot() visualizes the correlation matrix.
Run this in your R environment to see the correlation plot! If you need further refinements, let me know. ​​






You said:
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
ChatGPT said:
ChatGPT
Your R code is close to performing the desired correlation tests using Spearman's method. However, there is a minor issue: the function corme inside lapply needs to handle its arguments correctly.

Here is the corrected version of your code:
  
  Corrected R Code
r
Copy code
# Load required libraries
library(dplyr)
library(mosaic)

# Read data
mat <- read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ",")

# Subset columns (excluding microplastics-related columns)
not_microplastics <- mat %>% select(Gestational_Age:Infections_Group.B.strep)

# Define the correlation function
corme <- function(X, Y = mat$PE) {
  result <- cor.test(Y, X, method = "spearman")
  print(result)
  return(result$p.value)  # Optionally return p-values
}

# Apply the correlation function to all selected columns
cor_results <- lapply(not_microplastics, corme)

# Print a summary of the p-values
print(cor_results)
Key Changes and Fixes:
  corme Function:
  
  The function accepts X as input, where X represents columns passed from lapply.
The second argument, Y, is fixed to mat$PE for the correlation.
Output:
  
  The function now optionally returns the p-value from cor.test() while printing the full result.
lapply Usage:
  
  lapply runs the corme function on each column from not_microplastics.
Output:
  The script will print the correlation test result for each column in not_microplastics with respect to PE. It also gathers the p-values into cor_results for easy interpretation.

Let me know if you need further modifications or explanations! 🚀






You said:
  ################################################################################
# Load required libraries
library(dplyr)
library(mosaic)

# Read data
mat <- read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ",")

# Subset columns (excluding microplastics-related columns)
not_microplastics <- mat %>% select(Gestational_Age:Infections_Group.B.strep)

# Define the correlation function
corme <- function(X, Y = mat$PE) {
  result <- cor.test(Y, X, method = "spearman")
  if result$p.value<0.05{
    print(result)
    return(result$p.value)  # Optionally return p-values
  }
}
ChatGPT said:
  ChatGPT
Your code contains a syntax error because the if statement needs proper parentheses. Here is the corrected version:
  
  Fixed Code
r
Copy code
################################################################################
# Load required libraries
library(dplyr)
library(mosaic)

# Read data
mat <- read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ",")

# Subset columns (excluding microplastics-related columns)
not_microplastics <- mat %>% select(Gestational_Age:Infections_Group.B.strep)

# Define the correlation function
corme <- function(X, Y = mat$PE) {
  result <- cor.test(Y, X, method = "spearman")
  if (result$p.value < 0.05) {  # Corrected 'if' syntax
    print(result)
    return(result$p.value)  # Optionally return p-values
  }
}

# Apply the correlation function to all selected columns
significant_results <- lapply(not_microplastics, corme)

# Remove NULL results (non-significant correlations)
significant_results <- Filter(Negate(is.null), significant_results)

# Print significant results
print(significant_results)
Key Fixes and Additions:
  Correct if Syntax: Use if (condition) instead of if condition.
Filter NULL Values: The lapply function may return NULL for non-significant tests. Use Filter() to remove those.
Optional p-Values: You can return p-values for significant correlations.
Output:
  Correlation test results (only for significant tests where p < 0.05) will be printed.
The significant p-values are collected into significant_results.
Run this updated code, and it will correctly display and filter significant Spearman correlations! 🚀






You said:
  ok now I want to loop through not just PE but all the microplastics in the mat:
  PE        PP       ABS       SBR      PMMA        PC       PVC        PU      PET         N6       N66 CumulativeMNPs
ChatGPT said:
  ChatGPT
To loop through all the microplastics variables (PE, PP, ABS, SBR, PMMA, etc.) and test their correlation against the not_microplastics variables, you can use a nested loop or a combination of lapply and tidyverse functions.

Here’s the updated R code to achieve this:
  
  Updated R Code
r
Copy code
################################################################################
# Load required libraries
library(dplyr)
library(mosaic)

# Read data
mat <- read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ",")

# Subset columns
not_microplastics <- mat %>% select(Gestational_Age:Infections_Group.B.strep)

# List of microplastics variables
microplastics <- mat %>% select(PE, PP, ABS, SBR, PMMA, PC, PVC, PU, PET, N6, N66, CumulativeMNPs)

# Define the correlation function
corme <- function(X, Y) {
  result <- cor.test(Y, X, method = "spearman")
  if (result$p.value < 0.05) {
    cat("Correlation between", deparse(substitute(X)), "and", deparse(substitute(Y)), ":\n")
    print(result)
    return(result$p.value)
  }
}

# Nested loop to perform correlations between all microplastics and not_microplastics
results <- list()

for (mp_var in colnames(microplastics)) {
  for (nm_var in colnames(not_microplastics)) {
    p_value <- corme(mat[[nm_var]], mat[[mp_var]])
    if (!is.null(p_value)) {
      results[[paste(mp_var, nm_var, sep = "_vs_")]] <- p_value
    }
  }
}

# Print significant results
print("Significant correlations (p < 0.05):")
print(results)
Explanation:
  Microplastics Variables:
  
  We select all the microplastics columns into a microplastics object using select().
corme Function:
  
  A function that takes two variables (X and Y), computes their Spearman correlation, and prints results if p.value < 0.05.
Nested Loop:
  
  Outer loop: Iterates over all columns in microplastics.
Inner loop: Iterates over all columns in not_microplastics.
Computes correlation for each pair and stores significant results.
Results Storage:
  
  Significant correlations and their p-values are stored in a named list results.
Output:
  
  The significant correlations (variable pairs) and their p-values are displayed.
Output Example:
  If correlations are significant between PE and Gestational_Age, and between ABS and Infections_Group.B.strep, the output might look like this:
  
  plaintext
Copy code
Correlation between Gestational_Age and PE:
  Spearman's rank correlation rho: 0.45
    p-value: 0.001

Correlation between Infections_Group.B.strep and ABS:
    Spearman's rank correlation rho: -0.35
p-value: 0.03

Significant correlations (p < 0.05):
  $PE_vs_Gestational_Age
[1] 0.001

$ABS_vs_Infections_Group.B.strep
[1] 0.03
This approach ensures all possible combinations are tested and significant results are collected and displayed. Let me know if you need further adjustments! 🚀






You said:
  whi is this plot so small?
  library(corrplot)
library(pheatmap)
####################################################
df_clean_long
# the objective of this code is to make a correlation matrix with the microplastics
# the objective of this code is to make a correlation matrix with the microplastics
mat<-as_tibble(read.table("cleaned_no_variability_columns_sorted.csv",header = T,sep = ","))
mat
tmp<-df_clean[complete.cases(df_clean),]
############################ data imputation #########################

tmp



nums<-as.matrix(tmp%>%select(where(is.numeric),-c(TissueID)))
nums<-as.matrix(tmp%>%dplyr::select(c(PE:CumulativeMNPs, Gestational_Age,PriorPretermBirth,Birth.weight)))


rownames(nums)<-tmp$TissueID


c<-cor.mtest(nums)
c
n<-cor(nums)
n
corrplot(cor(nums))
ChatGPT said:
  ChatGPT
The issue with your plot size might arise from the default plotting window size in your R environment or how you are running the code (e.g., RStudio or other IDEs).

Here’s how you can fix it and ensure a larger and clearer correlation plot:
  
  Fixing the Plot Size and Appearance
Increase the Plot Window Size: Use the dev.new() function to open a larger plotting window.

Customize the corrplot(): Add arguments to control the visual details like type, tl.cex, and mar.

Full Corrected Code:
  
  r
Copy code
# Load libraries
library(corrplot)
library(dplyr)

# Read data
mat <- as_tibble(read.table("cleaned_no_variability_columns_sorted.csv", header = TRUE, sep = ","))
mat
# Subset the numeric columns for microplastics and other variables
nums <- as.matrix(mat %>% dplyr::select(c(PE:CumulativeMNPs, Gestational_Age, PriorPretermBirth, Birth.weight)))
df2$`Birth weight percentile`
# Set rownames for clarity
rownames(nums) <- mat$TissueID

# Compute correlation matrix

n <- cor(nums, use = "pairwise.complete.obs", method = "spearman")  # Use pairwise for missing data
n2<-cor.mtest(nums)

n2
# Plot correlation matrix with increased size
dev.off()
dev.new(width = 10, height = 10)  # Open a larger plot window


png(file=paste0("Figures/corrplot2.png"),
    res=300, 
    width=2000, 
    height=2000)

corrplot(n,
         method = "square",
         p.mat = n2$p,
         insig = "label_sig",
         tl.col = "black", 
         tl.cex = 1,
         pch.col = 'black',
         is.corr = T,
         order = "AOE",
         type = "lower",
         pch.cex = 1,)



dev.off()

########################################################################################
c$corrPos<-c$corrPos%>%mutate(q.value=p.adjust(p.value,n = 120))
c$corrPos%>%filter(q.value<0.05)


write.csv(c$corr,file = "spearmans_corr.csv",row.names = T)
write.csv(c$corrPos,file = "spearmans_corrPOS_with_qvalue.csv",row.names = T)

n2


ggsave(filename = "Figures/corrplot.png",
       device = "png",
       dpi = 600,
       width = 10,
       height = 10,
       units = "in")

# Load necessary libraries
library(tidyverse)
library(car)  # For VIF calculation

# Load the dataset
data <- read.csv("count_tab_log1p.csv")

# Convert Delivery_Term to a factor with "Term" as the reference level
data$Delivery_Term <- factor(data$Delivery_Term, levels = c("Term", "Preterm"))

#subset the variable we are going to fix
data2<-as_tibble(data)%>%select(TissueID,Delivery_Term,PriorPretermBirth)%>%distinct_all()

#convert it to a factor class
unique(data2$PriorPretermBirth)



TB<-data2%>%filter(PriorPretermBirth<1)%>%mutate(PriorPretermBirth.Yes=0)
PTB<-data2%>%filter(PriorPretermBirth>=1)%>%mutate(PriorPretermBirth.Yes=1)
data3<-full_join(TB,PTB)
tab<-tally(Delivery_Term~PriorPretermBirth.Yes,data=data3)
kable(tab)

perc_tab<-tally(Delivery_Term~PriorPretermBirth.Yes,data=data3,"perc")
kable(perc_tab)
chisq.test(tab)
data<-full_join(data,data3)
as_tibble(data$PriorPretermBirth.Yes)

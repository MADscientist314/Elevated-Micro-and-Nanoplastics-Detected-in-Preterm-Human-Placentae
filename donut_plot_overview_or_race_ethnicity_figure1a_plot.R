library(tidyverse)
library(readxl)
library(mosaic)
# df <- read_excel("Table1-allmeta LexieEdit.xlsx")
# df$Delivery_Term
# tally(Ethnicity_Race ~ Delivery_Term,)
# round(tally(Ethnicity_Race ~ Delivery_Term, df, "perc"), 2)

library(plotly)

# Create the data
ethnicity_race <- c(
  "Hispanic_Black", "Hispanic_NotReported", "Hispanic_White",
  "Non-Hispanic_Asian", "Non-Hispanic_Black", "Non-Hispanic_White"
)

# Preterm counts
preterm_counts <- c(1, 1, 60, 5, 13, 7)

# Term counts
term_counts <- c(3, 4, 27, 19, 19, 18)

# Define a common color palette for both charts
my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

# Function to create a donut chart with outside labels
create_donut_chart <- function(labels, values, title) {
  plot_ly(
    labels = labels,
    values = values,
    type = 'pie',
    hole = 0.5,  # Creates the donut hole
    textinfo = 'label+percent',
    textposition = 'outside',
    marker = list(
      colors = my_colors,
      line = list(color = '#FFFFFF', width = 1)
    )
  ) %>%
    layout(
      title = list(
        text = title,
        font = list(
          family = "Arial Bold",
          size = 20,
          color = "#000000"
        )
      ),
      font = list(
        family = "Arial Bold",
        size = 19,
        color = "#000000"
      ),
      showlegend = FALSE
    )
}

# Donut chart for Preterm
preterm_donut <- create_donut_chart(ethnicity_race, preterm_counts,title = NULL)

# Donut chart for Term
term_donut <- create_donut_chart(ethnicity_race, term_counts,title=NULL)

# Display the charts
preterm_donut
term_donut

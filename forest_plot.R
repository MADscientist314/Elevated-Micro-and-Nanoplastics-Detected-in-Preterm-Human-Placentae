# -------------------------------------------------------------------
# Example Forest Plot in R using ggplot2
# -------------------------------------------------------------------

# Load required packages
library(ggplot2)
library(dplyr)

# Convert your matrix "aor_table" to a data frame
df_forest <- as.data.frame(aor_table)%>%filter(Adjusted_Odds_Ratio<1000)
# Create a new column for variable names, drawn from row names
df_forest$Variable <- rownames(aor_table)

# Optional: remove the intercept row if you don’t want it in the forest plot
df_forest <- df_forest %>% mutate(Variable=rownames(df_forest))%>%
  filter(Variable != "(Intercept)")

# Reorder variables so they appear in a neat vertical order 
# (last variable at the bottom)
df_forest$Variable <- factor(df_forest$Variable, levels = rev(df_forest$Variable))

# Build the forest plot
ggplot(df_forest, aes(x = Adjusted_Odds_Ratio, 
                      y = Variable)) +
  geom_point(shape = 18, size = 3) + 
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), 
                 height = 0.2) +
  # Add a vertical line at x=1 (no effect line)
  geom_vline(xintercept = 1, linetype = "dashed") +
  # Usually we use a log scale for OR
  scale_x_log10() +
  # Labels and a clean theme
  xlab("Adjusted Odds Ratio (log scale)") +
  ylab("") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line()
  )

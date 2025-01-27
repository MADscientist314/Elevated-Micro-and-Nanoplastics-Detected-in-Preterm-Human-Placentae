count_tab_log1p
data
df
library(ggpubr)
library(ggsci)
library(tidyverse)
library(mosaic)
################################################################################
count_tab_log1p<-count_tab_log1p%>%
  mutate(Maternal.smoking_Never=factor(Maternal.smoking_Never),
         Gestational.diabetes_Yes=factor(Gestational.diabetes_Yes),
         Preeclampsia_Yes=factor(Preeclampsia_Yes))

my_comparisons<-list(c("0","1"))
ggboxplot(data = count_tab_log1p,
          x = "Maternal.smoking_Never",
          y = "CumulativeMNPs",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")


ggboxplot(data = count_tab_log1p,
          x = "Gestational.diabetes_Yes",
          y = "CumulativeMNPs",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")

ggboxplot(data = count_tab_log1p,
          x = "Preeclampsia_Yes",
          y = "CumulativeMNPs",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")
ggscatter(data = count_tab_log1p,
          x = "Gestational_Age",
          y = "CumulativeMNPs",
          add = "reg.line",
          cor.coef = T)+scale_color_viridis_c(option = "B")

ggplot(data = count_tab_log1p,
       mapping = aes(x = Gestational_Age,
                     y = CumulativeMNPs,
                     colour =CumulativeMNPs))+
  geom_point()+
  ggtitle(label = "Cumulative MNPs (log1p)")+
  stat_cor(method = "spearman")+
  ylab(label = " Concentration (μg/g)")+
  geom_smooth(method = "lm")+
  scale_color_viridis_c(option = "B")+
  theme_pubr(legend = "right")
################################################################################
range(df$PC)
df
ggplot(data = df,
       mapping = aes(x = Birth.weight,
                     y = PU))+
  geom_point()+
  # ggtitle(label = "Cumulative MNPs (log1p)")+
  stat_cor(method = "pearson",label.y.npc = "bottom")+
  ylab(label = " Concentration (μg/g)")+
  # yscale("log10")+
  geom_smooth(method = "lm")+
  scale_color_viridis_c(option = "B")+
  theme_pubr(legend = "right")
################################################################################


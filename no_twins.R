library(tidyverse)
library(readxl)
library(mosaic)
library(ggpubr)
library(ggsci)
getwd()

# Data import
# Load the data
df <- as_tibble(read.table(file="Table1-notwins_allmeta_master_wrangling_ML_one-hot_1246-averaged.csv",sep=",", header=TRUE))   
df2 <- as_tibble(read_excel("Table1-allmeta LexieEdit2.xlsx"))   

################################################################################
# Data wrangling
# get a vector of the twins
twins<-df2%>%
  select(TissueID,`Number of babies`)%>%
  filter(`Number of babies`>1)
df
df_clean<-df%>%filter(!TissueID%in%twins$TissueID)
df_clean
df_clean<-df_clean%>%mutate(Delivery=factor(Delivery_Term,
                                            levels=c("Preterm","Term")))




df_clean
#TODO
#TODO
#look at Normalizing the data to show a TSS / CLR of the sum of the microplastics
#such that pe doesnt obsfucate the results with PS (PS.csv file from teams chat)

#Then do a multivariate Logistic regression with each plastic.

#TODO
#TODO





################################################################################
# set the pallete and the comparisons
my_pal<-pal_npg()(10)
my_pal
my_comparisons<-list(c("Preterm","Term"))

################################################################################
shapiro.test(df_clean$CumulativeMNPs)

# Figure 1B - violin chart with cumulative MNPS
Fig1b<-ggviolin(data = df_clean,
         x = "Delivery",
         y = "CumulativeMNPs",
         add = "jitter",
         color = "Delivery",
         add.params = list(shape=16),
         trim = F,
         ylab = "Concentration (μg/g)")+
  scale_color_manual(values = my_pal)+
  ylim(c(0,1000))+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox",label.y = 960)+
  theme_pubr(legend = "none",base_size = 16,base_family = "",border = T)
Fig1b
ggsave(filename = "Figures/Fig1b_violin_CMNPs.png",
       plot = Fig1b,
       device = "png",
       width = 10,
       height = 5,
       units = "in",
       dpi = 600)

################################################################################
# Figure 1c - bar chart with individual microplastics comparisons
df_clean
df_clean_long<-df_clean%>%
  select(TissueID,PE:CumulativeMNPs,Delivery)%>%
  pivot_longer(cols = c(PE:CumulativeMNPs),names_to = "microplastic",values_to = "value")%>%
  filter(!is.na(value))%>%
  distinct_all()

range(df_clean_long$value)
glimpse(df_clean_long)
df_clean_long<-df_clean_long%>%mutate(log_value=log1p(value))
range(df_clean_long$log_value)




# Create the plot
Fig1c<-ggplot(df_clean_long, aes(x = Delivery, y = log_value, fill = Delivery)) +
  geom_bar(stat = "summary", 
           fun = "mean", 
           position = position_dodge(),
           color = "black") +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2,
                                              dodge.width = 0.9), 
              size = 1, alpha = 1, color = "black") +
  #yscale("log10")+
  #scale_y_continuous(trans = "log10", limits = c(1, 1000), breaks = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_fill_manual(values = my_pal)+
  facet_wrap(~microplastic,nrow = 1,scales = "free")+
  stat_compare_means(comparisons = my_comparisons,method = "t")+
  labs(y = "Concentration (µg/mL)", x = NULL, fill = NULL) +
  
  theme_pubr(legend = "none", base_size = 16,border = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# arrange them correctly
unique(df_clean_long$microplastic)
df_clean_long<-df_clean_long%>%
  mutate(microplastic=factor(microplastic,levels=c("CumulativeMNPs",
                                                   "ABS",
                                                   "N6",
                                                   "N66"
                                                   "PC",
                                                   "PE",
                                                   "PET",
                                                   
                                                   "PP",
                                                   "SBR",
                                                   "PMMA",
                                                   "PVC",
                                                   "PU",
)))



Fig1c<-ggviolin(data = df_clean_long,x = "Delivery",
         y = "value",
         add = "jitter",
         color = "Delivery",
         ylab = "Concentration (µg/g)",xlab = "",
         trim = T)+
  facet_wrap(~microplastic,scales = "free",nrow = 1)+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(legend = "right", base_size = 16,border = T) 
  
Fig1c
ggsave(filename = "Figures/Fig1c_bar_MNPs.png",
       plot = Fig1,
       device = "png",
       width = 20,
       height = 5,
       units = "in",
       dpi = 600)
################################################################################
write.table(df_clean,"df_clean.tsv",sep = "\t",row.names = F)

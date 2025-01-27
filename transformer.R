library(tidyverse)
library(microbiome)
library(ggpubr)
library(HTSSIP)

#import the dataset and convert the tissueID to factor class
df<-as_tibble(read.table("withPS.csv",sep=",",header = T))
df<-df%>%mutate(TissueID=factor(paste0("pt_",TissueID)))
# make a data frame with just the metadata
df_meta<-df%>%select(-c(PE:CumulativeMNPs))
# make a data frame with just the data
df_plastic<-df%>%select(TissueID,PE:N66)
#pivot the plastic to longer
df_plastic<-df_plastic%>%pivot_longer(cols = -TissueID)
#now pivot back to wifder using the pt_ID column
df_plastic<-df_plastic%>%pivot_wider(names_from = TissueID)
#convert from a tibble to a dataframe and remove the TissueID
df_plastic2<-data.frame(df_plastic)%>%select(-name)
#annotate the rownames
rownames(df_plastic2)<-df_plastic$name


##################### TRANSFORMATIONS ##########################################
# make a copy in case you mess it up
count_tab<-df_plastic2
count_tab_log1p<-t(transform(x = count_tab,transform = "log10p"))
count_tab_scale<-t(transform(x = count_tab,transform = "scale"))
count_tab
count_tab_tss<-t(t(tss(x = t(count_tab),na.rm = T)))
count_tab_tss
count_tab_log1p
# convert all NA to zero
count_tab[is.na(count_tab)]<-0
#now to the remaining transformations
count_tab_clr<-t(transform(x = count_tab,transform = "clr"))

count_tab_norm<-t(transform(x = count_tab,transform = "norm"))
count_tab_rel<-t(transform(x = count_tab,transform = "compositional"))


#add in the rownames as TissueID again
count_tab_log1p<-as_tibble(count_tab_log1p)%>%mutate(TissueID=factor(rownames(count_tab_log1p)))
count_tab_scale<-as_tibble(count_tab_scale)%>%mutate(TissueID=factor(rownames(count_tab_scale)))
count_tab_tss<-as_tibble(count_tab_tss)%>%mutate(TissueID=factor(rownames(count_tab_tss)))
count_tab_clr<-as_tibble(count_tab_clr)%>%mutate(TissueID=factor(rownames(count_tab_clr)))
count_tab_norm<-as_tibble(count_tab_norm)%>%mutate(TissueID=factor(rownames(count_tab_norm)))
count_tab_rel<-as_tibble(count_tab_rel)%>%mutate(TissueID=factor(rownames(count_tab_rel)))

#bring in the metadata
count_tab_log1p<-full_join(count_tab_log1p,df_meta)
count_tab_scale<-full_join(count_tab_scale,df_meta)
count_tab_tss<-full_join(count_tab_tss,df_meta)
count_tab_clr<-full_join(count_tab_clr,df_meta)
count_tab_norm<-full_join(count_tab_norm,df_meta)
count_tab_rel<-full_join(count_tab_rel,df_meta)

#add the total
count_tab_log1p
count_tab_clr
# convert all NA to zero
count_tab_scale[is.na(count_tab_scale)]<-0

count_tab_log1p<-count_tab_log1p%>%
  group_by(TissueID)%>%
  mutate(CumulativeMNPs=mosaic::sum(c(PE,PP,PS,ABS,SBR,PMMA,PC,PVC,PU,PET,N6,N66),na.rm = T))%>%
  ungroup()
count_tab_scale<-count_tab_scale%>%
  group_by(TissueID)%>%
  mutate(CumulativeMNPs=mosaic::sum(c(PE,PP,PS,ABS,SBR,PMMA,PC,PVC,PU,PET,N6,N66),na.rm = T))%>%
  ungroup()
count_tab_log1p
#count the Cumulative sum of the transformed microplastics

count_tab_scale<-count_tab_scale%>%mutate(CumulativeMNPs=PE+PP+PS+ABS+SBR+PMMA+PC+PVC+PU+PET+N6+N66)
count_tab_tss<-count_tab_tss%>%mutate(CumulativeMNPs=PE+PP+PS+ABS+SBR+PMMA+PC+PVC+PU+PET+N6+N66)
count_tab_clr<-count_tab_clr%>%mutate(CumulativeMNPs=PE+PP+PS+ABS+SBR+PMMA+PC+PVC+PU+PET+N6+N66)
count_tab_norm<-count_tab_norm%>%mutate(CumulativeMNPs=PE+PP+PS+ABS+SBR+PMMA+PC+PVC+PU+PET+N6+N66)
count_tab_rel<-count_tab_rel%>%mutate(CumulativeMNPs=PE+PP+PS+ABS+SBR+PMMA+PC+PVC+PU+PET+N6+N66)

#reorder the dataframe
count_tab_log1p<-count_tab_log1p%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)
count_tab_scale<-count_tab_scale%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)
count_tab_tss<-count_tab_tss%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)
count_tab_clr<-count_tab_clr%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)
count_tab_norm<-count_tab_norm%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)
count_tab_rel<-count_tab_rel%>%select(TissueID,Delivery_Term,PE:N66,CumulativeMNPs,Gestational_Age:LGA)


count_tab_log1p
count_tab_scale
count_tab_tss
count_tab_clr
count_tab_norm
count_tab_rel
############################DATA VISUALITION ####################################
my_comparisons<-list(c("Preterm","Term"))
#test for normality
shapiro.test(count_tab_log1p$CumulativeMNPs) #normal
shapiro.test(count_tab_scale$CumulativeMNPs) #not normal
shapiro.test(count_tab_tss$CumulativeMNPs) #not normal
shapiro.test(count_tab_clr$CumulativeMNPs) #normal
shapiro.test(count_tab_norm$CumulativeMNPs)# not normal
shapiro.test(count_tab_rel$CumulativeMNPs) # not normal

library(mosaic)
library(ggsci)
library(knitr)

kable(favstats(CumulativeMNPs~Delivery_Term,data=count_tab_log1p))

# ggviolin(data = count_tab_log1p,
#           x = "Delivery_Term",
#           y = "CumulativeMNPs",
#           add = "jitter",trim = T)+
#   stat_compare_means(comparisons = my_comparisons, method = "wilcox")
# # 
# ggsave(filename = "CumulativeMNPS_count_tab_log1p_violin.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# ggboxplot(data = count_tab_scale,x = "Delivery_Term",y = "CumulativeMNPs",add = "jitter")+stat_compare_means(comparisons = my_comparisons, method = "wilcox")
# ggsave(filename = "CumulativeMNPS_count_tab_scale.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# 
# ggboxplot(data = count_tab_tss,x = "Delivery_Term",y = "CumulativeMNPs",add = "jitter")+stat_compare_means(comparisons = my_comparisons, method = "wilcox")
# ggsave(filename = "CumulativeMNPS_count_tab_tss.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# count_tab_clr
# 
# ggboxplot(data = count_tab_clr,x = "Delivery_Term",y = "CumulativeMNPs",add = "jitter")#+stat_compare_means(comparisons = my_comparisons,method = "t")
# ggsave(filename = "CumulativeMNPS_count_tab_clr.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# 
# ggboxplot(data = count_tab_norm,x = "Delivery_Term",y = "CumulativeMNPs",add = "jitter")+stat_compare_means(comparisons = my_comparisons, method = "wilcox")
# ggsave(filename = "CumulativeMNPS_count_tab_norm.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# 
# ggboxplot(data = count_tab_rel,x = "Delivery_Term",y = "CumulativeMNPs",add = "jitter")+stat_compare_means(comparisons = my_comparisons, method = "wilcox")
# ggsave(filename = "CumulativeMNPS_count_tab_rel.png",device = "png",width = 5,height = 5,units = "in",dpi = 600)
# 
# write.table(count_tab_log1p,"count_tab_log1p.csv",sep = ",",row.names = F)
# write.table(count_tab_scale,"count_tab_scale.csv",sep = ",",row.names = F)
# write.table(count_tab_tss,"count_tab_tss.csv",sep = ",",row.names = F)
# write.table(count_tab_clr,"count_tab_clr.csv",sep = ",",row.names = F)
# write.table(count_tab_norm,"count_tab_norm.csv",sep = ",",row.names = F)
# # write.table(count_tab_rel,"count_tab_rel.csv",sep = ",",row.names = F)
################################################################################


################################################################################
shapiro.test(count_tab_log1p$CumulativeMNPs)
# set the pallete and the comparisons
my_pal<-pal_npg()(10)
my_pal
my_comparisons<-list(c("Preterm","Term"))
range(count_tab_log1p$CumulativeMNPs)

# Figure 1B - violin chart with cumulative MNPS
Fig1b<-ggviolin(data = count_tab_log1p,
                x = "Delivery_Term",
                y = "CumulativeMNPs",
                add = "jitter",
                color = "Delivery_Term",
                add.params = list(shape=16),
                trim = T,
                xlab = "",
                title = "Cumulative MNPs (log1p)",
                ylab = "Concentration (μg/g)")+
  scale_color_manual(values = my_pal)+
  # ylim(c(0,1000))+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")+#,label.y = 960)+
  theme_pubr(legend = "none",base_size = 16,base_family = "",border = T)
Fig1b
ggsave(filename = "Figures/Fig1b_violin_CMNPs.png",
       plot = Fig1b,
       device = "png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 600)

################################################################################
# Figure 1c - bar chart with individual microplastics comparisons
count_tab_log1p

library(ggpubr)


ggscatter(data = count_tab_log1p,x = "Gestational_Age",y = "CumulativeMNPs",cor.coef = T)

df_clean_long<-count_tab_log1p%>%
  select(TissueID,PE:N66,Delivery_Term)%>%
  pivot_longer(cols = c(PE:N66),
               names_to = "microplastic",values_to = "value")%>%
  filter(!is.na(value))%>%
  distinct_all()
df_clean_long%>%filter(is.na(microplastic))
count_tab_log1p

range(df_clean_long$value)
glimpse(df_clean_long)
df_clean_long
# Create the plot
Fig1c<-ggplot(df_clean_long, aes(x = Delivery_Term, 
                                 y = value, 
                                 fill = Delivery_Term)) +
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
  stat_compare_means(comparisons = my_comparisons,method = "wilcox")+
  labs(y = "log1p concentration (µg/g)", x = NULL, fill = NULL) +
  
  theme_pubr(legend = "none", base_size = 16,border = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
Fig1c
# arrange them correctly
unique(df_clean_long$microplastic)
df_clean_long<-df_clean_long%>%
  mutate(microplastic=factor(microplastic,levels=c("ABS",
                                                   "N6",
                                                   "N66",
                                                   "PC",
                                                   "PE",
                                                   "PET",
                                                   "PP",
                                                   "PS",
                                                   "SBR",
                                                   "PMMA",
                                                   "PVC",
                                                   "PU"
  )))



Fig1c<-ggviolin(data = df_clean_long,x = "Delivery_Term",
                y = "value",
                add = "jitter",
                color = "Delivery_Term",
                ylab = "log1p concentration (µg/g)",xlab = "",
                trim = T)+
  facet_wrap(~microplastic,scales = "free",nrow = 1)+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(legend = "right", base_size = 16,border = T) 

Fig1c
ggsave(filename = "Figures/Fig1c_bar_MNPs.png",
       plot = Fig1c,
       device = "png",
       width = 20,
       height = 5,
       units = "in",
       dpi = 600)
################################################################################




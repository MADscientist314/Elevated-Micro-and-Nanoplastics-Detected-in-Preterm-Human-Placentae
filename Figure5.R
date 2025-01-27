df_clean
df_long<-df_clean%>%
  select(TissueID,
         Delivery,
         Gestational_Age,
         c(PE:CumulativeMNPs),
         Preeclampsia_Yes,
         Gestational.diabetes_Yes,
         Maternal.smoking_Never,
         Maternal.smoking_Ever,Prenatal_Infections_Prenatal_Infection,
         Chorioamnionitis_Yes,
         Placental.Abnormality_Yes,
         Education_LT12years_score)%>%
  pivot_longer(cols = c(PE:CumulativeMNPs),names_to = "microplastic")
df_long<-df_long%>%
  mutate(across(.cols=c(Preeclampsia_Yes:Placental.Abnormality_Yes),
                function(X)
                  {
                  gsub("0","No",X)
                  }))%>%
  mutate(across(.cols=c(Preeclampsia_Yes:Placental.Abnormality_Yes),
                function(X)
                {
                  gsub("1","Yes",X)
                }))%>%
  mutate(across(.cols=c(Preeclampsia_Yes:Placental.Abnormality_Yes),
                function(X)
                {
                  as.factor(X)
                }))
my_comparisons<-list(c("No","Yes"))
pre<-ggviolin(df_long,
          x = "Preeclampsia_Yes",add = "jitter",
          ylab = "Concentration (µg/g)",
         color="Preeclampsia_Yes",#add.params = list(color="Delivery"),
          y = "value",trim = T)+
  facet_wrap(~microplastic,scales = "free")+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(base_size = 16,legend = "none")+
  scale_color_manual(values = c(my_pal[2],my_pal[1]))

pre
ggsave(filename = "Figures/Preeclampsia_violin.png",
       plot = pre,
       device = "png",
       width = 15,
       height = 15,
       units = "in",
       dpi = 600)
################################################################################
df_long$Maternal.smoking_Never
my_comparisons<-list(c("No","Yes"))
smoke<-ggboxplot(df_long,
              x = "Maternal.smoking_Never",add = "jitter",
              ylab = "Concentration (µg/g)",
              color="Maternal.smoking_Never",#add.params = list(color="Delivery"),
              y = "value")+
  facet_wrap(~microplastic,scales = "free")+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(base_size = 16,legend = "none")+
  scale_color_manual(values = c(my_pal[2],my_pal[1]))

smoke
ggsave(filename = "Figures/Preeclampsia_violin.png",
       plot = smoke,
       device = "png",
       width = 15,
       height = 15,
       units = "in",
       dpi = 600)

###############################################################################
df_long$Maternal.smoking_Ever
my_comparisons<-list(c("No","Yes"))
smoke<-ggboxplot(df_long,
                 x = "Maternal.smoking_Never",add = "jitter",ylab = "Concentration (µg/g)",
                 color="Maternal.smoking_Never",#add.params = list(color="Delivery"),
                 y = "value")+
  facet_wrap(~microplastic,scales = "free")+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(base_size = 16,legend = "none")+
  scale_color_manual(values = c(my_pal[2],my_pal[1]))
smoke
smoke2<-df_long%>%select(TissueID,Maternal.smoking_Never,Maternal.smoking_Ever)%>%distinct_all()
data.frame(smoke2)

ggsave(filename = "Figures/smoke_box.png",
       plot = smoke,
       device = "png",
       width = 15,
       height = 15,
       units = "in",
       dpi = 600)
###############################################################################
###############################################################################
###############################################################################
###############################################################################
tally(Delivery~Gestational.diabetes_Yes,df_clean,"perc")
chisq.test(tally(Delivery~Gestational.diabetes_Yes,df_clean))

my_comparisons<-list(c("No","Yes"))
gd<-ggboxplot(df_long,
                 x = "Gestational.diabetes_Yes",add = "jitter",
                 color="Gestational.diabetes_Yes",#add.params = list(color="Delivery"),
                 y = "value",          ylab = "Concentration (µg/g)",)+
  facet_wrap(~microplastic,scales = "free")+
  stat_compare_means(comparisons = my_comparisons)+
  theme_pubr(base_size = 16,legend = "none")+
  scale_color_manual(values = c(my_pal[2],my_pal[1]))

gd
# gd<-df_long%>%select(TissueID,Maternal.smoking_Never,Maternal.smoking_Ever)%>%distinct_all()
data.frame(smoke2)

ggsave(filename = "Figures/gestational_diabetes_box.png",
       plot = gd,
       device = "png",
       width = 15,
       height = 15,
       units = "in",
       dpi = 600)

###############################################################################
tally(Delivery~Prenatal_Infections_Prenatal_Infection,df_clean)
chisq.test(tally(Delivery~Prenatal_Infections_Prenatal_Infection,df_clean))

my_comparisons<-list(c("No","Yes"))
gd<-ggboxplot(df_long,
              x = "Prenatal_Infections_Prenatal_Infection",add = "jitter",
              color="Prenatal_Infections_Prenatal_Infection",#add.params = list(color="Delivery"),
              y = "value",          ylab = "Concentration (µg/g)",)+
  facet_wrap(~microplastic,scales = "free")+
  stat_compare_means(comparisons = my_comparisons, hide.ns = T)+
  theme_pubr(base_size = 16,legend = "none")+
  scale_color_manual(values = c(my_pal[2],my_pal[1]))

gd
# gd<-df_long%>%select(TissueID,Maternal.smoking_Never,Maternal.smoking_Ever)%>%distinct_all()
data.frame(smoke2)

ggsave(filename = "Figures/Prenatal_Infections_Prenatal_Infection_box.png",
       plot = gd,
       device = "png",
       width = 15,
       height = 15,
       units = "in",
       dpi = 600)

###############################################################################




# Figure 3
################################################################################
#correlation scatter plots with coefficients
df_clean
df_long<-df_clean%>%select(TissueID,Delivery,Gestational_Age,c(PE:CumulativeMNPs))%>%pivot_longer(cols = c(PE:CumulativeMNPs),names_to = "microplastic")

df_long
dd<-ggscatter(data = df_long,
          x = "Gestational_Age",
          y = "value",color = "Gestational_Age",
          ylab = "Concentration (µg/g)",
          add = "reg.line",
          cor.coef = T,cor.method = "spearman",
          cor.coeff.args = list(label.y.npc="top"),
          cor.coef.size =4,
          conf.int = T)+
  facet_wrap(~microplastic,scales = "free")+
 theme_pubr(legend = "right",base_size = 16)+
  scale_color_viridis_c(option = "A")
dd<-ggpar(dd,xticks.by = 2)

dd
ggsave(filename = "Figures/Fig3.png",
       plot = dd,
       device = "png",
       width = 15,
       height = 10,
       units = "in",
       dpi = 600)

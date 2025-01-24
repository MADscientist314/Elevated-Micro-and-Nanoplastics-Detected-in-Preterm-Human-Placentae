df3_long<-df3%>%pivot_longer(cols = c(BaP,BbF,DBA),names_to = "PAH",values_to = "PAH_count")
df3_long_PAH<-df3_long%>%select(pt_id,PAH,PAH_count)%>%distinct_all()
df3_long
df3_long_PAH
gghistogram(data = df3_long,x = "PAH_count",color = "PAH",fill = "PAH",bins = 80)#+xscale("log10")
#preprocess the data
library(ggpubr)
library(ggsci)
ggbarplot(data = df3_long,
          x = "pt_id",
          y = "PAH_count",
          color = "PAH",
          fill = "PAH",
          xlab = "Placenta",
          ylab = "PAH levels (ng/mg)",
          position = position_stack(),palette = "igv",
          ggtheme = theme_pubr(legend = "right"))+
  rotate_x_text()
ggsave(filename = "diversity_of_PAH.png",
       dpi = 300,
       device = "png",
       width = 8,
       height = 6,units = "in")
df3_long$N6
df3_long_microplastics<-df3_long%>%
  pivot_longer(cols = c(Polyethylene:N6),
               names_to = "Microplastics",values_to = "microplastics_count")%>%
  select(pt_id,Microplastics,microplastics_count)%>%distinct_all()

ggbarplot(data = df3_long_microplastics,
          x = "pt_id",
          y = "microplastics_count",
          color = "Microplastics",
          fill = "Microplastics",
          xlab = "Placenta",
          ylab = "Microplastics levels (mg/g)",
          position = position_stack(),palette = "igv",
          ggtheme = theme_pubr(legend = "right"))+
  rotate_x_text()

ggsave(filename = "diversity_of_microplastics.png",
       dpi = 300,
       device = "png",
       width = 10,
       height = 6,units = "in")
unique(df3_long_microplastics$microplastics_count)
df3_wide_microplastics<-df3_long_microplastics%>%
  # mutate(microplastics_count=gsub(NA,0,microplastics_count))%>%
  pivot_wider(names_from = Microplastics,values_from = microplastics_count,values_fill = 0)

df3_wide_microplastics
# Replace NAs with 0s
df3_wide_microplastics[is.na(df3_wide_microplastics)] <- 0
glimpse(df3_wide_microplastics%>%filter(pt_id=="pt_6"))

df3_wide_microplastics
df3_wide_microplastics<-df3_wide_microplastics%>%
  group_by(pt_id)%>%
  mutate(tot=Polyethylene+
           Polyvinyl_Chloride+
           Nylon+
           Polyethylene_Terephthalate+ 
           Styrene_Butadiene+
           Polycarbonate+ 
           Polypropylene+ 
           Polystyrene+ 
           Acrylonitrile_Butadiene_Styrene+ 
           PMMA +
           Polyurethane+ N6)
glimpse(df3_wide_microplastics%>%filter(pt_id=="pt_6"))

df3_wide_microplastics<-df3_wide_microplastics%>%
  mutate(across(everything(),function(X) X/tot))
glimpse(df3_wide_microplastics%>%filter(pt_id=="pt_6"))
#convert NA to zero
df3_long_microplastics_perc<-df3_wide_microplastics%>%
  select(-tot)%>%
  pivot_longer(cols = -pt_id,names_to = "Microplastics")%>%mutate(value=value*100)

ggbarplot(data = df3_long_microplastics_perc,
          x = "pt_id",
          y = "value",
          color = "Microplastics",
          fill = "Microplastics",
          xlab = "Placenta",
          ylab = "Proportional Microplastics levels (%)",
          position = position_stack(),palette = "igv",
          ggtheme = theme_pubr(legend = "right"))+
  rotate_x_text()

ggsave(filename = "diversity_of_microplastics_perc.png",
       dpi = 300,
       device = "png",
       width = 10,
       height = 6,units = "in")

########### PAH PROP ###############
df3_long_PAH
df3_wide_PAH<-df3_long_PAH%>%
  # mutate(microplastics_count=gsub(NA,0,microplastics_count))%>%
  pivot_wider(names_from = PAH,values_from = PAH_count,values_fill = 0)

df3_wide_PAH
df3_wide_PAH<-df3_wide_PAH%>%
  group_by(pt_id)%>%
  mutate(tot=BaP+BbF+DBA)

glimpse(df3_wide_PAH%>%filter(pt_id=="pt_6"))

df3_wide_PAH<-df3_wide_PAH%>%
  mutate(across(everything(),function(X) X/tot))
glimpse(df3_wide_PAH%>%filter(pt_id=="pt_6"))
#convert NA to zero
df3_long_PAH_perc<-df3_wide_PAH%>%
  select(-tot)%>%
  pivot_longer(cols = -pt_id,names_to = "PAH")%>%mutate(value=value*100)



ggbarplot(data = df3_long_PAH_perc,
          x = "pt_id",
          y = "value",
          color = "PAH",
          fill = "PAH",
          xlab = "Placenta",
          ylab = "Proportional PAH levels (%)",
          position = position_stack(),palette = "igv",
          ggtheme = theme_pubr(legend = "right"))+
  rotate_x_text()
ggsave(filename = "diversity_of_PAH_perc.png",
       dpi = 300,
       device = "png",
       width = 8,
       height = 6,units = "in")





############## LATER #######
# ggballoonplot(data = df3_long_microplastics,
#               x = "Microplastics",
#               y = "pt_id",
#               color = "Microplastics",
#               fill = "Microplastics",
#               size = "microplastics_count",#Microplastics levels (mg/g)",
#               rotate.x.text = T)+
#   scale_color_igv()+scale_fill_igv()+theme_pubr(legend = "none")
############## LATER #######
############## LATER #######
# 
# unique(df3_long$microplastics)
# df3_long%>%select(pt_id,microplastics,microplastics_count)%>%distinct_all()

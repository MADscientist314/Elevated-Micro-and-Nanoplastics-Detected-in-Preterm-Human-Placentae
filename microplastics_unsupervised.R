library(tidyverse)
library(mosaic)
library(ggpubr)
library(ggsci)
library(caret)

#import the data
df<-as_tibble(read.table(file = "pearson.input.csv",header = T,sep = ","))
glimpse(df)
# DATA WRANGLING
#remove the last column that doesnt have anything in it
df<-df%>%select(-c(X))
#add an index column
df<-df%>%mutate(pt_id=paste0("pt_",rownames(df)))
df$Birth_weight
glimpse(df)
colnames(df)
#convert the appropriate columns to factor class
#ordered class
df_ordered<-df%>%
  select(pt_id,APGAR_1min,APGAR_5min,SDI_score)%>%
  mutate(APGAR_1min=as.ordered(APGAR_1min),
         APGAR_5min=as.ordered(APGAR_5min),
         SDI_score=as.ordered(SDI_score))
#factor class
df_factor<-df%>%
  select(pt_id,Race,Ethnicity,Offspring_Biological_Sex,Delivery_Route,Superfund_Status)%>%
  mutate(Superfund_Status=gsub(1,TRUE,Superfund_Status),
         Superfund_Status=gsub(2,FALSE,Superfund_Status),
         Race_Black=gsub(4,TRUE,Race),
         Race_Black=gsub(5,FALSE,Race_Black),
         Race_White=gsub(4,TRUE,Race),
         Race_White=gsub(5,FALSE,Race_White),
         Ethnicity_Hispanic=gsub(1,TRUE,Ethnicity),
         Ethnicity_Hispanic=gsub(2,FALSE,Ethnicity_Hispanic),
         Sex_Male=gsub(1,FALSE,Offspring_Biological_Sex),
         Sex_Male=gsub(2,TRUE,Sex_Male),
         Sex_Female=gsub(2,FALSE,Offspring_Biological_Sex),
         Sex_Female=gsub(1,TRUE,Sex_Female),
         Delivery_Spontaneous=gsub(1,TRUE,Delivery_Route),
         Delivery_Spontaneous=gsub(2,FALSE,Delivery_Spontaneous),
         Delivery_Cesarean=gsub(2,TRUE,Delivery_Route),
         Delivery_Cesarean=gsub(1,FALSE,Delivery_Cesarean))%>%
  select(-c(Race,Ethnicity,Offspring_Biological_Sex,Delivery_Route))%>%
  mutate(across(-c(pt_id),as.logical))
#numeric class
glimpse(df)
df_numeric<-df%>%select(pt_id,BaP:Maternal_Age,Length,Birth_weight,GA_w)
df_ordered$Superfund_Status

class(df$Birth_weight)

df_numeric$Birth_weight
#join them together
df2<-full_join(df_numeric,df_factor)
df2<-full_join(df2,df_ordered)
#lets remove them for now
# TODO ask about  SumFTIR and SumPy.GC.MS
#lets remove them for now
df3<-df2%>%select(-c(SumPy.GC.MS,SumPAHs))
#bring back in the FTIR column
df3
###################### Plot the data ###########################
# #make a composite score of the PAH to use as a target
# df3_long<-df3%>%pivot_longer(cols = c(BaP,BbF,DBA),names_to = "PAH",values_to = "PAH_count")
# df3_long
# gghistogram(data = df3_long,x = "PAH_count",color = "PAH",fill = "PAH",bins = 80)#+xscale("log10")
#preprocess the data
#convert it to a data frame with rownames
df3.5<-data.frame(df3%>%select(-pt_id))
df3.5
rownames(df3.5)<-df3$pt_id
df2$Birth_weight
preproc<-preProcess(df3.5,
                    method = c("center","scale","knnImpute", "corr", "zv", "nzv"),
                    thresh = 0.95,
                    cutoff = 0.95,
                    verbose = T)
summary(preproc)
df3.5
df4<-predict(preproc,df3.5)
df4
# vec<-data.frame("a"=colnames(df3.5))
# vec2<-data.frame("a"=colnames(df4))
# vec3<-anti_join(vec,vec2)
# vec3
# we dropped these three variables:
# a
# 1 Styrene_Butadiene
# 2     Polycarbonate
# 3      Polyurethane
# > 

df4.5<-df4%>%
  mutate(PAH_composite=BaP+BbF+DBA,
         microplastics_composite=Polyethylene+
           Polyvinyl_Chloride+
           Nylon+
           Polyethylene_Terephthalate+
           Polypropylene+
           Polystyrene+
           Acrylonitrile_Butadiene_Styrene+
           PMMA+
           N6)
glimpse(df4.5)
colnames(df4.5)
colnames(df5)
df5.1<-df4.5%>%
  dplyr::select(-c(Sex_Male,
                   Delivery_Spontaneous,
                   Delivery_Cesarean,
                   Ethnicity_Hispanic,
                   Race_Black,
                   Length,
                   Maternal_Age,
                   Birth_weight,
                   SDI_score,
                   APGAR_5min,
                   GA_w,
                   Race_White,
                   Sex_Female,
                   SumFTIR,))
colnames(df5.1)
df5$BaP
cor()
d<-cor(df5.1%>%select(where(is.numeric)))
d2<-cor.mtest(df5.1%>%select(where(is.numeric)))

range(d)
corrplot(d,
         method = "square",
         p.mat = d2$p,
         insig = "label_sig",
         tl.col = "black", 
         tl.cex = 1,
         pch.col = 'black',
         is.corr = T,
         order = "AOE",
         type = "lower",
         pch.cex = 1)
         
# write.table(d,"correlation_values.tsv",sep = "\t",row.names = F)
# df5<-df4%>%mutate(across(where(is.ordered),as.numeric))


# write.table(d,"pearson_results.tsv",sep = "\t")

# ######### FIGURE THYME ####
# ggscatter(df4.5,x = "PAH_composite",y = "Birth_weight",color = "Birth_weight",
#           cor.coef = T,
#           cor.coeff.args = list(label.x.npc="center"),
#           conf.int = T,
#           add = "reg.line",
#           # cor.coeff.args = "pearson"
# )+
#    # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# 
# ggsave(filename = "PAH_composite_birthweight_not_fixed.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# ggscatter(df,x = "DBA",y = "Birth_weight",color = "Birth_weight",
#           cor.coef = T,
#           cor.coeff.args = list(label.x.npc="center"),
#           conf.int = T,
#           add = "reg.line",
#           # cor.coeff.args = "pearson"
# )+
#   # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# 
# ggsave(filename = "DBA_birthweight.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# ggscatter(df,x = "BaP",y = "Birth_weight",color = "Birth_weight",
#           cor.coef = T,
#           cor.coeff.args = list(label.x.npc="center"),
#           conf.int = T,
#           add = "reg.line",
#           # cor.coeff.args = "pearson"
# )+
#    # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# 
# ggsave(filename = "BaP_birthweight.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# 
# ggscatter(df,x = "DBA",
#           y = "GA_w",color = "GA_w",
#           cor.coef = T,
#           cor.coeff.args = list(label.x.npc="center"),
#           conf.int = T,
#           add = "reg.line",
#           # cor.coeff.args = "pearson"
# )+
#   # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# 
# 
# 
# ggsave(filename = "DBA_Ga_W.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# 
# ggscatter(df,x = "Nylon",
#           y = "APGAR_1min",
#           color = "APGAR_1min",
#           cor.coef = T,margin.plot = "density",
#           cor.coeff.args = list(label.x.npc="center",method="spearman"),
#           conf.int = T,
#           add = "reg.line",
#           palette = "gsea",
#            # cor.coeff.args = "spearman"
# )+
#   # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# 
# 
# 
# ggsave(filename = "Nylon_APGAR1min.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# 
# ggscatter(df,x = "Polypropylene",
#           y = "APGAR_1min",
#           color = "APGAR_1min",
#           cor.coef = T,margin.plot = "density",
#           cor.coeff.args = list(label.x.npc="center",method="spearman"),
#           conf.int = T,
#           add = "reg.line",
#           palette = "gsea",
#           # cor.coeff.args = "spearman"
# )+
# # coord_fixed()+
# scale_color_viridis_c()+theme_pubr(legend = "right")
# ggsave(filename = "Polypropylene_APGAR1min.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# 
# df$SDI_score
# ggscatter(df,x = "Nylon",
#           y = "SDI_score",
#           color = "SDI_score",
#           cor.coef = T,margin.plot = "density",
#           cor.coeff.args = list(label.x.npc="center",method="spearman"),
#           conf.int = T,
#           add = "reg.line",
#           palette = "gsea",
#           # cor.coeff.args = "spearman"
# )+
#   # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# ggsave(filename = "Nylon_SDI_score.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# 
# ggscatter(df,x = "Polyethylene",
#           y = "SDI_score",
#           color = "SDI_score",
#           cor.coef = T,margin.plot = "density",
#           cor.coeff.args = list(label.x.npc="center",method="spearman"),
#           conf.int = T,
#           add = "reg.line",
#           palette = "gsea",
#           # cor.coeff.args = "spearman"
# )+
#   # coord_fixed()+
#   scale_color_viridis_c()+theme_pubr(legend = "right")
# ggsave(filename = "Polyethylene_SDI_score.png",
#        dpi = 300,
#        device = "png",
#        width = 10,
#        height = 6,units = "in")
# 
# cor.test(df$Polyethylene,df$SDI_score,method="spearman")
################ ITS ON THE WRONG END OF p=0.05 #######################
# Spearman's rank correlation rho
# 
# data:  x and y
# S = 19639, p-value = 0.05018
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2937348 

################ ITS ON THE WRONG END OF p=0.05 #######################
##################### create the dissimilarity matrix  ##########################

library(cluster)

#' Compute Gower distance
# df4<-df4%>%mutate(Superfund_Status=as.logical(Superfund_Status))
colnames(df5)
df5.1
gower_dist <- daisy(df5.1, metric = "gower")
gower_dist
gower_mat <- as.matrix(gower_dist)
gower_mat2<-as_tibble(gower_mat)%>%mutate(pt=rownames(gower_mat))
corrplot(gower_mat,)
M<-gower_mat
corrplot(M, order = 'hclust',type="full",method="square", hclust.method = 'ward.D2', addrect = 4)

#' Print most similar patients
most_disparate<-df[which(gower_mat2 == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]


#' Print most dissimilar patients
most_similar<-df[which(gower_mat2 == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
glimpse(most_similar)

# lets look for a number of clusters that are both meaningful and easy to remember, 
#i.e. 2 to 8 maximum. The silhouette figure helps us identify the best option(s).
set.seed(314)
#
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

dev.off()
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 4
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
pam_results[2,2]
######### Visualize your clusters
library(Rtsne)
tsne_obj2 <- Rtsne(X = gower_dist, is_distance = TRUE, perplexity = 15,max_iteration=1000)
saveRDS(tsne_obj2,"tsne_obj2.RDS")
tsne_data <- tsne_obj2$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
# library(corrplot)
# distmat<-as.matrix(gower_dist)
# library(pheatmap)
# distmat

# df5.5<-df5%>%
#   mutate(PAH_composite=BaP+BbF+DBA,
#          microplastics_composite=Polyethylene+
#            Polyvinyl_Chloride+
#            Nylon+
#            Polyethylene_Terephthalate+
#            Polypropylene+
#            Polystyrene+
#            Acrylonitrile_Butadiene_Styrene+
#            PMMA+
#            N6)

# pheatmap(distmat,
#          cutree_cols = 5,
#          cutree_rows = 5,
#          cellwidth = 12,
#          cellheight = 12,
#          fontsize = 14,
#          border_color = NA)#,
#          # legend_breaks = c(10,20,30,40,500),
#          annotation_row = df4.5[,c(1:3,26)],
#          annotation_col = df4.5[,c(4:14,27)])
# dev.off()  
# annot_col<-df5.5%>%
#   mutate(across(where(is.logical),as.character))%>%
#   select(c(where(is.ordered),where(is.character)))
# 
# dim(annot_col)
# dim(annot_col)
# 
# 
# pheatmap(distmat,
#          # cluster_rows = F,
#          # cluster_cols = F,
#           cutree_cols = 5,
#          cutree_rows = 5,
#          cellwidth = 12,
#          cellheight = 12,
#          fontsize = 14,
#          border_color = NA,
#          annotation_row = annot_col[6:11],
#          annotation_col =annot_col[1:5])
dev.off()
df_tmp<-cbind(df5,tsne_data)

df_tmp<-df_tmp%>%arrange(desc(PAH_composite))

# df_tmp<-NULL
df_tmp
# ggballoonplot(data = df_tmp,x = "pt_id",y = "PAH_composite")
ggpar(ggscatter(data = df_tmp,
                x = "X",
                y = "Y",
                shape = "cluster",
                color = "cluster"),
                    # ggtheme = theme_minimal(),star.plot = T,
                    # margin.plot = "hist",
                    # fill = "cluster"),
                legend = "right")
colnames(df5.1)

ggsave(filename = "tsne_PAM_cluster12123.png",
       dpi = 300,
       device = "png",
       width = 10,
       height = 6,units = "in")
################################################################################

vec<-colnames(df_tmp)
vec
lapply(vec, function(VAR) {
  ggsave(filename = paste0(VAR,"_PAM_121223.png"),
         plot = ggpar(ggscatter(data = df_tmp,
                  x = "X",
                  y = "Y",
                  shape = "cluster",
                  color = VAR,
                  fill = "cluster"),legend = "right"), dpi = 300,
         device = "png",
         width = 10,
         height = 6,units = "in")
})

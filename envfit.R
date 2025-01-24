scores(tsne_obj2)
library(vegan)
library(MASS)

fit <- envfit(gower_dist, 
              tmp4,
              perm = 9999, na.rm=TRUE)

saveRDS(fit,"fit.RDS")
tmp4


vec<-scores(fit, "vectors")
fac<-scores(fit, "factors")
vec
fac

library(ggsci)
colors<-pal_igv()(4)
map=setNames(pal_igv()(4),c(1,2,3,4))

tsne_data<-tsne_data%>%
  mutate(colors=cluster)
tsne_data$colors=map[tsne_data$cluster]
tsne_data

plot(tsne_data$X,tsne_data$Y,col=tsne_data$colors)
plot(fit, col ="black", p.max=0.05)

tsne_data

fit
dev.off()
a<-plot(tsne_data$X,tsne_data$Y,col=tsne_data$colors)+
plot(fit, p.max = 0.065, col ="black",cex=0.75)
a
ggsave(plot = a,
       device = "png",
       filename = "envfit_03142024.png",
       width = 8,
       height = 8,
       dpi = 600,
       units = "in")


# 
# plot(fit,# labels=list(factors = paste("M", c(1,2,4,5), sep = "")),
#      # bg = rgb(0.005,0.005,0.005,0.005),
#      cex=0.95)
# 
# plot(fit)
# ordispider(gower_dist,groups = df$PMMA,display = "sites",)
# plot(fit, p.max = 0.05)
# ## Adding fitted arrows to CCA. We use "lc" scores, and hope
# ## that arrows are scaled similarly in cca and envfit plots
# ord <- cca(varespec ~ Al + P + K, varechem)
# plot(ord, type="p")
# fit <- envfit(ord, varechem, perm = 999, display = "lc")
# plot(fit, p.max = 0.05, col = "red")
# ## 'scaling' must be set similarly in envfit and in ordination plot
# plot(ord, type = "p", scaling = "sites")
# fit <- envfit(ord, varechem, perm = 0, display = "lc", scaling = "sites")
# plot(fit, col = "red")
# 
# ## Class variables, formula interface, and displaying the
# ## inter-class variability with ordispider, and semitransparent
# ## white background for labels (semitransparent colours are not
# ## supported by all graphics devices)
# 
# data(dune)
# data(dune.env)
# ord <- cca(dune)
# dune.env
# fit <- envfit(ord ~ Moisture + A1, dune.env, perm = 0)
# plot(ord, type = "n")
# with(df5.5, ordispider(tsne_obj, cluster, col="skyblue"))
# with(dune.env, points(ord, display = "sites", col = as.numeric(Moisture),
#                       pch=16))
# plot(fit, cex=1.2, axis=TRUE, bg = rgb(1, 1, 1, 0.5))
# ## Use shorter labels for factor centroids
# labels(fit)
# plot(ord)
# plot(fit, labels=list(factors = paste("M", c(1,2,4,5), sep = "")),
#      bg = rgb(1,1,0,0.5))
# ################################################################################
# 
# 

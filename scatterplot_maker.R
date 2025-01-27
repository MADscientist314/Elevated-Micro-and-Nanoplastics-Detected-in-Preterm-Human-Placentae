ga<-df%>%select(TissueID,CumulativeMNPs,Gestational_Age)%>%distinct_all()
library(ggpubr)
ga
colnames(df[2:14])
ggscatter(data = df,
          x = "Gestational_Age",
          y = "CumulativeMNPs",
          add = "reg.line",
          # rug = T,
          conf.int = T,
          cor.coef = T,
          cor.method = "spearman")+
  yscale("log10")
ggscatter(data = df,
          x = "Gestational_Age",
          y = "PE",
          add = "reg.line",
          cor.coef = T, 
          cor.method = "spearman")+
  yscale("log10")

ggscatter(data = df,
          x = "Gestational_Age",
          y = "N66",
          add = "reg.line",
          cor.coef = T, 
          cor.method = "spearman")+
  yscale("log10")


lapply(colnames(df[2:14]), FUN = function(X){
  a<-ggscatter(data = df,
            x = "Gestational_Age",
            y = X,
            title = X,
            add = "reg.line",
            # rug = T,
            conf.int = T,
            cor.coef = T,
            cor.method = "spearman")+
    yscale("log10")
  ggsave(filename = paste0(X,"_vs_GA.png"),
         width = 5,
         height = 5,
         units = "in",device = "png",dpi = 600,path = "scatterplots")
})

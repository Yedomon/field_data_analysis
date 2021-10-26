
# Data analysis 
# Correlation
# Yedomon Ange Bovys Zoclanclounon

# Date 25th October 2021



#### Libraries

library(patchwork)
library(FactoMineR)
library(factoextra)


#Set working directory

setwd("C:/Users/ange_/Downloads/lala_analysis/correlation/gifford/pca")


# Data loading

data_pca = read.csv("pca_data.csv", h = T, sep = ";", dec = ",", row.names = 1)


##acp

res.pca = PCA(data_pca[1:16], scale = TRUE, graph=F)



# Visualize eigenvalues/variances

fviz_screeplot(res.pca, addlabels = TRUE, title = "")


# Contributions of variables to PC1
contrib1 = fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, title = "(A) Principal component 1", xtickslab.rt = 0)


# Contributions of variables to PC2
contrib2 = fviz_contrib(res.pca, choice = "var", axes = 2, top = 10, title = "(B) Principal component 2", xtickslab.rt = 0)


# Contributions of variables to PC3
contrib3 = fviz_contrib(res.pca, choice = "var", axes = 3, top = 10, title = "(C) Principal component 3", xtickslab.rt = 0)


# Contributions of variables to PC4
contrib4 = fviz_contrib(res.pca, choice = "var", axes = 4, top = 10, title = "(D) Principal component 4", xtickslab.rt = 0)



### Merge the four graphs

(contrib1 | contrib2) /
  (contrib3 | contrib4)



# Control variable colors using their contributions

dim12 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(A)",
                     axes = c(1, 2)
                     )

dim13 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(B)",
                     axes = c(1, 3)
)



dim14 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(C)",
                     axes = c(1, 4)
)

### Representing the plot of thwe most contributing variables following axes


(dim12 | dim13 | dim14)



### HCPC on ACP


res.hcpc = HCPC(res.pca, graph = FALSE)


### get the clusters

clust = res.hcpc$data.clust


### Save in csv file


write.csv(clust, "clusters.csv")



### Biplot habillage cluster

grp_cluster = as.factor(data_pca[, "clust"])


clusterA = fviz_pca_biplot(res.pca, 
                           repel = FALSE,
                           col.var = "black",
                           geom.var = "text",
                           label = "var",
                           title = "",
                           habillage = grp_cluster,  # color by groups)
                           palette = c("#00AFBB", "#E7B800", "#FC4E07", "#0796fc")
) +
  theme(legend.position="top") +
  theme_bw()

clusterA 



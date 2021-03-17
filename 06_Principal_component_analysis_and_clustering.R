##--Script for Principal component analysis and clustering
##--By Yedomon Ange Bovys Zoclanclounon | twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Science | Department of Genomics | Department of Genomics |RDA | Republic of South Korea



#### Libraries

library(FactoMineR)
library(factoextra)


#### Data

data_acp= read.csv("ACP.csv", h=T, sep = ",", row.names = 1)

View(data_acp)


##acp

res.pca = PCA(data_acp[1:16], scale = TRUE, graph=F)

## Gt the contributions of the variables

res.pca$var$contrib

## Get the correlation with each dimension

res.pca$var$cor



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


# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10, title = "")



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


(dim12) /
  (dim13) /
  (dim14)




### Biplot habillage infloresence type

grp_inflo = as.factor(data_acp[, "Inflorescence_type"])

inflo = fviz_pca_biplot(res.pca, 
                        repel = FALSE,
                        col.var = "black",
                        geom.var = "text",
                        label = "var",
                        title = "A: Growth habit",
                        habillage = grp_inflo, # color by groups)
                        palette = c("#00AFBB", "#E7B800", "#FC4E07")
)+
  theme(legend.position="top")


### Biplot habillage Branching_grouping

grp_branch = as.factor(data_acp[, "Branching_grouping"])

branching  = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "B: Branching type",
                             habillage = grp_branch, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
)+
  theme(legend.position="top")




### Biplot habillage Flower_color

grp_flocol = as.factor(data_acp[, "Flower_color"])

flowercol  = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "C: Flower color",
                             habillage = grp_flocol, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top")



### Biplot habillage seed color

grp_seedcol = as.factor(data_acp[, "Seed_color_grouping"])


seed_color = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "D: Seed color",
                             habillage = grp_seedcol, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top")


### pLOT THE FOUR GRAPHS TOGETHER


inflo + branching + flowercol + seed_color


### Biplot habillage region of origin

grp_region = as.factor(data_acp[, "Region"])


region = fviz_pca_biplot(res.pca, 
                         repel = F,
                         col.var = "black",
                         geom.var = "text",
                         #label = "var",
                         title = "",
                         habillage = grp_region #, # color by groups)
                         #palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") 


region


### HCPC on ACP


res.hcpc = HCPC(res.pca, graph = FALSE)


### get the clusters

clust = res.hcpc$data.clust


### Save in csv file


write.csv(clust, "clusters.csv")


### Biplot habillage cluster

grp_cluster = as.factor(data_acp[, "Cluster"])


clusterA = fviz_pca_biplot(res.pca, 
                           repel = FALSE,
                           col.var = "black",
                           geom.var = "text",
                           label = "var",
                           title = "",
                           habillage = grp_cluster,  # color by groups)
                           palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()




##########################################PCA Core Oil data included######################################

### Packages

library(FactoMineR)
library(factoextra)
library(patchwork)

### Data importation

data_pca = read.csv("acp_oil_agro_core.csv", h=T, row.names = 1)


### Scaled the data

df = scale(data_pca[1:24])


### PCA

res.pca = PCA(df, scale = TRUE, graph=F)


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


# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10, title = "")



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


(dim12) /
  (dim13) /
  (dim14)



### HCPC on ACP


res.hcpc = HCPC(res.pca, graph = FALSE)


### get the clusters

clust_oil = res.hcpc$data.clust


### Save in csv file


write.csv(clust_oil, "clusters_oil.csv")


### Biplot habillage cluster

grp_cluster = as.factor(data_pca[, "Cluster"])


biplot_core = fviz_pca_biplot(res.pca, 
                              repel = TRUE,
                              col.var = "black",
                              geom.var = "text",
                              #abel = "var",
                              title = "(A) Core collection",
                              show.legend = FALSE,
                              #addEllipses = TRUE,
                              habillage = grp_cluster, #, # color by groups)
                              palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()





# Top 30 most contributing individuals


biplot_top_30_contrib = fviz_pca_biplot(res.pca, 
                                        repel = TRUE,
                                        select.ind = list(contrib = 30),
                                        col.var = "black",
                                        geom.var = "text",
                                        #label = "var",
                                        title = "(B) Top 30 most contributing accessions",
                                        show.legend = FALSE,
                                        #addEllipses = TRUE,
                                        habillage = grp_cluster, #, # color by groups)
                                        palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()




# Top 30 ind according to the cos2: select.ind = list(cos2 = 30)


biplot_top_30_cos2 = fviz_pca_biplot(res.pca, 
                                     repel = TRUE,
                                     select.ind = list(cos2 = 30),
                                     col.var = "black",
                                     geom.var = "text",
                                     #label = "var",
                                     title = "(C) Top 30 accessions based on cos2 value",
                                     show.legend = FALSE,
                                     #addEllipses = TRUE,
                                     habillage = grp_cluster, #, # color by groups)
                                     palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()



### Plot the three graphs together


(biplot_core)/
  (biplot_top_30_contrib) /
  (biplot_top_30_cos2)


# Description of clusters

res.hcpc


### DENFROGRAMME VIEW

fviz_cluster(res.hcpc, 
             habillage = grp_cluster,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             show.legend = FALSE, title = "" ) +
  theme_bw()


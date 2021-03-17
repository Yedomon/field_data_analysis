### ACP 

##STR

#Data importation

acp_ws_data = read.csv("stracp_new.csv", sep = ";", h=T, dec=",", row.names = 1)
head(acp_ws_data)



##Scaling
df2 <- scale(acp_ws_data)

##Packages

library(FactoMineR)
library(factoextra)
library(ggthemes)

##ACP 
res.pca = PCA(df2, graph = FALSE)

#res.pca = PCA(acp_ws_data, graph = FALSE)
# description des variables
dimdesc(res.pca)



##Clustering on ACP
reshcpc = HCPC(res.pca, graph=FALSE)
names(reshcpc)
reshcpc$desc.var


###Mining clusters
###Cluster
strclust = reshcpc$data.clust
strclust
write.csv(strclust,"C:/Users/user/Documents/Mystrclust.csv", row.names = FALSE)


#Cluster described by var
vardescstr=reshcpc$desc.var
vardescstr



#Cluster described by ind
inddescstr=reshcpc$desc.ind
inddescstr


#axes principaux assosi? aux clusters
reshcpc$desc.axes




#Visualization of cluster
#STR
clstr = fviz_cluster(reshcpc, repel = TRUE,
                     show.clust.cent = TRUE,
                     show.legend = FALSE,
                     ggtheme = theme_minimal(),
                     palette = c("darkgreen","dodgerblue","orangered"),
                     main = '(D): Cluster Plot'
)+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
 


clstr 







##Visualization k_colors = ("#00AFBB","#2E9FDF","#E7B800","#FC4E07")
strdend= fviz_dend(reshcpc, repel = TRUE, cex = 0.8, type = "rectangle", 
                   k_colors = c("orangered","dodgerblue","darkgreen"),
                   main = "",
                   rect = TRUE,
                   rect_fill = TRUE,
                   horiz = TRUE,
                   #type = 'circular'
                   rect_border = c("orangered","dodgerblue","darkgreen")
)#title = "(A): Water Stress Scheme")
strdend

##Visualization

#Data
data1 = read.csv("str_acp_clust.csv", sep = ",", h=T, row.names = 1)#STR

head(data1)


res.pcagain = PCA(data1, quali.sup=4, graph=FALSE)#STR

##habillage
grp <- as.factor(data1[, "clust"])#STR

##Biplot 
biplotstr = fviz_pca_biplot(res.pcagain,col.var = "black",  habillage=grp, 
                            palette = c("darkgreen","dodgerblue","orangered"),
                            label = "var",
                            geom.var = c("text"),
                            addEllipses = FALSE, repel = TRUE, title = "(C): PCA Biplot")+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )

biplotstr




# library(cowplot)

# h = plot_grid(biplotstr,clstr, labels = c('(A): PCA Biplot','(B): Cluster Plot'), label_size = 12)


# h = plot_grid(biplotstr,strdend, labels = c('(A): PCA Biplot','(B): Dendrogram Plot'), label_size = 12)

# ggsave("strn1.pdf", limitsize = FALSE, width = 23.89, height = 18.16, plot = h)
# ggsave("strn1234.pdf", limitsize = FALSE, width = 16.89, height = 12.16, plot = h)

library(patchwork)

g = biplotstr + clstr

g

# ggsave("str2.pdf", limitsize = FALSE, width = 23.89, height = 18.16, plot = g)


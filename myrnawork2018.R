---
  title: "RNA Seq Data Analysis"
author: "By Yedomon"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
social: menu
---
  
  ```{r setup, include=FALSE}
library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)

#Data set

dth = read.csv("C:/Users/YEDOMON/Documents/his.csv", h=T, sep = ",")

```

Distribution
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Distribution of raw normalized fpkm data
  
  ```{r}
p  <- ggplot(dth, aes(fkpm_value, colour=Samples, fill=Samples))
p  <- p + geom_density(alpha=0.55)
p  <- p + facet_wrap(~Samples)
p  <- p + facet_grid(rows = vars(Samples))
ggplotly(p)
```


### Distribution of log10(fpkm) data

```{r}
p  <- ggplot(dth, aes(log10fpkm, colour=Samples, fill=Samples))
p  <- p + geom_density(alpha=0.55)
p  <- p + facet_wrap(~Samples)
p  <- p + facet_grid(rows = vars(Samples))
ggplotly(p)
```

Row
-----------------------------------------------------------------------
  
  ### Surimposed distribution of log10(fpkm)
  
  ```{r}
p  <- ggplot(dth, aes(log10fpkm, colour=Samples, fill=Samples))
p  <- p + geom_density(alpha=0.2)
ggplotly(p)
```

### Distribution of log2(fkpm)

```{r}
p  <- ggplot(dth, aes(log2fpkm, colour=Samples, fill=Samples))
p  <- p + geom_density(alpha=0.55)
p  <- p + facet_wrap(~Samples)
p  <- p + facet_grid(rows = vars(Samples))
ggplotly(p)
```

Venn diagram
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between 5leaf and 6 leaf samples
  
  ```{r}
fvs = read.csv("C:/Users/YEDOMON/Documents/fvss.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(fvs, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("dodgerblue", "goldenrod1"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("dodgerblue", "goldenrod1")
)
```



### Number of overlaped genes between 5leaf and root samples

```{r}
fvr = read.csv("C:/Users/YEDOMON/Documents/fvr.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(fvr, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("dodgerblue", "seagreen3"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("dodgerblue", "seagreen3")
)


```

Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between 5leaf and shoot samples
  
  ```{r}
fvs = read.csv("C:/Users/YEDOMON/Documents/fvs.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(fvs, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("dodgerblue", "darkgray"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("dodgerblue", "darkgray")
)
```


### Number of overlaped genes between 5leaf and stem samples

```{r}
fvst = read.csv("C:/Users/YEDOMON/Documents/fvst.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(fvst, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("dodgerblue", "coral4"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("dodgerblue", "coral4")
)

```


Venn diagram (continued)
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between 6leaf and root leaf samples
  
  ```{r}
svr = read.csv("C:/Users/YEDOMON/Documents/svr.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svr, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("goldenrod1", "seagreen3"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("goldenrod1", "seagreen3")
)
```



### Number of overlaped genes between 6leaf and shoot samples

```{r}
svs = read.csv("C:/Users/YEDOMON/Documents/svs.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svs, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("goldenrod1", "darkgray"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("goldenrod1", "darkgray")
)


```

Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between 6leaf and stem samples
  
  ```{r}
svste = read.csv("C:/Users/YEDOMON/Documents/svste.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svste, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("goldenrod1", "coral4"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("goldenrod1", "coral4")
)
```


### Number of overlaped genes between shoot and root samples

```{r}
svro = read.csv("C:/Users/YEDOMON/Documents/svro.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svro, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("darkgray", "seagreen"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("darkgray", "seagreen")
)

```

Venn diagram (end)
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between shoot and stem leaf samples
  
  ```{r}
svstem = read.csv("C:/Users/YEDOMON/Documents/svstem.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svstem, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("darkgray", "coral4"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("darkgray", "coral4")
)
```



### Number of overlaped genes between stem and root samples

```{r}
svroot = read.csv("C:/Users/YEDOMON/Documents/svroot.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(svroot, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("coral4", "seagreen"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("coral4", "seagreen")
)


```

Row
-----------------------------------------------------------------------
  
  ### Number of overlaped genes between all samples
  
  ```{r}
allsamples = read.csv("C:/Users/YEDOMON/Documents/allsamples.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(allsamples, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(venn)
venn(list, ilab=FALSE, 
     ellipse=FALSE,
     zcolor = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
     opacity = 0.8,
     cexil = 1, 
     cexsn = 0.8, 
     lty = 1,
     lwd = 3,
     col= c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3")
)
```

### Number of overlaped genes between all samples with details of intersection size


```{r}
allsamples = read.csv("C:/Users/YEDOMON/Documents/allsamples.csv", h=T, sep = ",")
library(BBmisc)
list = convertColsToList(allsamples, name.list = FALSE, name.vector = FALSE, factors.as.char = TRUE, as.vector = TRUE)
library(UpSetR)
upset(fromList(list), order.by = "freq", matrix.color = "dodgerblue", main.bar.color = "dodgerblue", sets.bar.color = "dodgerblue", shade.color = "dodgerblue", mainbar.y.label = "Number of shared genes", sets.x.label = "Number of genes per sample")
```


Hierarchical Clustering Heatmap of DEGs analysis
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Fith Leaf vs Root
  
  ```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/fivsro.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))

```



### Fith Leaf vs Sixth Leaf

```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/fivssi.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))
```

Row
-----------------------------------------------------------------------
  
  ### Fifth Leaf vs Stem
  
  ```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/fivsst.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))
```

### Sixth Leaf vs Root


```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/sivsro.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))

```


Hierarchical Clustering Heatmap of DEGs analysis (end)
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Sixth Leaf vs Stem
  
  ```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/sivsst.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))

```



### Shoot vs Root

```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/shvsro.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))
```

Row
-----------------------------------------------------------------------
  
  ### Shoot vs Stem
  
  ```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/shvsst.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))
```

### Stem vs Root


```{r}
library(pheatmap)
data = read.csv("C:/Users/YEDOMON/Documents/stvsro.csv", header=TRUE, sep=",", row.names=1)
dat = data.matrix(data)
pheatmap(dat, scale="column", clustering_distance_rows="euclidean", 
         clustering_distance_cols="euclidean",
         cellwidth = 15, 
         cellheight = 10,
         color = colorRampPalette(rev(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")))(100))

```


Volcano Plot of DEGs analysis
=======================================================================
  
  Row
-----------------------------------------------------------------------
  
  ### Sixth Leaf vs 
  
  ```{r}
library(ggplot2)
library(ggrepel)
v56 = read.csv("C:/Users/YEDOMON/Documents/v565656.csv", h=T, sep = ",")
p56 = ggplot(data = v56, 
             mapping = aes(x=log2fc, y=-log(p.value,10))) +
  geom_point(aes(color=Genes))+
  geom_vline(xintercept = 1,col = "black",linetype = "dashed",size = 0.5)+
  geom_vline(xintercept = 0,col = "black",linetype = "solid",size = 0.5)+
  geom_vline(xintercept = -1,col = "black",linetype = "dashed",size = 0.5)+
  theme(legend.position="top")+
  scale_color_manual(values=c("#F08080","#999999", "#56B4E9"))+
  theme_classic()
ggplotly(p56)     
```

### Stem vs Root







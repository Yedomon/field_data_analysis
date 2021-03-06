# iNSTALL DESeq2

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")


# Load libraries

library(ggplot2)
library(DESeq2)

# Load count and meta data

countData <- read.csv('countData.csv', header = TRUE, sep = ",")
metaData <- read.csv('metaData.csv', header = TRUE, sep = ",")


# Construct DESEQDataSet Object


dds <- DESeqDataSetFromMatrix(countData=countData, 
                              colData=metaData, 
                              design=~organ, tidy = TRUE)

dds

# Now we’re ready to run DESEQ function

dds <- DESeq(dds)


# Take a look at the results table

res <- results(dds)

head(results(dds, tidy=TRUE)) #let's look at the results table



# Summary of differential gene expression


summary(res) #summary of results



# Sort summary list by p-value


res <- res[order(res$padj),]

head(res)



# Get differential expression results
res <- results(dds)
table(res$padj<0.05)

res

#We observe 421 differentially expressed genes with adjusted p value <= 0.05 |FALSE|TRUE| |—|—| |421|54|



## Order by adjusted p-value
res <- res[order(res$padj), ]
## Merge with normalized count data
resdata <- merge(as.data.frame(res), as.data.frame(counts(dds, normalized=TRUE)), by="row.names", sort=FALSE)
names(resdata)[1] <- "Gene"
head(resdata)
## Write results
write.csv(resdata, file="diffexpr-results.csv",quote = FALSE,row.names = F)


yed = as.data.frame(res)

yed

write.csv(yed, file="orderbypvalue-results.csv",quote = FALSE,row.names = F)

# 最后设定阈值，筛选差异基因，导出数据(全部数据。包括标准化后的count数) 
#select differential genes with a logfold cuttoff
#  tuto https://pzweuj.github.io/2018/07/12/rna-seq-3.html


res <- res[order(res$padj),]
diff_gene <- subset(res, padj < 0.05 & (log2FoldChange > 1 | log2FoldChange < -1))
diff_gene <- row.names(diff_gene)
resdata <- merge(as.data.frame(res), as.data.frame(counts(dds, normalized=TRUE)), by="row.names", sort=FALSE)
write.csv(resdata,file = "male_female_chrX_diff.csv",row.names = FALSE)


# oPTION1
# PCA

#First we need to transform the raw count data
#vst function will perform variance stabilizing transformation

vsdata <- vst(dds, blind=FALSE)
plotPCA(vsdata, intgroup="organ") #using the DESEQ2 plotPCA fxn we can


# option2 better

# # Regularized log transformation for clustering/heatmaps, etc
rld <- rlogTransformation(dds)
head(assay(rld))
hist(assay(rld))

# Principal Components Analysis
plotPCA(rld,intgroup="organ")



# Plot Dispersions:

png("qc-dispersions.png", 1000, 1000, pointsize=20)
plotDispEsts(dds, main="Dispersion plot")
dev.off()




# Colors for plots below
## Ugly:
## (mycols <- 1:length(unique(condition)))
## Use RColorBrewer, better
library(RColorBrewer)





# Sample distance heatmap
condition <- factor(c(rep("Bud",3),rep("Leaf",3), rep("Seed",3)))
condition=relevel(condition,ref = "Bud")
condition
(mycols <- brewer.pal(8, "Dark2")[1:length(unique(condition))])

sampleDists <- as.matrix(dist(t(assay(rld))))
library(gplots)
png("qc-heatmap-samples2.png", w=1000, h=1000, pointsize=20)
heatmap.2(as.matrix(sampleDists), key=F, trace="none",
          col=colorpanel(100, "black", "white"),
          ColSideColors=mycols[condition], 
          RowSideColors=mycols[condition],
          margin=c(10, 10), main="Sample Distance Matrix")
dev.off()

#heatmap clustering

#Here, for demonstration, let us select the 150 genes with the
#highest variance across samples

library( "genefilter")
topVarGenes <- head( order( rowVars( assay(rld) ), decreasing=TRUE ),100)
heatmap.2( assay(rld)[ topVarGenes, ], scale="row",
           trace="none", dendrogram="column",
           col = colorRampPalette( rev(brewer.pal(9, "RdBu")) )(255))


# or save in png format

png("heatmap-samples150.png", w=1000, h=1000, pointsize=20)
heatmap.2( assay(rld)[ topVarGenes, ], scale="row",
           trace="none", dendrogram="column",
           col = colorRampPalette( rev(brewer.pal(9, "RdBu")) )(255))
dev.off()


# or save in pdf format
pdf("heatmap-samples150.pdf", w=1000, h=1000, pointsize=20)
heatmap.2( assay(rld)[ topVarGenes, ], scale="row",
           trace="none", dendrogram="column",
           col = colorRampPalette( rev(brewer.pal(9, "RdBu")) )(255))
dev.off()


# tuto link 1 https://physiology.med.cornell.edu/faculty/mason/lab/clinicalgenomics/lectures/2015/Lecture20_Sboner.pdf
# tuto link 2 https://lashlock.github.io/compbio/R_presentation.html
# tuto link 3 https://bioc.ism.ac.jp/packages/2.14/bioc/vignettes/DESeq2/inst/doc/beginner.pdf
# tuto link 4 http://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html
# tuto link 5 http://www.cbs.dtu.dk/courses/27626/Exercises/rnaseq.php outlier detection



# try with pheatmap

colData(dds)

# this gives log2(n + 1)
ntd <- normTransform(dds)

head(ntd)


?normTransform


#library("vsn")
#meanSdPlot(assay(ntd))
# just top 100


library("pheatmap")
select <- order(rowMeans(counts(dds,normalized=TRUE)),
                decreasing=TRUE)[1:100]
df <- as.data.frame(colData(dds)[,c("organ","id")])
pheatmap(assay(ntd)[select,], 
         cluster_rows=FALSE, 
         show_rownames=TRUE,
         cluster_cols=FALSE, 
         #annotation_col=df, 
         angle_col = "0",
         color=rev(RColorBrewer::brewer.pal(10,"PuOr"))
         )







# top 150

library("pheatmap")
select <- order(rowMeans(counts(dds,normalized=TRUE)),
                decreasing=TRUE)[1:150]
df <- as.data.frame(colData(dds)[,c("organ","id")])
pheatmap(assay(ntd)[select,], cluster_rows=FALSE, show_rownames=FALSE,
         cluster_cols=FALSE, annotation_col=df)

###ggplot style


deseq2Data <- DESeq(dds)

resdeseq = results(deseq2Data)

resdeseq

# Transform count data using the variance stablilizing transform
deseq2VST <- vst(deseq2Data)

# Convert the DESeq transformed object to a data frame
deseq2VST <- assay(deseq2VST)
deseq2VST <- as.data.frame(deseq2VST)
deseq2VST$Gene <- rownames(deseq2VST)
head(deseq2VST)



# Coerce to a data frame
deseq2ResDF <- as.data.frame(res)

# Examine this data frame
head(deseq2ResDF)

# Set a boolean column for significance
deseq2ResDF$significant <- ifelse(deseq2ResDF$padj < .1, "Significant", NA)



# Plot the results similar to DEseq2
ggplot(deseq2ResDF, aes(baseMean, log2FoldChange, colour=significant)) + geom_point(size=1) + scale_y_continuous(limits=c(-3, 3)) + scale_x_log10() + geom_hline(yintercept = 0, colour="tomato1", size=2) + labs(x="mean of normalized counts", y="log fold change") + scale_colour_manual(name="q-value", values=("Significant"="red"), na.value="grey50") + theme_bw()

# Let's add some more detail
ggplot(deseq2ResDF, aes(baseMean, log2FoldChange, colour=padj)) + geom_point(size=1) + scale_y_continuous(limits=c(-3, 3)) + scale_x_log10() + geom_hline(yintercept = 0, colour="darkorchid4", size=1, linetype="longdash") + labs(x="mean of normalized counts", y="log fold change") + scale_colour_viridis(direction=-1, trans='sqrt') + theme_bw() + geom_density_2d(colour="black", size=2)



# Keep only the significantly differentiated genes where the fold-change was at least 3
sigGenes <- rownames(deseq2ResDF[deseq2ResDF$padj <= .05 & abs(deseq2ResDF$log2FoldChange) > 3,])
deseq2VST <- deseq2VST[deseq2VST$Gene %in% sigGenes,]

# Convert the VST counts to long format for ggplot2
library(reshape2)
library(viridis)

# First compare wide vs long version
deseq2VST_wide <- deseq2VST
deseq2VST_long <- melt(deseq2VST, id.vars=c("Gene"))

head(deseq2VST_wide)
head(deseq2VST_long)

# Now overwrite our original data frame with the long format
deseq2VST <- melt(deseq2VST, id.vars=c("Gene"))

# Make a heatmap
heatmap <- ggplot(deseq2VST, 
                  aes(x=variable, y=Gene, fill=value)) + 
  geom_raster() + 
  scale_fill_viridis(trans="sqrt") + 
  theme(axis.text.x=element_text(angle=, hjust=1), legend.position = "none", axis.text.y=element_blank(), axis.ticks.y=element_blank())

heatmap

#--Library loading

library(RIdeogram)

#---Data importation

#pc_karyo = read.csv("pc_karyo.csv", header = TRUE, sep = ",")
pc_karyo <- read.table("pc_karyo.txt", sep = "\t", header = T, stringsAsFactors = F)


#--Karyotype plotting

ideogram(karyotype = pc_karyo)
convertSVG("chromosome.svg", device = "png")

#--Genes density data importation
genes_density_pc <- read.table("pc_genes_density.txt", sep = "\t", header = T, stringsAsFactors = F)


#--Plotting gene desity of NBS-LLR genes
ideogram(karyotype = pc_karyo, overlaid = genes_density_pc)
convertSVG("chromosome.svg", device = "png")



#--Distribution data importation

nlr_genes <- read.table("distri_pc.txt", sep = "\t", header = T, stringsAsFactors = F)


#--Plotting the distribution graph

help(ideogram)


ideogram(karyotype = pc_karyo, label = nlr_genes, label_type = "marker")
convertSVG("chromosome.svg", device = "png")


#--Overlapped distribution and type



ideogram(karyotype = pc_karyo, overlaid = gene_density, label = nlr_genes, label_type = "marker", width = 200, Lx = 80, Ly = 25)
convertSVG("chromosome.svg", device = "png")






##--Script for correlation
##--By Yedomon Ange Bovys Zoclanclounon | twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Science | Department of Genomics | Department of Genomics |RDA | Republic of South Korea


### Libraries


library(ggstatsplot)

data_correlation= read.csv("ACP.csv", h=T, sep = ",")

ggcorrmat(data = data_correlation[2:17], type = "nonparametric", matrix.type = "lower")

##--Script for repeatability
##--By Yedomon Ange Bovys Zoclanclounon | twitter: @AngeBovys27
##--15.03.2021
##--National Institute of Agricultural Science | Department of Genomics |RDA | Republic of South Korea

### Load libraries

library(rptR)


### Load datasets

data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
data_nqr1 = read.csv("data_nqr1.csv", h=T)
data_capsule_length_width = read.csv("capsule_length_and_width.csv", h=T)


names(data_qr)

### Repeatability calculation

rptPoisson(Plant_height ~ Accession, 
                 grname="Accession",
                 data = data_qr, 
                 nboot=0, 
                 npermut=0)
           

# Data analysis 
# Correlation
# Yedomon Ange Bovys Zoclanclounon

# Date 25th October 2021



#Set working directory

setwd("C:/Users/ange_/Downloads/lala_analysis/correlation/gifford/correlation")

# Data loading

data_corr = read.csv("cor_gifford.csv", h = T, sep = ";", dec = ",")


# Correlation with corrplot

library(corrplot)
corrplot.mixed(cor(data_corr))


# correlation test + correlation with Hmisc

library(Hmisc)
rcorr(as.matrix(data_corr))


# Another way with ggcorrplot package

library(ggcorrplot)
ggcorrmat(data = data_corr, type = "parametric", matrix.type = "lower")

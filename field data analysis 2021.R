# Analysis of sesame phenotyping experiment data by Yedomon 


## Package loading

library(ggstatsplot) # To do boxplot
library(patchwork) # To plot multiple graph in one set
library(descriptr) # Descriptive analysis
library(dplyr) # manipulate dataset
library(lme4) # Mix model running
library(lmerTest) # mix model running
library(ggplot2) # make graphs



## Data importation
data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
data_nqr1 = read.csv("data_nqr1.csv", h=T)
data_nqr_2_1 = read.csv("data_nqr_2_1.csv", h=T)
data_capsule_length_width = read.csv("capsule_length_and_width.csv", h=T)
names(data_capsule_length_width)





## Box plot representation of variables per continent



## Pie graph style

### Number of capsule per leaf axil

G = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_2_1,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Capsule_type,
  y = Continent
) +
  guides(fill = guide_legend(title = "Number of capsules per leaf axil"))+
  theme_bw()+
  ggtitle("(S)")

### Data 
data_nqr_b = read.csv("branching_patterns.csv", h=T)
data_nqr_c = read.csv("capsule_hairiness.csv", h=T)
data_nqr_d = read.csv("flower color.csv", h=T)
data_nqr_e = read.csv("number_carpels_per_capsule.csv", h=T)
data_nqr_f = read.csv("seed_color.csv", h=T)
data_nqr_g = read.csv("inflorescence_type.csv", h=T)
names(data_nqr_g)

### Branching paterns

A = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_b,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Branching_Patterns,
  y = Continent
) +
  guides(fill = guide_legend(title = "Branching patterns"))+
  theme_bw()+
  ggtitle("(M)")
 
A

### Capsule_Hairiness

B = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_c,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Capsule_Hairiness,
  y = Continent
) +
  guides(fill = guide_legend(title = "Capsule hairiness"))+
  theme_bw()+
  ggtitle("(N)")



### Flower_Color


C = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_d,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Flower_Color,
  y = Continent
) +
  guides(fill = guide_legend(title = "Flower color"))+
  theme_bw()+
  ggtitle("(O)")


### Number_Carpels_Capsule

D = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_e,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Number_Carpels_Capsule,
  y = Continent
) +
  guides(fill = guide_legend(title = "Number of carpels per capsule"))+
  theme_bw()+
  ggtitle("(P)")


### Seed_Color

E = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_f,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Seed_Color,
  y = Continent,
  package = "RColorBrewer", palette = "Paired", direction = 1) +
  guides(fill = guide_legend(title = "Seed color"))+
  theme_bw()+
  ggtitle("(Q)")
  

### Inflorescence_Type


F = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_g,
    #Continent %in% c("Africa", "Asia","Others")
    Continent %in% c("Africa", "Asia")
  ),
  x = Inflorescence_Type,
  y = Continent,
  package = "RColorBrewer", palette = "Paired", direction = 1) +
  guides(fill = guide_legend(title = "Growth habit"))+
  theme_bw()+
  ggtitle("(R)")


### Render 4 graphs together

A + B + C + D

### Render 3 graphs together

(E) /
  (F | G) 


## ANOVA

### Load libraries

library(lmerTest)
#library(nlme)
#library(lme4)


# Data

data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)

names(data_qr)


### Plant height

model_plant_height <- lmer(formula = Plant_height ~ Accession * Site + (1 | Block:Site), data = na.omit(data_qr))

summary(model_plant_height)

anova(model_plant_height)



### Capsule number

model_Capsule_Number <- lmer(formula = Capsule_Number ~ Accession * Site + (1 | Block:Site), data = na.omit(data_qr))

anova(model_Capsule_Number)



### Branch_number

model_Branch_number <- lmer(formula = Branch_number ~ Accession * Site + (1 | Block:Site), data = na.omit(data_qr))

anova(model_Branch_number)




### Diameter

model_Diameter <- lmer(formula = Diameter ~ Accession * Site + (1 | Block:Site), data = na.omit(data_qr))

anova(model_Diameter)




### Productive_axis_length

model_Productive_axis_length <- lmer(formula = Productive_axis_length ~ Accession * Site + (1 | Block:Site), data = na.omit(data_qr))

anova(model_Productive_axis_length)



### Dried_Biomass


model_Dried_Biomass <- lmer(formula = Dried_Biomass ~ Accession * Site + (1 | Block:Site), data = na.omit(data_nqr1))

anova(model_Dried_Biomass)








model_Dried_Biomass <- aov(Dried_Biomass ~ Block + Accession * Site , data = data_nqr1)

model_Dried_Biomass <- aov(Dried_Biomass ~ Block, data = data_nqr1)

model_Dried_Biomass <- aov(Dried_Biomass ~ Accession, data = data_nqr1)

model_Dried_Biomass <- aov(Dried_Biomass ~ Site, data = data_nqr1)

model_Dried_Biomass <- aov(Dried_Biomass ~ Accession:Site, data = data_nqr1)

summary(model_Dried_Biomass)

anova(model_Dried_Biomass)


model_Dried_Biomass <- lmer(formula = Dried_Biomass ~ Accession * Site + (Block | Site), data = data_nqr1)

summary(model_Dried_Biomass)

ranova(model_Dried_Biomass, reduce.terms = FALSE)








### Harvest_Index


model_Harvest_Index <- aov(Harvest_Index ~ Block + Accession * Site , data = data_nqr1)

model_Harvest_Index <- aov(Harvest_Index ~ Block, data = data_nqr1)

model_Harvest_Index <- aov(Harvest_Index ~ Accession, data = data_nqr1)

model_Harvest_Index <- aov(Harvest_Index ~ Site, data = data_nqr1)

model_Harvest_Index <- aov(Harvest_Index ~ Accession:Site, data = data_nqr1)

summary(model_Harvest_Index)



### Thousand_Seed_Weight


model_Thousand_Seed_Weight <- aov(Thousand_Seed_Weight ~ Block, data = data_nqr1)

model_Thousand_Seed_Weight <- aov(Thousand_Seed_Weight ~ Accession, data = data_nqr1)

model_Thousand_Seed_Weight <- aov(Thousand_Seed_Weight ~ Site, data = data_nqr1)

model_Thousand_Seed_Weight <- aov(Thousand_Seed_Weight ~ Accession:Site, data = data_nqr1)

summary(model_Thousand_Seed_Weight)


### Flowering

model_Flowering <- aov(Flowering ~ Block, data = data_nqr1)

model_Flowering <- aov(Flowering ~ Accession, data = data_nqr1)

model_Flowering <- aov(Flowering ~ Site, data = data_nqr1)

model_Flowering <- aov(Flowering ~ Accession:Site, data = data_nqr1)

summary(model_Flowering)


### Maturity

model_Maturity <- aov(Maturity ~ Block, data = data_nqr1)

model_Maturity <- aov(Maturity ~ Accession, data = data_nqr1)

model_Maturity <- aov(Maturity ~ Site, data = data_nqr1)

model_Maturity <- aov(Maturity ~ Accession:Site, data = data_nqr1)

summary(model_Maturity)



### Flowering_to_Maturity


model_Flowering_to_Maturity <- aov(Flowering_to_Maturity ~ Block, data = data_nqr1)

model_Flowering_to_Maturity <- aov(Flowering_to_Maturity ~ Accession, data = data_nqr1)

model_Flowering_to_Maturity <- aov(Flowering_to_Maturity ~ Site, data = data_nqr1)

model_Flowering_to_Maturity <- aov(Flowering_to_Maturity ~ Accession:Site, data = data_nqr1)

summary(model_Flowering_to_Maturity)


### Dried seed weight

model_Dried_Seed_Weight <- aov(Dried_Seed_Weight ~ Block, data = data_nqr1)

model_Dried_Seed_Weight <- aov(Dried_Seed_Weight ~ Accession, data = data_nqr1)

model_Dried_Seed_Weight <- aov(Dried_Seed_Weight ~ Site, data = data_nqr1)

model_Dried_Seed_Weight <- aov(Dried_Seed_Weight ~ Accession:Site, data = data_nqr1)

summary(model_Dried_Seed_Weight)


### Capsule width

model_Capsule_width <- lmer(formula = Capsule_width ~ Block + Accession + (1 | Block:Accession), data = data_capsule_length_width)

anova(model_Capsule_width)


### Capsule length

model_Capsule_length <- lmer(formula = Capsule_length ~ Block + Accession + (1 | Block:Accession), data = data_capsule_length_width)

anova(model_Capsule_length)








names(data_nqr1)


library(rptR)

# distribution

library(pastecs)

View(data_nqr1)

res.norm.dbi = stat.desc(data_nqr1[4:10], norm = TRUE, basic=FALSE, desc=FALSE)

res.norm.dbi

library(dlookr)
normality(data_nqr1[4:10])
plot_normality(data_nqr1[4:10])

names(data_nqr1)


# imputation NA


### Dried_Biomass

Dried_Biomass_mean <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "mean")
n1 = plot(Dried_Biomass_mean)






Dried_Biomass_median <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "median")
n2 = plot(Dried_Biomass_median)

Dried_Biomass_mode <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "mode")
n3 = plot(Dried_Biomass_mode)

Dried_Biomass_knn <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "knn")
n4 = plot(Dried_Biomass_knn)

Dried_Biomass_rpart <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "rpart")
n5 = plot(Dried_Biomass_rpart)

Dried_Biomass_mice <- imputate_na(data_nqr1, xvar = Dried_Biomass, yvar = Dried_Seed_Weight, method = "mice", seed = 999)
n6 = plot(Dried_Biomass_mice)

(n1) /
  (n2) /
  (n3) /
  (n4) /
  (n5) /
  (n6)

write.csv(Dried_Biomass_mice, "mice_Dried_Biomass")

### Data cleaning and impuation

setwd("C:/Users/user/Documents")


#### Packages

library(tidyverse)
library(flextable)
library(dlookr)
library(DMwR) # knn imputation

data_correlation= read.csv("correlation.csv", h=T, sep = ",")




#### General 
diagnose(data_correlation)

#### Numeric
diagnose_numeric(data_correlation)

#### Outlier
diagnose_outlier(data_correlation)

#### Missing values

plot_na_pareto(data_correlation)


data_correlation %>% 
  plot_na_pareto(only_na = TRUE)


names(data_correlation)

## Imputation

### TSW

tsw_mean <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "mean")
a1 = plot(tsw_mean)


tsw_knn <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "knn")
a2 = plot(tsw_knn)

tsw_median <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "median")
a3 = plot(tsw_median)

tsw_mode <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "mode")
a4 = plot(tsw_mode)


tsw_rpart <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "rpart")
a5 = plot(tsw_rpart)


tsw_mice <- imputate_na(data_correlation, xvar = TSW, yvar = DSW, method = "mice", seed = 999)
a6 = plot(tsw_mice)


(a1) /
(a2) /
(a3) /
(a4) /
(a5) /
(a6)



write.csv(tsw_mice, "mice_tsw")


### HIN

names(data_correlation)

hi_mean <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "mean")
b1 = plot(hi_mean)


hi_knn <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "knn")
b2 = plot(hi_knn)

hi_median <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "median")
b3 = plot(hi_median)

hi_mode <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "mode")
b4 = plot(hi_mode)


hi_rpart <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "rpart")
b5 = plot(hi_rpart)


hi_mice <- imputate_na(data_correlation, xvar = HIN, yvar = DSW, method = "mice", seed = 999)
b6 = plot(hi_mice)


(b1) /
(b2) /
(b3) /
(b4) /
(b5) /
(b6)

write.csv(hi_mice, "mice_hi")



### CLE

names(data_correlation)

CLE_mean <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "mean")
c1 = plot(CLE_mean)


CLE_knn <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "knn")
c2 = plot(CLE_knn)

CLE_median <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "median")
c3 = plot(CLE_median)

CLE_mode <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "mode")
c4 = plot(CLE_mode)


CLE_rpart <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "rpart")
c5 = plot(CLE_rpart)


CLE_mice <- imputate_na(data_correlation, xvar = CLE, yvar = CWI, method = "mice", seed = 999)
c6 = plot(CLE_mice)


(c1) /
  (c2) /
  (c3) /
  (c4) /
  (c5) /
  (c6)




write.csv(CLE_rpart, "rpart_CLE")


### CWI



names(data_correlation)

CWI_mean <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "mean")
d1 = plot(CWI_mean)


CWI_knn <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "knn")
d2 = plot(CWI_knn)

CWI_median <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "median")
d3 = plot(CWI_median)

CWI_mode <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "mode")
d4 = plot(CWI_mode)


CWI_rpart <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "rpart")
d5 = plot(CWI_rpart)


CWI_mice <- imputate_na(data_correlation, xvar = CWI, yvar = CLE, method = "mice", seed = 999)
d6 = plot(CWI_mice)


(d1) /
  (d2) /
  (d3) /
  (d4) /
  (d5) /
  (d6)




write.csv(CWI_mice, "mice_CWI")



### DSW


DSW_mean <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "mean")
e1 = plot(DSW_mean)

DSW_median <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "median")
e2 = plot(DSW_median)

DSW_mode <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "mode")
e3 = plot(DSW_mode)

DSW_knn <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "knn")
e4 = plot(DSW_knn)

DSW_rpart <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "rpart")
e5 = plot(DSW_rpart)


DSW_mice <- imputate_na(data_correlation, xvar = DSW, yvar = TSW, method = "mice", seed = 999)
e6 = plot(DSW_mice)


(e1) /
  (e2) /
  (e3) /
  (e4) /
  (e5) /
  (e6)

write.csv(DSW_rpart, "rpart_DSW")



### DBI


DBI_mean <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "mean")
f1 = plot(DBI_mean)

DBI_median <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "median")
f2 = plot(DBI_median)

DBI_mode <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "mode")
f3 = plot(DBI_mode)

DBI_knn <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "knn")
f4 = plot(DBI_knn)

DBI_rpart <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "rpart")
f5 = plot(DBI_rpart)


DBI_mice <- imputate_na(data_correlation, xvar = DBI, yvar = HIN, method = "mice", seed = 999)
f6 = plot(DBI_mice)


(f1) /
  (f2) /
  (f3) /
  (f4) /
  (f5) /
  (f6)

write.csv(DBI_mice, "mice_DBI")



### CNU
names(data_correlation)

CNU_mean <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "mean")
g1 = plot(CNU_mean)

CNU_median <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "median")
g2 = plot(CNU_median)

CNU_mode <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "mode")
g3 = plot(CNU_mode)

CNU_knn <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "knn")
g4 = plot(CNU_knn)

CNU_rpart <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "rpart")
g5 = plot(CNU_rpart)


CNU_mice <- imputate_na(data_correlation, xvar = CNU, yvar = BNU, method = "mice", seed = 999)
g6 = plot(CNU_mice)


(g1) /
  (g2) /
  (g3) /
  (g4) /
  (g5) /
  (g6)

write.csv(CNU_rpart, "rpart_DBI")


# PAL

names(data_correlation)

PAL_mean <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "mean")
h1 = plot(PAL_mean)

PAL_median <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "median")
h2 = plot(PAL_median)

PAL_mode <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "mode")
h3 = plot(PAL_mode)

PAL_knn <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "knn")
h4 = plot(PAL_knn)

PAL_rpart <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "rpart")
h5 = plot(PAL_rpart)


PAL_mice <- imputate_na(data_correlation, xvar = PAL, yvar = PHE, method = "mice", seed = 999)
h6 = plot(PAL_mice)


(h1) /
  (h2) /
  (h3) /
  (h4) /
  (h5) /
  (h6)

write.csv(PAL_mice, "mice_PAL")


### BNU

names(data_correlation)

BNU_mean <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "mean")
i1 = plot(BNU_mean)

BNU_median <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "median")
i2 = plot(BNU_median)

BNU_mode <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "mode")
i3 = plot(BNU_mode)

BNU_knn <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "knn")
i4 = plot(BNU_knn)

BNU_rpart <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "rpart")
i5 = plot(BNU_rpart)


BNU_mice <- imputate_na(data_correlation, xvar = BNU, yvar = CNU, method = "mice", seed = 999)
i6 = plot(BNU_mice)


(i1) /
  (i2) /
  (i3) /
  (i4) /
  (i5) /
  (i6)

write.csv(BNU_mice, "mice_BNU")


### DIA

DIA_mean <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "mean")
j1 = plot(DIA_mean)

DIA_median <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "median")
j2 = plot(DIA_median)

DIA_mode <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "mode")
j3 = plot(DIA_mode)

DIA_knn <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "knn")
j4 = plot(DIA_knn)

DIA_rpart <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "rpart")
j5 = plot(DIA_rpart)


DIA_mice <- imputate_na(data_correlation, xvar = DIA, yvar = PAL, method = "mice", seed = 999)
j6 = plot(DIA_mice)


(j1) /
  (j2) /
  (j3) /
  (j4) /
  (j5) /
  (j6)

write.csv(DIA_mice, "mice_DIA")

### FLO


FLO_mean <- imputate_na(data_correlation, xvar = FLO, yvar = MAT, method = "mean")
k1 = plot(FLO_mean)

FLO_median <- imputate_na(data_correlation, xvar = FLO, yvar = MAT, method = "median")
k2 = plot(FLO_median)

FLO_mode <- imputate_na(data_correlation, xvar = FLO, yvar = MAT, method = "mode")
k3 = plot(FLO_mode)

FLO_knn <- imputate_na(data_correlation, xvar = FLO, yvar = MAT, method = "knn")
k4 = plot(FLO_knn)

FLO_rpart <- imputate_na(data_correlation, xvar = FLO, yvar = MAT, method = "rpart")
k5 = plot(FLO_rpart)


FLO_mice <- imputate_na(data_correlation, xvar = FLO, yvar = PAL, method = "mice", seed = 999)
k6 = plot(FLO_mice)

(k1) /
  (k2) /
  (k3) /
  (k4) /
  (k5) /
  (k6)

write.csv(FLO_knn, "knn_FLO")


### FTM


FTM_mean <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "mean")
l1 = plot(FTM_mean)

FTM_median <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "median")
l2 = plot(FTM_median)

FTM_mode <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "mode")
l3 = plot(FTM_mode)

FTM_knn <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "knn")
l4 = plot(FTM_knn)

FTM_rpart <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "rpart")
l5 = plot(FTM_rpart)


FTM_mice <- imputate_na(data_correlation, xvar = FTM, yvar = FLO, method = "mice", seed = 999)
l6 = plot(FTM_mice)

(l1) /
  (l2) /
  (l3) /
  (l4) /
  (l5) /
  (l6)

write.csv(FTM_rpart, "rpart_FTM")

### MAT


MAT_mean <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "mean")
m1 = plot(MAT_mean)

MAT_median <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "median")
m2 = plot(MAT_median)

MAT_mode <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "mode")
m3 = plot(MAT_mode)

MAT_knn <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "knn")
m4 = plot(MAT_knn)

MAT_rpart <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "rpart")
m5 = plot(MAT_rpart)


MAT_mice <- imputate_na(data_correlation, xvar = MAT, yvar = FLO, method = "mice", seed = 999)
m6 = plot(MAT_mice)

(m1) /
  (m2) /
  (m3) /
  (m4) /
  (m5) /
  (m6)

write.csv(MAT_rpart, "rpart_MAT")


### PHE

PHE_mean <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "mean")
n1 = plot(PHE_mean)

PHE_median <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "median")
n2 = plot(PHE_median)

PHE_mode <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "mode")
n3 = plot(PHE_mode)

PHE_knn <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "knn")
n4 = plot(PHE_knn)

PHE_rpart <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "rpart")
n5 = plot(PHE_rpart)


PHE_mice <- imputate_na(data_correlation, xvar = PHE, yvar = PAL, method = "mice", seed = 999)
n6 = plot(PHE_mice)

(n1) /
  (n2) /
  (n3) /
  (n4) /
  (n5) /
  (n6)

write.csv(PHE_mice, "mice_PHE")


### Correlation

data_correlation_imputed= read.csv("ACP.csv", h=T, sep = ",")

names(data_correlation_imputed)

View(data_correlation_imputed)

##Using GGally
library(GGally)
ggcorr(data_correlation_imputed[2:17], method = c("pairwise", "pearson"), label = TRUE, label_round = 2, label_size = 5)
ggcorr(data_correlation_imputed[2:17], method = c("all.obs", "spearman"), label = TRUE, label_round = 2, label_size = 5)


## Using performance analytics chart correlation
library(PerformanceAnalytics)
chart.Correlation(data_correlation_imputed[2:17], histogram=TRUE, pch="+", method = "pearson")
chart.Correlation(data_correlation_imputed[2:17], histogram=TRUE, pch="+", method = "spearman")




## Using ggstatsplot
library(ggstatsplot)

## Red blue gradient 

ggcorrmat(data = data_correlation_imputed[2:17], 
          type = "parametric", matrix.type = "lower", 
          colors = c("#3B9AB2", "#EEEEEE", "#F21A00")
          #colors = c("#E69F00", "white", "#009E73")
          )



## green yellow gradient
ggcorrmat(data = data_correlation_imputed[2:17], 
          type = "parametric", matrix.type = "lower"
          )
    

## Red blue gradient      
ggcorrmat(data = data_correlation_imputed[2:17], 
          type = "nonparametric", matrix.type = "lower",  
          colors = c("#3B9AB2", "#EEEEEE", "#F21A00")
          )

## green yellow gradient
ggcorrmat(data = data_correlation_imputed[2:17], 
          type = "nonparametric", matrix.type = "lower"  
          )



## Path coefficient analysis
library(agricolae)


# Dependant variables definition

x = data_correlation_imputed[, c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17)]


#  Independent variable definition

y = data_correlation_imputed[,8]


# Correlation y and x


cor.y = correlation(y,x)$correlation

cor.x = correlation(x)$correlation


# Path analysis


path.table = path.analysis(cor.x,cor.y)


# Save the results


write.csv(path.table, "pathcoefanalysisresults.csv")


### ACP


#### Library

library(FactoMineR)
library(factoextra)


#### Data

data_acp= read.csv("ACP.csv", h=T, sep = ",", row.names = 1)

View(data_acp)


##acp

res.pca = PCA(data_acp[1:16], scale = TRUE, graph=F)


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
                         habillage = grp_cluster, #, # color by groups)
                         palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()

# Description of clusters

res.hcpc


### DENFROGRAMME VIEW

clusterB = fviz_cluster(res.hcpc, habillage = grp_cluster,palette = c("#00AFBB", "#E7B800", "#FC4E07"), show.legend = FALSE, title = "(B)" ) +
  theme_bw()

clusterA
clusterB




#


# Remove species column (5) and scale the data
iris.scaled <- scale(iris[, -5])

# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(iris.scaled, 3, nstart = 2)

# Visualize kmeans clustering
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")+
  theme_minimal()

# Visualize silhouhette information
require("cluster")
sil <- silhouette(km.res$cluster, dist(iris.scaled))
fviz_silhouette(sil)
fviz_silhouette()



### Corehunter3 core colection


## Installation
install.packages("rJava")
install.packages("corehunter")

## Loading
library(rJava)
library(corehunter)


## Import the data
data_pheno = read.csv("corehunter_datapheno.csv", h = T, sep = ",")

## convert into corehunter class

phenot <- phenotypes(data_pheno)


## Infer the core collection

core <- sampleCore(phenot)

## Results

core


### core collection evaluation

data_pheno_core_collection = read.csv("core_collection_quanti.csv", h = T, sep = ",")
data_pheno_whole_collection = read.csv("whole_collection_quanti.csv", h = T, sep = ",")
View(data_pheno_core_collection)
View(data_pheno_whole_collection)


### Install pastecs package 

install.packages("pastecs")


### Load pastecs


library(pastecs)


### Stats for core collection

stat.desc(data_pheno_core_collection[2:17])
stat.desc(data_pheno_whole_collection[2:17])


### test difference between whole and core collection for each trait

### First let's check the data normality 


### Data importation

data_whole_core = read.csv("data_anova_t_test_wilcoxon_others.csv", h = T, sep = ",")

View(data_whole_core)
### Check the normality test quickly with the pastecs package

library(pastecs)

res.norm = stat.desc(data_whole_core[1:16], norm = TRUE, basic=FALSE, desc=FALSE)

write.csv(res.norm, "res.normality.csv")


### Try with dlookr

library(dlookr)


normality(data_whole_core[1:16])

## We've got the same results. Now let's plot the distribution

plot_normality(data_whole_core[1:16])


## Only PAL are normally distributed. So, let's run the Student's paired t-test.


library(dplyr) # To select what I want

attach(data_whole_core)
PAL_whole = data_whole_core %>%
            select(PAL) %>%
            filter(Type == "Whole")


PAL_core = data_whole_core %>%
           select(PAL) %>%
           filter(Type == "Core")

### Run the t test
t.test(PAL_whole, PAL_core)


## Let' run the Wilcoxon signed-rank test for none normally distributed data

## CNU

CNU_whole = data_whole_core %>%
  select(CNU) %>%
  filter(Type == "Whole")
CNU_whole

CNU_core = data_whole_core %>%
  select(CNU) %>%
  filter(Type == "Core")

View(CNU_whole)


wilcox.test(CNU_core,CNU_whole)

# The dplyr method not work for wilcoxon so I CHANGE THE STRATEGY BY DOING:

CNU_whole = c(103.1818182,91.46666667,57,121.974359,135.3157895,147.4444444,7.4,132.7,115.2,131.4,165.2,127.1,9.5,152.3,165.1818182,148.2,93.66666667,191,93.77777778,109,110.375,121.7,118,141.7777778,120.2,77.6,173.3,104.4444444,163.7,137.1,141.4,123.125,173.25,145.2,119.4,156,119.6,123.3,159,152.9,70.9,100.3333333,61,117.2,92.77777778,222,101.6666667,94.4,110.7777778,151.8888889,121.9,101.5,123.6666667,140.5,88.77777778,101.2222222,121.3,110.125,126.5,112,82.44444444,78.88888889,94.125,71.9,90.5,78.7,71.55555556,81.35714286,101.7,114,135.9,96.875,98.1,59.4,121.9,88.44444444,32,84,190.9,137.125,199.1,123.5555556,33.4,93,84.22222222,122.4444444,114.5,70.3,122,103.5555556,144.6,127.6666667,119.6,155.1,145.2222222,127.4285714,83.2,136.6666667,148,111.8888889,159.625,105.875,92.11111111,80.3,166.4,55.4,103.8,78.11111111,82,102.625,31.7,58.2,113.875,113.1,100.125,156,116.9,118.9,186.5,153.5,118.25,62.2,29,78.1,125.3,164.4285714,24.16666667,71.375,90,69.75,121.3333333,111,188.8,133.3333333,94.28571429,159.125,91,58.14285714,127.6,105.8333333,92.75,234,119.6666667,130,51.57142857,74.375,121.7777778,111.4,30.375,111,115.8888889,115.3333333,89.88888889,66.875,60.71428571,105.9,64.6,57.6,78.66666667,173.2,138.75,74.6,55.375,76.5,62.8,116.6,107.2,57.5,87.9,92.5,108.875,72.7,112,62.25,149.1,77.66666667,42.22222222,25.375,33,64.44444444,150.4,115.6,138.2,125.2222222,164.5555556,128.6666667,94.875,80.75,5.1,11.5,38.1,2.3,14.3,22,10.66666667,24.77777778,45.33333333,31.4,78.44444444,40.125,78.75,2.625,9,28.875,27.16666667,76.5,48.28571429,81.25,19.25,110.125,101.5,47,118.9,127,117.125,101.875,88.625,131.3,135.4,100.9,112.6,68.375,77.625,136.7,104.4285714,110.7777778,31.42857143,107,59.7,69.625,78.25,87.33333333,132.2,89.88888889,55.11111111,100,113.2,114.625,65.9,24.375,35.33333333,119.4444444,181.1666667,154.625,180.8888889,139.9,152.5555556,149.7,100,184,152.7777778,174.8888889,112.4,114.7777778,165.2,147,146.7777778,150.375,181.3333333,123.4,141.2222222,127.8888889,144.7,167.75,93.2890873,110,136.8888889,155.4444444,130.4,122.2857143,124.4,143.6,92.8,138.4444444,115,106.1111111,111.4,104.7,175.4,140,138.8,266,74.75,58.66666667,122,129.3,143.5,116.2,138.8888889,161.5,79.3,100.75,136.4,89.5,177.4444444,97.5,167.4444444,224.7142857,157,125.7777778,126.4,120.8571429,107.2222222,101.2,138.3333333,70.9,167.6,104.3,97.11111111,123.4,141.8,85.33333333,85.66666667,118,119.3333333,154.4444444,73.5,141.1111111,86.55555556,106,126.8571429,227,213.6666667,116.7777778,82.11111111,70.5,27.375,144,47.33333333,113.3,167.6666667,149.625,142.7777778,149.3333333,106,106.8888889,77.5,179.1,92,164,81,105.3,146.7777778,128.5,124.5555556,184.6666667,104.25,129.6,78.55555556,137.3333333,206.2857143,107.7,114.7777778,110.6,145.6,155,129.4,138.5555556,194.3333333,308,115.125,138.6,223.6666667,168.6,188.6666667,171.3333333,165.125,312.1666667,213.8888889,143.5555556,119.1,181.7142857,154.8571429,221.7777778,143.8666667,284.375,131.25,109.8571429,61.75,130.625,88,106.5555556,85.125,141.1666667,104.375,52.7,103.1428571,121.3,143.6666667,87.5,218.5,157,146.7,163.2222222,158.8,116.9,158.6,114,98,116.75,128.75,93.5,85.7,106.5,134.5,112.25,102.6666667,144.3,149.9166667,122.7,147.2857143,136.7,118.1,101.3,85.5,88.78571429,71.2,108.1,87.88888889,89.5,113.7,65.8,60.9,72.44444444,142.6,141.4444444,198.25,126.375,125.3333333,96.7,68.46153846,81,143.7,110.7,38.6,113.6576908,72.7,95.6,397,159,57,30.5,130,129.0762963,106.3333333,129.0762963,76,152.6,119.2,87,129.6,80.4,83.6,83.2,91.6,95.4,73.4,63.4,111.4,188.4,120.2,97.6,115.2,76.5,101.8333333,92,93.66666667,76.4,107.8,104.8,81.8,74.8,98.4,109.4,95.4,101.6,96.2,101.6,93.2,81.4,93.2,130.2,120.4,105,95.8,102,105.8,150.6,128.8,98.2,111.8,100.2,125,118.6,131.6,101.4,123.2,99,141.6,86.8,89.4,91.4,128,114.6,92.6,137.2,111.8)
CNU_core = c(103.1818182,91.46666667,57,121.974359,132.7,127.1,148.2,119.4,117.2,101.6666667,94.125,81.35714286,121.9,199.1,123.5555556,114.5,155.1,83.2,31.7,100.125,29,164.4285714,127.6,234,130,111,115.8888889,89.88888889,66.875,60.71428571,62.8,149.1,2.3,78.44444444,28.875,76.5,19.25,117.125,101.875,135.4,100.9,77.625,136.7,78.25,87.33333333,113.2,24.375,152.5555556,141.2222222,136.8888889,74.75,58.66666667,107.2222222,118,154.4444444,126.8571429,213.6666667,70.5,167.6666667,179.1,128.5,78.55555556,155,129.4,138.5555556,138.6,188.6666667,143.5555556,143.8666667,146.7,163.2222222,128.75,93.5,85.7,106.5,134.5,112.25,144.3,149.9166667,122.7,101.3,108.1,87.88888889,113.7,65.8,60.9,72.44444444,142.6,72.7,130,119.2,83.2,111.4,120.2,76.4,107.8,96.2,105.8,125,131.6,141.6,86.8)
res.cnu <- wilcox.test(CNU_whole, CNU_core)
res.cnu

# work well . So I will do that for the rest 

# PHE case

PHE_whole = c(130.3636364,105.6444444,91.66666667,128.3846154,133.3684211,169.7777778,140.1,136.7,119.3,131.5,117.1,109,195.2,168.8,192.8181818,185.2,132.3333333,157.625,169.1111111,165.1111111,169.625,171.4,186.8571429,184.4444444,168.7142857,181.5,177.6,145.4444444,173,179.8,185,172.125,171.5833333,143.8,174.2,152,180.1,188.3,168.625,160,188.7,219.3333333,187.3,192.2,200.6666667,155.2857143,181.4444444,168.4,192.5555556,192,173.8,165.4,192.3333333,158.5,185.2222222,193,131.7,189.5,168.7,183.4444444,212.5555556,215.5555556,184,181.1,159.8,222,170.1111111,194.4285714,181.6,169.2,178.2,190.75,160.2,178.8,167.8,198.2222222,167.2,192.6666667,127.8,148.25,150.3,160.2222222,191.1,195.2222222,184.3333333,184.4444444,161.6666667,179.4,183.6666667,155.5555556,190.1,187.1111111,137.9,169.8,136.3333333,163.4285714,157.7,162.2222222,153.9,171.2222222,136.5,174,148.3333333,168.5,198.2,184.2,169.7,164,162.8,157.625,155.1,187.6,144.875,137.4,153.75,155.7777778,162.4,194,160.7,164.9,167.625,184,171.5,163.1,102.5,180.5714286,177,174.625,156.1666667,172.5,193.6666667,168.1428571,213.2,192.4444444,165.2857143,181.25,184.7777778,172.1428571,199,167.5,187.375,151.25,182.1666667,219.2,198.2857143,191.25,200.3333333,193.8,194.75,151.3333333,161.5555556,147.3333333,181.2222222,173,190.7142857,195.2,188.6,190.7,194.3333333,205,182.25,188.5,177.75,200.4,197,186.3,199.5,188.3,169.4,196.4,186.375,185.3,129.875,160.5,168.7,216.6666667,212.6666667,189.75,201.2,192.3333333,104.7,183.5,140.7,158.4444444,181.2222222,182.2222222,161.75,183,188.7,193.5,156.3,170.2,182.8,208.75,202.1111111,208.2222222,184,175.8,194.7777778,190,213.625,177,171.375,193.625,150.6666667,157.8333333,154.7142857,185.375,192.75,192.125,157.6666667,186.5,137.8,153.75,158.125,168.75,165.625,140.8,147,165.2,146.3,166.875,162.125,179.8,156,194.6666667,179.5714286,192.8,218.6,198.5,144.5,209.1111111,199.4,184,206.5555556,212.2222222,192.9,211.75,180.4,196.625,189.1666667,144.2222222,127.8333333,136.375,138.5555556,96.6,147.3333333,176.8,138.5555556,161.6666667,123.8888889,144.5555556,135,124.5555556,152.3,147.6,129.2222222,172.875,160.6666667,129.6,153.1111111,122.2222222,145.5,155.25,183.125,105.625,141.5555556,137.8888889,158.1,138.4285714,144,155.6,119,120.1111111,129.5,110,130.2,134.9,142.4,139.8,150.8,134.5,95.75,111.6666667,205.6666667,137.9,149.6,163.6,163,166.75,112.3,116.875,138.3,126.25,142.3333333,153.1,132.1111111,153.2857143,140.2,122.4444444,116.3,194.7142857,138.2222222,94.9,149.8888889,174.3,149.6,145.4,122.8888889,147.9,151.3,188.8888889,176.2222222,143.875,166.1111111,177.6666667,160.2,135.2222222,168.7777778,116.2,109.2857143,124.75,139.3333333,111.5777778,162.7777778,155.75,193.875,154.7777778,180,129.3,147.1111111,166.875,168.8888889,177.7777778,101.4,153.5555556,154.4,147.6,160.7,156.6,174.5,121,149.7777778,147.4,125.8888889,139.3333333,175.75,137.5,142.5555556,118.5555556,132.1428571,139.3,156.6666667,157,111,175.3333333,148.9,153.8888889,152.7777778,148.1666667,170.5,183.8,144.3333333,93.4,156.7777778,111.8888889,164.125,175,153.2222222,169.1111111,122.7,128.5714286,187.7142857,138.7777778,128.4,147.125,156.375,125.7142857,182.625,147.5,156.375,176.6666667,132.125,165,129.125,170,178,129.4,141.7777778,130.25,170.6666667,159,156.1,176.5555556,139,124,165,147.75,178.2,107,163.5,165.7,135.6,153.8,128.5,144.75,145.7777778,160.7,78.83333333,122.7,154.8571429,131.1,112,90.4,101.2,92.65714286,112.1,120.7,124.4444444,146,106.5,71.7,50.7,133.1111111,122.4,131.8888889,156.5,92.375,107.5555556,115.4,106.9615385,69.1,155.1,64.8,152,149.585,153.3,205,171,139.8,154,156.5,175.3333333,167.1607143,169.3333333,162.4233333,208,171.6,106.8,133.8,123.8,131.2,122.4,124.4,124.4,110,115.6,122.6,116.6,144,146.4,113.8,156.2,123.75,97.66666667,109.75,128.8333333,106.8,131.6,136.6,113.4,113.4,99.4,132.4,103.4,101.8,111,133,114.4,120,119.4,155.8,125.2,136,158,149.6,126.8,137.2,160.4,139.8,122,142,157.4,153.4,152.4,131.4,149.4,126.6,182.8,121,143.8,110.4,125,134,104.2,106.8,148.2)

PHE_core = c(130.3636364,105.6444444,91.66666667,128.3846154,136.7,109,185.2,174.2,192.2,181.4444444,184,194.4285714,167.8,150.3,160.2222222,161.6666667,169.8,157.7,155.1,153.75,171.5,180.5714286,199,151.25,219.2,151.3333333,161.5555556,181.2222222,173,190.7142857,197,168.7,170.2,194.7777778,193.625,157.8333333,192.75,158.125,168.75,147,165.2,162.125,179.8,144.5,209.1111111,192.9,196.625,147.3333333,153.1111111,141.5555556,95.75,111.6666667,138.2222222,143.875,177.6666667,109.2857143,139.3333333,155.75,147.1111111,147.6,147.4,142.5555556,175.3333333,148.9,153.8888889,183.8,156.7777778,169.1111111,128.4,156.1,176.5555556,163.5,165.7,135.6,153.8,128.5,144.75,160.7,78.83333333,122.7,90.4,120.7,124.4444444,106.5,71.7,50.7,133.1111111,122.4,153.3,175.3333333,106.8,124.4,116.6,146.4,106.8,131.6,111,126.8,157.4,152.4,182.8,121)


res.phe <- wilcox.test(PHE_whole, PHE_core)
res.phe


# BNU case


BNU_whole = c(0.409090909,1.155555556,0.666666667,0.307692308,2.052631579,11.11111111,6.5,4.5,3,6.3,4.2,5,6,6.2,6.181818182,8.1,7.777777778,7,7.333333333,7.222222222,7,6.4,7.285714286,7.444444444,9,8,10,7.333333333,8.6,8.8,5.6,7.625,7.416666667,2.6,6.5,6.714285714,7.6,6.2,6.375,4.9,2.2,5.666666667,1.3,3.8,5.888888889,6.857142857,6.222222222,9.3,7.666666667,8.444444444,8.4,6.7,8.555555556,7.7,2.888888889,2.333333333,6.2,2.75,4.9,5.222222222,1.888888889,1.222222222,4.375,1.8,2.5,2.7,2.444444444,5.5,3.1,2.7,6.2,4.875,2.6,3.7,6,3.888888889,6.9,11,6.8,11.85714286,8.1,6.666666667,5.9,7.222222222,4.777777778,4.333333333,6.333333333,7.3,2.777777778,1.777777778,8,6.111111111,7.5,7.1,6.777777778,9.857142857,8.4,9,9.1,6.777777778,6.75,5.375,5.444444444,5.5,8.4,7.6,6.6,2,2.1,2.5,10.5,6.2,9.5,6.4,7.625,5.777777778,2.4,4.2,3.8,3.2,5.375,4,2.166666667,4.1,2.8,6.857142857,5.6,7.25,3.666666667,3,5.777777778,5.857142857,8,4.444444444,3.285714286,6,6.777777778,3.142857143,5.4,6.666666667,4.875,5.875,4.166666667,5.8,6.285714286,6,7.555555556,7,9.625,5.888888889,7.333333333,6.333333333,3,2.125,5.714285714,5,4.6,5.6,8.111111111,8.3,8,5,7.25,9.6,7.4,8.2,12.2,7.6,8,8.8,6.625,7.4,2.75,5.125,7.5,5,5.777777778,5.375,5.8,6.888888889,2.9,8.7,5.4,8,7.444444444,6.888888889,5.75,2.125,9.9,4.125,7.1,10,11.3,8,10.22222222,9.25,5.333333333,5.4,11.66666667,11.75,7.875,5.125,12.375,10.375,6,6.833333333,4.428571429,6.375,3.625,9,2.5,10.55555556,5.1,7.375,6.5,8.375,2.75,2.9,5.1,9,6.7,4.5,2.25,7.3,7.285714286,5.444444444,4.428571429,6.2,11.3,6.125,2.5,6.444444444,4.4,2.333333333,6.222222222,6.111111111,3.9,6.375,2.4,8.375,10.2,6.111111111,5.833333333,6.625,3.888888889,1,2.444444444,5.2,3.444444444,6.333333333,8.444444444,7,3.3,5.555555556,6.7,5.4,5.444444444,5.125,8.555555556,7.3,5.555555556,4.888888889,7.6,7.625,15.5,5.875,4.888888889,5.888888889,6.2,5,3.9,8,6.6,6.555555556,4.7,6.777777778,5.2,3.7,9,5.7,4.6,7.5,3,2.333333333,9.666666667,3.8,4.3,4.9,3.444444444,4.375,5,7.875,7.4,6.75,7.333333333,4.2,7.222222222,7.142857143,6.3,6.333333333,4.7,5,4.555555556,5,5,6.6,4,5.3,4.888888889,8.2,6,9.444444444,8.111111111,7.25,9.555555556,5,4.8,7.777777778,8.333333333,1.8,4.857142857,3.625,8.111111111,1.571428571,2.8,3.5,3.375,2.444444444,1.333333333,4.7,2.888888889,6.375,9.555555556,6.222222222,2.5,8,2.2,6.3,5.3,5.4,1.75,6.1,6.666666667,5,7.222222222,6.444444444,5.625,7.7,5.666666667,7.222222222,4.285714286,6,6.666666667,2.8,6.3,3.888888889,4.9,5.666666667,7.333333333,8.166666667,4.5,8.6,9,2.3,7.888888889,7.111111111,6.5,10.16666667,7.444444444,6.888888889,6,8.571428571,5.285714286,5.666666667,3.733333333,9,4.75,3.571428571,7.625,12.375,8,7.888888889,5.5,6.666666667,4.125,5.9,8,6.4,6.666666667,5.25,6.333333333,4.1,9.3,6.444444444,2.6,5.4,5.6,3.5,4.2,1.75,2.125,4.6,1.4,1.2,3.1,4,2.888888889,1.9,1.916666667,2.1,5.285714286,7.8,6.1,1.1,5.4,4.5,4.1,6.1,1.111111111,1,1.4,6.3,3.4,0.888888889,10.9,7,5.75,5.5,4.888888889,4.5,5.230769231,2,8.6,2.1,6.6,5.095121951,6.4,10.2,6,3.6,3,2.75,8.333333333,5.329285714,7.333333333,6.580952381,7.6,2.2,6,10,5.2,1.6,3,3,5.2,3.8,1.6,0.6,3.2,4.2,3.6,1,3,2.75,2.666666667,0.75,1.166666667,2.2,2.4,3,2.6,1.8,4.2,2.2,2.4,2.6,1.2,1.4,1.6,1.6,2.6,6.8,4,4.4,3.8,0,3.8,5,1.4,2,2,5.2,4.2,2,6,5.4,2.6,4.8,4.8,3.6,3.4,2.6,5.4,4.4,3.6,4.4,4)



BNU_core = c(0.409090909,1.155555556,0.666666667,0.307692308,4.5,5,8.1,6.5,3.8,6.222222222,4.375,5.5,6,8.1,6.666666667,6.333333333,7.1,8.4,10.5,7.625,2.166666667,6.857142857,5.4,5.875,5.8,5.888888889,7.333333333,3,2.125,5.714285714,7.4,7.5,10,11.66666667,10.375,6.833333333,3.625,6.5,8.375,5.1,9,2.25,7.3,2.5,6.444444444,3.9,8.375,2.444444444,5.555555556,4.888888889,3,2.333333333,4.555555556,7.25,5,4.857142857,8.111111111,3.5,2.888888889,6.3,5,5.666666667,3.888888889,4.9,5.666666667,8.6,7.888888889,6.888888889,3.733333333,9.3,6.444444444,2.125,4.6,1.4,1.2,3.1,4,1.9,1.916666667,2.1,1.1,6.1,1.111111111,1.4,6.3,3.4,0.888888889,10.9,6.4,8.333333333,6,3,3.2,3.6,2.2,2.4,1.2,3.8,4.2,6,4.8,3.6)

res.bnu = wilcox.test(BNU_whole, BNU_core)
  
res.bnu 

# DIA




DIA_whole = c(14.85909091,10.45222222,7.866666667,11.59230769,13.17894737,22.82222222,19.09,13.18,14.74,11.02,14.05,12.58,27.18,16.46,21.10909091,17.9,13.17777778,17.3,18.06666667,17.45555556,17.2625,15.94,18.25714286,17.76666667,18.87142857,16.68,17.1,15.64444444,19.05,16.88,17,20.925,18.33333333,15.44,15.15,17.77142857,15.99,15.98,16.8,13.56,14.1,17.31111111,13.54,17.2,20.01111111,19.31428571,20.84444444,15.02,17.46666667,18.88888889,16.77,14.21,16.38888889,12.74,16.35555556,13.75,13.02,14.2375,14.86666667,16.01111111,17.77777778,19.46666667,18.275,14.22,12.02222222,18.82,12.93333333,15.9,15.31,13.9,18.23,18.175,14.09,14.25,18.57,20.53333333,20.51,18.8,14.45,11.9625,16.57,15.31111111,16.31,20.36666667,17.01111111,16.57777778,18.03333333,16.38,16.51111111,11.72222222,20.87,18.16666667,12.49,14.59,14.02222222,20.07142857,22.36,16.25555556,15.04,15.97777778,16.075,23.6,17.3,18.46,21.01,17.5,22.98,15.83333333,14.6,15.575,19.93,16.86,14.2,13.53,16.2,18.03333333,14.18,18.18,21.62,17.36,17.85,15.58,15.43333333,17.15,13.51,21.7,19.18333333,18.9375,14.13333333,15.9375,23.68888889,20.78571429,23.54,23.91111111,15.17142857,18.1875,22.11111111,15.04285714,22.9,18.91666667,22.3375,18.5375,22.25,22.42,20.48571429,18.5125,17.64444444,16.25,17.4,14.07777778,15.12222222,13.31666667,18.91111111,12.65,15.72857143,17.16666667,16.32,15.36,20.5,23.93,21.925,17,18.8375,21.96,17.86,17.43,29.07,18.08,20.26,18.25,17.525,21.26,12.35,20.425,13.87,20,18.67777778,17.875,24.32,22.03333333,12.625,18.15,11.58,13.22222222,16.64444444,20.02222222,15.125,13.475,17.59,20.8,13.16,15.96,18.08,18.25,22.32222222,26.37777778,21.05,18.3,22.37777778,20.0125,21.8125,18.6,20.5,21.05,19.53333333,24.56666667,16.11428571,19.8375,18.3875,20.875,17.21666667,18.51,15.83,15.9,13,19.6125,17.1,11.87,16.52,20.79,16.06,14.5,14.4125,16.64,18.57142857,17.56666667,19.58571429,16.14,22.34,20.625,10.675,17.2,18.18,19.11111111,19.27777778,18.52222222,16.21,21.825,16.07,18.6125,19.28333333,13.23333333,15.1,16.3625,14.78888889,13.28,14.12222222,15.19,12.55555556,17.05,14.12222222,17.51111111,12.47,12.33333333,16.13,13.21,15.51111111,17.5625,18.85555556,14.11,15.2,12.77777778,15.57,15.4875,20.475,12.2,13.76666667,11.44444444,16.63,13.42857143,15.53,14.8,12.4,13.67777778,13.04,12.75555556,13.83,10.61,17.2,11.36,14.67,20.4,9.1,8.9,21.33333333,14,12.06,16.67,16.56666667,16.3,11.83,14.9625,13.94,12,14.41111111,11.21,12.12222222,19.18571429,17.44,14.07777778,15.06,22.21428571,10.93333333,11.19,15.72222222,16.66666667,14.88,13.61,12.91111111,14.55555556,14.48,22.03333333,19.81111111,14.9875,24.81111111,19.48888889,22.37,16.34444444,20.37777778,10.93,14.42857143,18.15,19.34444444,13.44444444,15.81111111,21.375,21.675,15.66666667,16.9,14.52,16.15555556,26.275,25.13333333,21.94444444,11.34,20.62222222,18.57777778,17.29,16.66,16.46,14.7375,12.16,16.24444444,14.2,12.32222222,16.25555556,19.375,15.64,12.06666667,12.67777778,22.4,12.41,21.43333333,15.77,14.66,22.07777778,14.69,15.34444444,23.2,22.7,15.1125,27.36,15.91666667,13.4,22.48888889,18.38888889,14.5,26.66666667,21.6,18.66666667,18.67,13.67142857,18.84285714,14.43333333,13.78666667,14.4375,14.0125,12.91428571,28.275,17.2375,13.4,16.01111111,15.5125,17.01666667,15.8625,14.22,16.42857143,11.48,12.13333333,11.6375,18.16666667,16.3,16.81,20.47777778,12.77,12.45,16.26,13.0375,13.72,12.1,14.2125,14.8,11.23,11.88,12.17,14.9625,15.52222222,13.7,11.48333333,12.98,14.74285714,12.38,12.215,12.56,11.28,11.32857143,10.71,14.46,12.3,10.31,12.06,10.51,13.7,11.33333333,12.51,12.91111111,15.28571429,12.425,14.58888889,10.2,12.52857143,9.12,14.08,10.75,16.66666667,14.65219048,15.49,19.04,25.9,12.8,17.65,13.85,22.3,15.36944444,17.4,17.85955556,18.4,15.8,12.8,13.2,12.6,15.8,14.4,12.8,12.2,10.8,11,10.8,11.6,15.2,11.4,11.4,11.8,9.75,11,10.75,11.83333333,11.2,12.2,10.8,10.2,11.2,12.2,14.2,11.8,11.8,14.6,14.8,12,11.8,11.2,16.6,11.4,12.2,13.6,11,14.4,14.4,13.6,13.8,13,13,12.6,15.8,12.8,12,13.8,12,12.2,11,11.8,10.8,10.8,14.6,13.2,12.2,13.8)



DIA_core = c(14.85909091,10.45222222,7.866666667,11.59230769,13.18,12.58,17.9,15.15,17.2,20.84444444,18.275,15.9,18.57,16.57,15.31111111,18.03333333,14.59,22.36,19.93,16.2,15.43333333,21.7,22.9,18.5375,22.42,14.07777778,15.12222222,18.91111111,12.65,15.72857143,17.86,13.87,15.96,22.37777778,21.05,24.56666667,18.3875,13,19.6125,16.52,20.79,14.4125,16.64,10.675,17.2,16.21,18.6125,14.12222222,15.2,13.76666667,9.1,8.9,10.93333333,14.9875,19.48888889,14.42857143,19.34444444,21.375,16.15555556,17.29,14.2,12.06666667,22.07777778,14.69,15.34444444,27.36,22.48888889,18.66666667,13.78666667,16.81,20.47777778,14.2125,14.8,11.23,11.88,12.17,14.9625,13.7,11.48333333,12.98,12.56,14.46,12.3,12.06,10.51,13.7,11.33333333,12.51,15.49,22.3,12.8,12.8,11.6,11.4,11.2,12.2,14.6,14.4,12.6,12.8,12.2,11)


res.dia = wilcox.test(DIA_whole, DIA_core)
res.dia


# DBI


DBI_whole = c(308.7,182.9,244.65,243.55,339.75,578.5,299,215.25,342,244,328.5,356.5,520,384,512,456,158.5,511,364,240,296.5,438.5,268,296,250,338.25,750,325,500.75,417,390.5,324,324,240,310,267.5,216,384,307.5,447.5,327,216.5,209,264,420.5,413,371.5,286.25,174,462,324,313,388,232,336.25,296,209.75,213.25,270,376,296,352,461,217.5,220.5,489.25,276,345,308,264,237.5,356,258,225.5,383.5,395.5,376,278,209.5,268.5,437,316.75,469.5,471,325.5,395,468.5,570.5,258.25,186.5,364.25,389,164,207.5,216,437.5,430.25,263,252,248,331,316,284.75,317,492.75,270,561.5,250.5,227.5,221.25,332,386.1,340.5,292,289,494.25,337.25,509,336.5,300.25,606,665,380.5,265,453,408.5,392,488.5,189.25,275.5,438,333.5,507.75,391.5,241.75,332.5,288.5,424,363.5,294.75,578.5,376,285.5,483,310.5,333,396,477.5,556,337.5,407.5,268.75,484,264,268,304,301,303.5,530.5,312,384.5,355.5,349,375,464,437.5,508,643,367,526,392,548,231.75,692.5,282,410,440,529.5,328,450,239.5,322,243.25,387,348.75,278.5,791,138,510.25,390,256,313,347.5,346,354,470.25,340,535.5,427.75,503.85,356.5,476,284.5,664.75,223.5,258,212.5,664,372,705.5,184,364,289.5,180,180.5,318,302,193,282.5,488,404.25,358,238,414,199,447,336.5,219.65,735.5,573.5,226.8,524,192,368.5,421.25,434.5,268,393.5,420,414.5,400.5,312,224,362,329,165.5,226.25,456.25,176,414.75,323.75,436,122,282,452,332,226,500,347.25,272,292.25,172.5,176.5,280.5,287.5,155.5,191.2,246,383.5,163.25,250,196,166,273,258,190,186,261.5,276,279.5,388,378.25,169,174.75,431.7,178.5,330.5,214,276,239,179.5,179,280,128,293,231.25,212,518.5,198,200,245.25,422,254,161,276,365,172,268.25,251.25,238,208,560,484,501,716.25,448,297,248,390,164.25,256.25,751,507.25,121.5,283.5,373.5,455,353,260,349.75,269.25,320,510.5,860,156.25,359,368.5,324,326,285.75,341,227.5,372.5,308.4,391.5,178,417.5,394,222.5,241.25,250,228.75,384.5,319,331,425.5,492,306,538,266,422,300,252,188.75,372.5,401,311,380.5,509.5,326,229,283.5,363,471.75,375,378,264.75,270,708.5,405,283,560,293,474.5,230.75,377.5,332,247.5,216,179.5,413,505,335.5,392.5,305.5,194.5,298,194,233.5,179.25,310.5,248,204.75,155.5,273.5,197.5,202,365.5,162.5,205,341.5,291,200.5,199.5,124.5,325,196.75,293.5,176,166.5,147.75,115.25,176.25,192.25,264.75,234,392,201.5,330,194.5,150.5,117.75,372.75,128,271,384.55,312.5,444.3,490,515,389.65,235,415.85,239,275,297,401,223,157,210.5,127.5,298.9,540,245,194.5,197.5,185.5,102.5,130.5,137.5,145.5,278,200.5,139.5,295,215,208.5,202.5,196.5,124,172,144.5,104,158,127,240,132.5,134,165,209.5,142,183,111,314,204,165.5,210,233.9,194,213,226,142,142,247.2,248,192.5,168,186,180.5,148,176,188.5,146,148.5,144,187,190.5)


DBI_core = c(308.7,182.9,244.65,243.55,215.25,356.5,456,310,264,371.5,461,345,383.5,437,316.75,468.5,207.5,430.25,332,289,380.5,408.5,363.5,376,483,337.5,407.5,484,264,268,464,282,313,427.75,664.75,258,372,180.5,318,282.5,488,238,414,226.8,524,268,414.5,226.25,292.25,191.2,169,174.75,254,501,448,256.25,507.25,373.5,269.25,324,308.4,222.5,425.5,492,306,300,372.5,326,375,335.5,392.5,310.5,248,204.75,155.5,273.5,197.5,365.5,162.5,205,199.5,293.5,176,147.75,115.25,176.25,192.25,264.75,312.5,415.85,157,245,130.5,145.5,202.5,196.5,132.5,210,142,248,180.5,148)


res.dbi = wilcox.test(DBI_whole, DBI_core)
res.dbi



# DSW

DSW_whole = c(55.04230769,32.91741935,32.91741935,55.04230769,41.41284615,6.6,50.79333333,30.98,40.2,56.73,77.25,40.9,5.537216981,50.9,40.635,7.3,5.89,77.23,9.3,0.5,4.7,49.39,11.55,8.6,4.6,7.2,71.19,40.1,22.845,10.9,44.655,20.515,11.7,45.1,2.6,29.4,23.68,19.4,37.68,23,18.6,16.62,18.04,23,5.3,63.655,22.8,10.6,13.2,20.4,18.6,28.67,11.5,30.4,21.1,34.9,33.145,21.825,38.1,33.2,9.6,11,14.9,15,39.745,25.485,30.35,28.1,38.9,46.9,32.6,23.85,14.6,5.755,45.62,18.22,1.735,1.1,36.325,12.8,52.83,28.3,18.605,20.29,19.045,31.1,11.7,20.6,61.4,36.8,19.09887755,23.8,9.8,19.3,27.02,16.9,3.825,9.5,37.83,6.5,38.6,2.8,13.02,10.435,3.4,1.2,9.9,37.66,31.035,24.225,0.8,5.537216981,3.3,14.8,25.675,9.8,11.9,40.3,22.5,48.3,23.64,5.537216981,6.6,21.5,63.1,28.29,1.38,6,13.545,15.3,5.3,19.09887755,18.215,18.56,23.7,26.6,7.5,7.985,26.1,13.2,9.445,51.535,17.6,13.825,1.6,8.9,10,9.2,0.6,19,7.1,54.3,11.985,23.6,3.5,10.1,6.255,9.6,5.5,0.9,3.5,11.3,2.5,9.8,7.1,9.87,2.6,16.5,8.8,12.5,28.8,8,8.4,0.4,12.8,8.2,2.6,1.3,0.4,9.1,50.245,5.5,41.705,7.4,35.575,27.675,4.4,32.48,5.537216981,0.5,3.3,5.537216981,0.2,1.3,0.4,1,0.4,6.1,2.1,7.64,3.8,0.1,5.537216981,5.975,41.41284615,0.9,0.7,3.1,0.3,2.1,21.5,0.825,34.71,8.9,24.59,7.3,11.305,31.5,41.3,4.6,14.8,15.375,12.09,17.7,10.8,23.91,1.315,19.09887755,1.8,31.8,41.41284615,58.2,15.52,26.6,7.68,17,22.5,11.315,4.2,0.7,6.4,36.745,38.9,32.86,54.6,67.335,47.995,53.235,30.555,55,34.1,42.9,35.7,73.6,81.595,48.5,39.4,51.91,35.69,26.7,18.4,43.185,14.4,34.4,5.537216981,40.515,30.4,43.9,52.005,26.8,49.27,41.41284615,22.2,76.76,64.26,37.1,30.9,47.8,54.5,38.145,64.3,55.1468,38.825,31.855,19.09887755,14.9,55.9,54.195,40.1,43.255,24.875,8,52.2,21.8,23,50.47,45,46.9,26.9,21.595,41.17,6.91,26.4,32.1,34.41,9.955,22.5,29.665,49.705,11.4,20.38,1.2,5.9,47.59,52.735,42.6,16.31,8.7,4.5,45.965,48.98,114.455,103.105,20.65,25.365,8.705,13.315,71.8,3.9,56.725,51.66,16.39,43.2,34.2,48.39,25.75,38.965,48.27,14,42.02,31.495,52.24,68.49,29.9,65.475,62.71,7.7,86.8,28.575,47.545,48.67,57.35,26.685,46.26,42.67,13.9,75.4,24.165,58.6,9,39.365,22.2,15.7,48.175,36.215,60.045,28.93,30.7,82.335,14.245,41.75,27.97,27.7,60.2,72.925,38.085,51.805,37.285,6.6,15,9.4,15.82,16.08,20.5,39.885,6.4,7.96,37.26,54.47,33.24,58.29,109.34,37.575,37.865,58.945,57.035,25.72,33.885,20.9,48.1,71.855,38.02,53.475,27.87,64.295,47.385,42.5,32,48.27,63.2,44.7,57.89,47.95,47.245,24.74,69.94,34.7,40.195,42.46,66.04,36.23,24.93,25.08,48.22,33.945,61.665,52.775,1.225,64.49,40.7,19.705,41.815,19,48.8,41.41284615,41.41284615,2.1,54.12,19.09887755,89.4,2.98,5.6,19.09887755,41.41284615,20.31,41.41284615,8.6,64.2,32.9,26,48.7,55.04230769,0.2,23.6,34.3,37.2,25.2,12.4,29.4,20.3,25.8,53.5,38.2,33,48.2,42.1,40.9,53.9,40.4,13.1,34.5,34.4,27,31.9,29,46.6,16.2,25.4,3.72,38.3,32.9,30.8,33.8,32.5,44.3,18.5,32.5,50.5,39.5,24.4,44.3,33.6,38.7,30.2,55.3,38.8,30,42.8,42.6,37.6,30.8,26.3,38.8,33.2,30.5,39.3,42.4)

DSW_core = c(55.04230769,32.91741935,32.91741935,55.04230769,30.98,40.9,7.3,2.6,23,22.8,14.9,28.1,45.62,52.83,28.3,11.7,19.3,3.825,0.8,25.675,6.6,28.29,26.1,51.535,13.825,19,7.1,11.985,23.6,3.5,7.1,12.8,5.537216981,2.1,5.975,0.9,0.3,24.59,7.3,41.3,4.6,12.09,17.7,41.41284615,58.2,22.5,0.7,47.995,18.4,30.4,38.825,31.855,26.4,47.59,42.6,48.98,103.105,8.705,51.66,48.27,29.9,28.575,13.9,75.4,24.165,22.2,36.215,14.245,72.925,37.575,37.865,71.855,38.02,53.475,27.87,64.295,47.385,32,48.27,63.2,47.245,40.195,42.46,36.23,24.93,25.08,48.22,33.945,2.1,19.09887755,32.9,23.6,29.4,25.8,53.9,40.4,16.2,32.5,38.7,55.3,42.6,37.6)

res.dsw = wilcox.test(DSW_whole, DSW_core)
res.dsw


# TSW

TSW_whole = c(
  2.594314847,2.949444658,2.706659506,2.921458338,2.968232885,1.51328955,2.734609826,2.148426347,2.268875267,2.26774781,2.70137011,2.54274781,2.455319517,3.073604397,2.84771687,2.748974605,2.12,2.88502335,2.571808717,3.52861471,3.06161004,2.85978342,2.78705896,2.4429568,2.63208231,2.21550321,2.886041155,3.47399416,2.368796555,3.13722942,2.69023234,3.1638237,3.273116947,2.47705896,3.07065032,2.73478342,3.00430356,3.073604397,2.794282934,2.758316695,3.058834014,3.112696244,3.028155477,3.323364467,2.4793345,2.59999241,2.530013037,2.721730005,2.850670947,3.227017707,2.872466627,2.6507122,2.84909457,2.96861471,3.57400175,2.631568787,2.24319673,3.12585931,3.15041039,2.972007394,2.56119206,3.019971784,3.003145164,3.724700914,3.695160147,3.30651722,3.514700914,2.477017707,2.490013037,2.918627747,2.28681903,2.62137011,2.310513524,1.831527533,1.97688091,2.173686904,1.7,1.989637069,2.86292586,3.305979275,2.8365791,2.794556527,2.56388558,1.86915645,2.63113018,2.429793734,2.3041798,2.39616112,2.629313874,2.232528507,2.5,2.801808717,3.25747694,3.39951255,2.969313874,2.652760847,2.18,3.28788733,2.934522864,3.0338237,2.849742167,2.64094454,2.00143199,1.87412551,1.68556509,1.40963631,2.35502335,3.12268593,2.922706557,2.66633917,1.83,1.944,2.38478342,1.941870597,2.359793734,2.575561295,2.895561295,2.505057013,2.62909457,2.546839657,2.28298774,2.28,2.28645534,2.440691574,2.854762794,1.875623175,2.00286398,1.64215178,2.37322767,1.886730005,2.061945513,1.96,1.928200525,2.010732827,2.650910877,2.08526328,1.78280969,1.8148453,2.365461957,1.91712084,2.04047227,2.694084257,2.35843666,2.45526328,2.48,2.097758124,1.819711227,1.978457287,2.404844249,2.447518194,2.219934325,2.90250788,1.98819673,2.34047227,1.3,2.044584744,1.54532516,2.597758124,1.712929655,2.16238412,2.10921074,1.464166764,2.01756976,2.138751507,2.04909457,1.76188091,2.44597548,2.33274781,1.789036485,1.87526328,2.6765791,2.09957443,2.554564117,2.04,2.451150807,2.10921074,2.715603093,2.24,2.331563572,2.55232224,2.273875267,2.72544133,2.635910877,2.18068126,2.745561295,2.684005545,2.86202802,2.6707122,2.323839112,2.721019225,2.20370753,2.889714263,2.31960537,2.325722942,2.135511675,2.150731309,2,2.38843666,1.990856917,2.06,2.07376182,2.442315177,2.042002393,2.16,2,2.2,2.98,2.05603736,2.095687332,2.31095213,2.433426347,2.08,2.641589414,2.548796555,2.66202802,2.408974605,2.175722514,2.75292586,2.989971784,2.07376182,2.71202802,1.90640105,2.0607122,2.869533177,1.77256976,2.859454465,1.34,2.392918583,2.463741622,2.85363806,1.82,2.90478342,2.407518194,2.367977427,2.04,2.10095213,2.461829344,1.83,2.20777875,2.570649231,2.11807297,2.665900564,2.90430356,2.810211714,3.106558474,2.580900564,2.31819673,2.661568787,2.94133917,2.759553804,3.50058085,2.54161004,2.875900564,3.194940844,2.782706557,2.74544133,2.567497567,2.34,2.771568787,3.283703735,2.527278264,3.006777777,2.658974605,2.878155477,2.655535784,2.63,3.235739345,2.502466627,2.781349484,2.99813485,2.632048647,3.052304649,3.34106071,2.750879937,2.86816579,2.76681903,2.97496147,2.887497567,3.089964193,2.93223701,2.79202802,2.7,2.884982097,2.734053317,2.502809261,2.588655964,2.753594084,3.14723701,2.94472154,3.00358377,3.096668125,3.11975248,3.302657713,3.31699708,3.04609924,3.270369137,2.85400175,3.214742167,3.7221366,2.795,3.033333334,2.18,3.06,2.918333334,2.335,2.286666667,2.98,3.1,2.87,3.031088927,2.68089025,1.968642498,3.0131582,2.963145164,2.445863105,2.591568787,3.050869624,2.67975248,2.86268593,2.863145164,3.02651722,2.880629694,3.125400077,2.389553804,2.63178809,2.648045825,3.005081435,2.406140494,2.4665791,2.567487254,2.546938995,2.40184997,2.33592119,2.82154816,2.882466627,2.649971784,3.620770285,2.721666667,2.005,2.188333334,3.2525,2.778333334,2.338333334,2.678333334,3.128333334,2.815,2.8,3.051777777,3.077915547,2.612487254,2.687278264,2.936777777,2.88747694,2.74747694,2.90633917,2.386938995,3.024974507,2.653604397,3.13722942,3.25243841,2.461829344,3.249484333,2.762466627,3.428395407,2.6038237,3.112905234,2.836219205,3.259293247,3.049491924,2.77585931,3.07358377,2.762627845,3.34992294,2.330691574,2.876318544,1.964584744,2.57202802,2.99517046,3.02609924,2.050129207,2.520910877,2.758494745,2.851666667,2.6925,2.72,1.745,1.995,2.86,2.75,3.068333334,2.631666667,2.755,2.63,2.575,2.67,2.648333334,2.383333334,2.87,2.6825,2.898734675,3.05720607,2.49137011,2.499553804,2.734773107,3.114951157,3.027696244,2.947696244,2.926328857,2.46,2.884084257,2.646666667,2.795,2.491140494,2.755660634,2.413457287,2.02,2.272079587,2.435482584,2.77089025,2.73178809,2.51637011,2.765,2.716666667,2.798333334,1.863333334,2.6475,3.148333334,2.16,2.495,2.775,2.446666667,2.58,3.065,2.518333334,2.243837302,2.782760186,1.78,2.3,3.02,2.52,2.02,1.96,2.637914029,2.85779062,2.31,3.094111013,2.723658687,2.733333333,2.703333333,3,2.476666667,3.122523875,2.572149737,3.106666667,3.273333333,2.7,2.486666667,2.766666667,2.606666667,2.673333333,2.74,2.653333333,2.973333333,3.3,2.993333333,3.286666667,3.006666667,3.146666667,2.833333333,3.14,3.156666667,2.663333333,2.57,3.106666667,2.903333333,3.08,3.246666667,2.933333333,2.788648373,2.95112259,2.812280987,3.338106633,2.894995133,2.912719593,2.97770928,2.912719593,3.078147887,3.042698967,2.894995133,2.78274022,2.99543374,3.70441214,3.237668027,2.658669,2.954076667,3.06633158,2.971801127,2.72956684,2.81818914,2.930444053,3.001341893,3.036790813,3.397188167,2.85363806,2.510965167,2.88908698,3.001341893
)


TSW_core = c(2.594314847,2.949444658,2.706659506,2.921458338,2.148426347,2.54274781,2.748974605,3.07065032,3.323364467,2.530013037,3.003145164,2.477017707,1.97688091,2.8365791,2.794556527,2.3041798,3.39951255,2.18,1.83,2.359793734,2.28645534,1.875623175,2.365461957,2.694084257,2.45526328,2.447518194,2.219934325,1.98819673,2.34047227,1.3,2.04909457,2.451150807,2.889714263,1.990856917,2.16,2.2,2.095687332,2.66202802,2.408974605,2.989971784,2.07376182,2.0607122,2.869533177,1.82,2.90478342,2.461829344,2.570649231,2.31819673,2.527278264,3.235739345,2.884982097,2.734053317,3.06,2.963145164,2.591568787,3.02651722,3.125400077,2.648045825,2.546938995,2.721666667,2.678333334,3.077915547,2.386938995,3.024974507,2.653604397,3.249484333,2.6038237,2.77585931,2.876318544,2.63,2.575,3.05720607,2.49137011,2.499553804,2.734773107,3.114951157,3.027696244,2.926328857,2.46,2.884084257,2.755660634,2.435482584,2.77089025,2.51637011,2.765,2.716666667,2.798333334,1.863333334,1.78,2.637914029,2.703333333,3.106666667,2.606666667,2.74,3.146666667,2.833333333,3.246666667,3.078147887,3.237668027,2.954076667,2.81818914,2.930444053
)

res.tsw = wilcox.test(TSW_whole, TSW_core)
res.tsw




# FLO


FLO_whole = c(62.25,59.71428571,46.81734427,49.57142857,72.57142857,54.5,61.5,46,54,43,54,42.5,45,43.5,42,65.5,54.5,58,46.5,69,70,47.5,71,57.5,46.5,46.5,46.5,48.5,46.5,46.5,57,46,47.5,52.5,68,51,47,48,67.5,50.5,56.5,47,43.5,53,67.5,46.5,48.5,45,48,45.5,74.5,47,49.5,43.5,63.5,44.5,63,43,45.5,45.5,52.5,49,48,42.5,67,60,43.5,44.75,66.5,43,46,48.5,71,52,46,47,69,67.5,44.5,41.5,39.5,56,52,56,71,48.5,58,69,47,66.5,53,68,42.5,45,43,50,44.5,50,71.5,58,45.5,84.5,50,50.5,54.5,56,51.5,44,46,47,52,42,50.5,50,49,47.5,45.5,46,70.5,49,54,88,55,48.5,40.5,50.5,54.5,54,61,50,51.5,52.5,53,51,70.5,71.5,75.5,75,71,68.5,77,70.5,48.5,73,71.5,73,72.5,71,79.5,75.5,72,56.5,50,69,73,77,73,73,75.5,72.5,46.5,72,71,72.5,71.5,67,51,62,52,71.5,55,56,46,52.5,71,62,69,68.25,75,66,40,65,58.5,65.5,56,61.5,61.5,59.5,44,54.5,53,48,38,54.5,55,53,54.5,51,51,53,69,60,83,71.5,80,69.5,70.5,66,64.5,54.5,47.5,49,57.5,40.5,60.5,65,60,65.5,65,71,69,53.5,73,69,72,52.5,67.5,40,68.5,50.5,48,56,72.5,61,76,42.5,47,57,47,63,49.5,50,40.5,46,47,54.5,67,45.5,69,43.5,49.5,63.5,49.5,45.5,44.5,49.5,71.5,72,48.5,46,48.5,66.5,51,41.5,73,61.5,66.5,66,48.5,47.5,64,67,64.5,39.5,40.5,46.5,66,50.5,67.5,67,60,59,41,53,64.93909229,52.5,40,48,49,62,41,41.5,62.5,62,64,41,50.5,43,49.5,64,38.5,56,56,57,49.5,56,43.5,59.5,45,65.5,47.5,75.5,76,62,50.5,47.5,57.5,46.5,54.5,42.5,68.5,55.5,54.5,69,47.5,50.5,69,56.5,61,65.5,63,47.5,57.5,67.5,53,69.5,57.5,48.5,48,71,69,40,52.5,54,43.5,57,63,40,41,43,42,64,64.5,67.5,38,46.5,64.5,42,44.5,69.5,63,56,63.5,43.5,46.5,44,40.5,44.5,55.5,66,85,41,42,53.5,42.5,43,64,43,72.5,45.5,65,57,66.5,47,64,55,44.5,41,40.5,50.5,42,63.5,65.5,56,38,59,63.5,43.5,44,59.5,52,53,51,63,51.5,45,60,50.5,63,60,65.5,64.5,71.5,67.5,42,62,61,59.5,57,62.5,43,54.5,62.5,43,64,41.5,44.5,67.5,42.5,43.5,43,62,59.5,42,39,41,51,40,42,90,42,38,54,39,62,40,53,51,45,45,45,47.4096892,62,42,38,41,42,37,41,41,41,42,42,42,42,42,43,42,42,42,42,42,42,42,42,42,42,42,42,43,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,42,42,42,42,42,42,38,45,46,41,42,41)


FLO_core = c (62.25,59.71428571,46.81734427,49.57142857,46,42.5,65.5,68,53,48.5,48,44.75,46,39.5,56,58,45,44.5,52,49,55,50.5,71,70.5,73,75.5,72,50,69,73,71.5,71,48,51,71.5,69.5,64.5,60.5,65,65,71,73,69,48,56,47,63,67,48.5,66.5,41,53,56,62,47.5,68.5,54.5,50.5,63,48.5,54,41,46.5,64.5,42,56,46.5,66,42.5,65.5,56,52,53,51,63,51.5,45,50.5,63,60,67.5,59.5,57,43,54.5,62.5,43,64,51,54,45,42,41,41,42,42,42,42,42,43,42,42)



res.flo = wilcox.test(FLO_whole, FLO_core)
res.flo



# MAT


MAT_whole = c(98.75,95.57142857,94.02173913,94.07142857,110.8571429,103,103.5,89,96,87,91.5,90,95.5,89.5,89.5,107.5,91.5,91.5,94,112,111,92,108.5,91.5,90.5,92.5,92.5,91.5,92.5,93,97,94.5,97.5,97,114.5,97.5,97,98.5,113.5,97,97.5,92,90.5,97.5,114.5,94.5,94.5,88,90.5,89,118,92,96.5,97,116,98,118,96.5,100,95,98,97,97.5,99.5,117.5,99.5,100,98.25,115.5,94,95.5,95,114.5,98,94.5,99.5,115.5,116.5,98.5,95.5,107.5,95,103.5,102.5,120,97.5,98.5,116.5,98.5,112.5,97.5,116,95.5,113.5,90,114.5,99.5,96.5,111.5,98.5,93,126.5,99.5,98,102,103.5,98.5,84.5,96.5,95,106,90,101.5,102,101,95,98,95.5,114.5,95,98,125,102,98.5,92.5,98,99.5,97.5,105,95.5,107,99.5,108,99.5,111.5,113.5,117,115,117.5,119.5,118,115.5,95,114.5,117,118,117.5,117.5,121,116,118,110.5,100.5,122.5,117,123,119.5,120,120,120,97.5,120,117.5,119,116.5,121.5,98.5,117,105,119.5,115.5,104.5,112,98.5,123,112,120.5,118.25,118,119,98,106.5,105.5,109.5,105,117.5,118.5,108,90,92,90.5,93,84,96.5,103,103.5,100.5,101.5,97,98.5,118,103.5,119.5,113,120,121.5,119,118.5,114,103,108.5,102.5,110.5,91.5,116.5,121,118.5,107,105.5,112.5,119.5,102,116.5,120.5,119,103.5,116,79,118,115.5,122,115.5,123,113,122.5,102.5,109.5,110.5,115,105,96.5,101.5,90,106.5,101.5,102,112,94,116,105,101.5,106,91.5,92,92,92,115.5,117.5,101,99,99.5,108,92.5,87,110,102,110,107.5,101.5,93,108,109,110,80.5,86.5,102.5,108.5,102.5,109,111,108,83,88,107,100.31875,109,84,110,102.5,104.5,89.5,83.5,105.5,106,108.5,84,104.5,92.5,88.5,104.5,82.5,115,113,113.5,113,109.5,95.5,115,100.5,106,85.5,111,112.5,108,92.5,87,98.5,99.5,98,89,116,104.5,100.5,103,99.5,100.5,118.5,99,101,110,108.5,91.5,93,109,100,109.5,96,100,94,116.5,116,81.5,108,101,90,101.5,116,95,98,88.5,89.5,108.8,107,106.5,88.5,91.5,106,88.5,89.5,116,117,105.5,106,85,91.5,87,88,93,94,108,133.5,86,97,91,88.5,97,113.5,98.5,118,98.5,113,101,118.5,98.5,112.5,100.5,95,84,88.5,90,96.5,103.5,106.5,96,88.5,103,118.5,99.5,101,106,101.5,88.5,89.5,103.5,89.5,87,102.5,88.5,98.5,105.5,117.5,108,112,105.5,86,100,106,100,104.5,105.5,85.5,88,114.5,99,104.5,88.5,97,115,98.5,88.5,90,106.5,99,84,88,92,98.5,83,89,128,89,74,92,90,92,81,107,108,88,88,80,86.94186047,108,91,81,86,84,79,88,81,81,87,86,85,91,91,91,91,91,84,87,85,85,91,84,93,91,81,91,85,91,88,91,87,85,84,87,85,85,85,85,85,85,85,85,85,85,88,81,85,85,84,87,88,88,88,88)

MAT_core = c(98.75,95.57142857,94.02173913,94.07142857,89,90,107.5,114.5,97.5,94.5,97.5,98.25,94.5,107.5,95,98.5,113.5,99.5,106,101,102,98,117.5,115.5,114.5,116,118,100.5,122.5,117,116.5,123,93,97,113,121.5,114,116.5,121,105.5,112.5,116.5,120.5,122,115.5,109.5,105,112,99.5,110,88,107,113,108,87,116,100.5,100.5,108.5,100,101,98,91.5,106,88.5,105.5,91.5,108,88.5,106.5,96,101.5,88.5,89.5,103.5,89.5,87,88.5,98.5,105.5,105.5,100,104.5,85.5,88,114.5,99,104.5,98.5,92,88,91,88,81,91,91,91,87,85,85,81,85)

res.mat = wilcox.test(MAT_whole , MAT_core)

res.mat 



# FTM


FTM_whole = c(36.5,35.85714286,48.26666667,44.5,38.28571429,48.5,42,44,42,44,37.5,47.5,46,46,47.5,42,37,33.5,47.5,43,41,44.5,37.5,34,44,46,46,43,46,46.5,40,48.5,50,44.5,46.5,46.5,50,50.5,46,46.5,41,45,47,44.5,47,48,46,43,42.5,43.5,43.5,45,47,53.5,52.5,53.5,55,53.5,54.5,49.5,45.5,48,49.5,57,50.5,39.5,56.5,53.5,49,51,49.5,46.5,43.5,46,48.5,52.5,46.5,49,54,54,68,39,51.5,46.5,49,49,40.5,47.5,51.5,46,44.5,48,53,68.5,47,64.5,55,46.5,40,40.5,47.5,42,49.5,47.5,47.5,47.5,47,40.5,50.5,48,54,48,51,52,52,47.5,52.5,49.5,44,46,44,37,47,50,52,47.5,45,43.5,44,45.5,45,47,55,48.5,41,42,41.5,40,46.5,51,41,45,46.5,41.5,45.5,45,45,46.5,41.5,40.5,46,54,50.5,53.5,44,46,46.5,47,44.5,47.5,51,48,46.5,46.5,45,54.5,47.5,55,53,48,60.5,48.5,66,46,52,50,51.5,50,43,53,58,41.5,47,44,49,56,57,48.5,46,37.5,37.5,42,38,42,48,50.5,46,50.5,46,45.5,49,43.5,44,41.5,48,52,48.5,52.5,49.5,48.5,61,53.5,53,38,56,56,58.5,41.5,40.5,41.5,50.5,48.5,43.5,51.5,47,51,48.5,39,49.5,65,74,59.5,50.5,52,46.5,60,62.5,53.5,68,42,47,51.5,49.5,60.5,54.5,47.5,45,48.5,47,61.5,52,42.5,42,46.5,47.5,42.5,44,45.5,52.5,53,51,41.5,41.5,45.5,37,40.5,43.5,41.5,53,45.5,44,42,45.5,41,46,56,42.5,52,41.5,44,48,24,47,54,42.6,56.5,44,62,53.5,42.5,48.5,42,43,44,44.5,43,54,49.5,39,40.5,44,59,57,56.5,63.5,53.5,52,55.5,55.5,40.5,38,35.5,36.5,46,42,39.5,41,53,43.5,46.5,47.5,49,46,34,52,50,49.5,42.5,40,44.5,45.5,44,35.5,41.5,47,40,38.5,51.5,46,45.5,47,41.5,55.5,47,46.5,44.5,53,55,57,45.5,47.5,44.8,42.5,39,50.5,45,41.5,46.5,45,46.5,54,49.5,42.5,41.5,45,43,47.5,48.5,38.5,42,48.5,45,55,37.5,46,54,49.5,55.5,45.5,53,48,44,52,51.5,48.5,45.5,50.5,43,48,39.5,54.5,40,41,40,50.5,44,55,56,57,46.5,49.5,35.5,38.5,40.5,38,42,42.5,38,35.5,45.5,52,43.5,40.5,38,44,38,45,40.5,47.5,43,42.5,33.5,52,56,40.5,47,52.5,47.5,56,45,47,44.5,39.5,42,49,51,47.5,43,47,38,47,36,38,51,30,41,54,57,43,43,35,48.26666667,46,49,43,45,42,42,47,40,40,45,44,43,49,49,48,49,49,42,45,43,43,49,42,51,49,39,49,42,49,46,49,45,43,42,45,43,43,43,43,43,43,42,42,43,43,46,39,43,43,46,42,42,47,46,47)


FTM_core = c(36.5,35.85714286,48.26666667,44.5,44,47.5,42,46.5,44.5,46,49.5,53.5,48.5,68,39,40.5,68.5,55,54,52,47,47.5,46.5,45,41.5,40.5,46,50.5,53.5,44,45,52,42,46,41.5,52,49.5,56,56,40.5,41.5,43.5,51.5,74,59.5,62.5,42,45,51,43.5,47,54,57,46,39.5,47.5,46,50,45.5,51.5,47,57,45,41.5,46.5,49.5,45,42,46,41,40,49.5,35.5,38.5,40.5,38,42,38,35.5,45.5,38,40.5,47.5,42.5,33.5,52,56,40.5,47.5,38,43,49,47,40,49,49,49,45,43,42,39,43)

res.FTM = wilcox.test(FTM_whole , FTM_core)

res.FTM


# CLE

CLE_whole = c(26.815625,27.96421569,26.26153846,27.96421569,26.815625,21,16.66666667,23.4,21.6,27.2,27.4,22.8,29.43333333,26.2,23.2,19.2,18.6,20,22,20.6,19.5,22.4,23,25.6,23.6,22.8,21,26,22.4,23.8,21.4,22.4,22.4,27.8,23.8,23.6,22.4,22.4,24.8,24.6,30.4,24,24.2,27.4,22.4,23.6,23,21.2,22.4,25,25.8,26.2,24.8,24,34.2,30.2,31.8,23.8,28.2,25.6,26.6,25.2,30.33333333,32.8,33.2,26.6,32.4,28.4,28.8,24.8,26.4,25.4,21.6,23.6,23.8,22.6,21.6,21.2,23.6,24.2,24.8,21.6,23.8,23.6,22,25,22.8,22,26.4,25.2,24.2,24.6,28.4,26.8,22.6,21.33333333,24.8,25.4,29.4,22,27.66666667,19,21.6,21.8,18.8,23.66666667,20.4,27.8,25.2,24,14.4,23.30560748,19.4,19.2,19.8,23.6,26.2,23.6,22.8,23,23.25,23.30560748,28,26.4,27.6,22.5,24.33333333,22,23.66666667,23.66666667,24.25,20.33333333,19.97222222,23.4,21.33333333,21.66666667,26.6,25.5,27,20,27,22,23.33333333,25.66666667,23.4,23.8,23.4,24,24.33333333,19.4,21.25,22,23.25,26.33333333,22,20.6,19.6,24.2,25.4,23,22,22.4,23.25,21.6,24.66666667,19,22.2,21.4,20.4,19.6,25.4,23,22.66666667,21.33333333,22.6,24.33333333,26.25,23,25,31.25,27,24,27.2,27.4,22.6,23.6,24,31.4,20,19,23.6,29.43333333,29.43333333,18.5,20,20.25,22.66666667,23,21.25,18.75,22.2,21.80782313,21.80782313,23.33333333,22.50833333,22.25,15.66666667,17.66666667,20.66666667,19,23,25,27.6,22.6,24.5,20.25,21.5,23.2,27,19.2,21.6,22,23.66666667,24.8,20.5,24.25,24.66666667,23.30560748,25.6,24.66666667,26.26153846,23.75,23.33333333,34.2,23.4,22.4,22,23.5,24.6,20,23,26.6,28.5,20.25,31,26.4,24,23.8,26.5,24.66666667,26.4,27.2,29.6,29.6,26.2,28,20.25,19.66666667,19.6,20.4,21.8,25.8,23.2,26.6,22.99117647,26.8,25.4,27.2,26.8,30,27.2,29.6,25.8,28.8,28,31.2,29.2,30.2,28.2,27.6,28.8,24.90909091,26.26153846,26.26153846,25,29,31.8,26.4,30.2,30.2,25.4,27.2,24.6,28.2,29.4,33.6,26.2,24.4,28.4,27.2,15,22.2,30.25,23,24.75,21.8,23,23.8,28.4,23.2,23.2,19.8,17.6,28,23.2,25.2,21.6,20.4,18.8,27.2,21,29,22.2,28.5,23,23.75,31.66666667,22,25,22,24.2,24,20.8,25.8,24.8,25.4,26.4,20.6,23.2,24.33333333,31.66666667,27,23.25,25.2,31.8,28.2,23,24.8,24.25,24.4,27.2,22.4,17.8,21,24.6,22,27.8,20,24,25,21.66666667,24.66666667,20.8,18.2,24,22.4,23.8,25.6,25.2,23.6,27.2,23.6,19.66666667,25.2,27.8,17.66666667,23.2,19.5,25.66666667,19.33333333,18,24.25,20.66666667,20.66666667,23.33333333,21.8,21,24.8,31.8,26.8,23.33333333,31.2,22.8,19,26.2,27.8,34.5,21.33333333,22.66666667,30,30.4,26.4,25.2,27.8,28.4,27.8,28.6,33.2,26.8,28,20.66666667,21,27.4,30.4,26.6,29.4,28.4,25.8,29.2,25.8,25,33.8,22,32,15.2,34.6,28.66666667,30.5,24,23.6,24.2,20.6,25,20.8,33.66666667,23.30560748,20,24.90909091,23.34358974,26.815625,21.80782313,21.80782313,22.99117647,27.96421569,23.30560748,30.58181818,20.8,32,29.2,26.2,28.8,34.8,34.4,30.8,29,26.4,29.8,24.4,27.8,27.2,26.4,27,27.8,31,26,26.25,28.83333333,28.6,24.8,25.8,27.8,30.2,33.8,28.6,28.6,27.6,25.6,30.4,31.2,26.2,28,26.4,28.2,26.2,25,27.8,27.6,30.2,30.6,29.2,29.8,30.6,31.6,29.4,27,25,31,27.2,26.6,30.2,28.6,26,24.2,28.8,28,31.8,41.4)

CLE_core = c(26.815625,27.96421569,26.26153846,27.96421569,23.4,22.8,19.2,23.8,27.4,23,30.33333333,28.4,23.8,24.8,21.6,22.8,26.8,24.8,14.4,19.8,28,22.5,27,22,25.66666667,19.4,21.25,23.25,26.33333333,22,24.66666667,22.6,29.43333333,21.25,23.33333333,22.25,20.66666667,24.5,20.25,27,19.2,23.66666667,24.8,26.26153846,23.75,22,20,24,21.8,25.4,26.26153846,26.26153846,30.25,28,25.2,21,22.2,23.75,24.2,20.6,25.2,24.25,22,27.8,20,24.66666667,24,23.6,27.8,22.8,19,30.4,26.4,25.2,27.8,28.4,27.8,33.2,26.8,28,30.4,25.8,29.2,25,33.8,22,32,15.2,20,22.99117647,29.2,30.8,27.8,26.4,28.6,24.8,25.6,27.6,31.6,27,26.6,30.2)


res.CLE = wilcox.test(CLE_whole , CLE_core)

res.CLE


# CWI 


CWI_whole = c(8.189,8.681,7.032666667,9.504,7.666666667,5.7,7.6,7.64,7.36,7.16,7.68,8.34,7.742,8.24,7.08,6.5,7.94,7.04,6.94,6.72,6.975,7.08,7.48,7.56,6.98,7.18,7.26,7.62,7.06,7.12,7.06,7.36,7.5,7.44,7.1,6.54,6.98,6.74,7.02,8.56,8.7,8.16,8.24,7.8,7.24,7.66,7.34,6.7,6.62,7.34,7.4,8.2,7.44,7.74,9.54,8.4,7.14,8.16,7.38,8.52,8.5,7.72,7.966666667,9.9,9.4,9.7,9.54,7.68,7.58,7.52,9.78,7.08,6.8,6.38,6.68,7.62,6.38,6.94,6.94,8.66,7,7.6,8.28,6.18,7.52,7.66,7.04,7.46,7.9,7,7.18,7.28,8.22,8.12,7.68,7.7,8.3,7.36,7.66,7.28,8.166666667,7.533333333,7.48,7.32,6.88,7.333333333,7.36,8.34,7.72,8.04,6.64,8.061333333,8.24,8.46,8.44,7.7,8.02,7.48,6.92,7.34,7.15,7.288666667,7.55,7.22,7.44,6.15,7.433333333,7.466666667,7.6,8.166666667,7.825,6.466666667,7.240666667,7.4,7.4,6.9,7.06,7.2,7.55,7,7.28,8.02,7.9,7.5,7.7,7.78,7.44,7.36,7.4,8.28,7.975,6.75,7.125,8.066666667,6.933333333,7.46,7.28,7.66,6.9,7.26,7.933333333,7.48,7.45,7.5,7.4,6.9,7.24,6.98,8.4,6.84,7.46,7.66,6.666666667,6.766666667,7.34,7.2,8.525,7.9,7.833333333,6.65,8,7.46,8.08,7.88,7.46,7.6,7.833333333,10.18,6.7,8.9,8.26,7.534666667,7.84,7.5,7.8,9.325,8.466666667,7.3,8.525,6.65,7.85,7.6,7.250333333,6.8,7.414666667,7.575,6.333333333,7.033333333,7.533333333,7.1,6.9,7.84,8.94,7.18,7.775,7.6,6.85,5.92,8.5,6.78,8.34,7.166666667,11.43333333,7.68,7.65,6.925,7.166666667,6.762666667,7.56,6.833333333,8.918666666,8.225,7.566666667,8.7,7.9,7.62,6.92,6.925,7.86,7.766666667,6.8,7.14,7.55,7.525,7.46,7.98,7.22,7.48,7.875,6.566666667,7.18,6.94,7.72,7.38,7.54,7.12,7.5,6.966666667,8.3,7.52,6.02,7.22,7.3,8.28,6.906,7.58,7.26,7.46,7.18,7.66,7.28,7.32,7.06,7.6,7.16,8.56,7.44,7.02,6.86,7.66,7.32,7.388,8.696,7.672,7.166666667,8,7.64,8.28,8.38,8.06,7.32,7.3,6.96,7.7,7.6,7.56,7.08,7.84,7.86,8.12,11.78,7.16,7.325,6.88,7.65,7.66,7.633333333,7.92,8.1,7.66,7.32,6.6,7.06,7.4,6.56,7.18,7.56,6.66,7.5,8.76,8,7.533333333,6.86,6.825,7.15,8.575,7.7,10.075,8.25,8.88,7,7.92,6.74,7.12,8.22,8.22,9.58,7.28,6.88,6.666666667,10.1,7.38,8.125,7.08,8.12,8.14,8.05,7.14,7.925,6.64,7.2,7.04,6.58,7.2,7.6,6.4,6.6,7.1,6.76,7.1,6.066666667,6.8,7.36,7.74,6.98,7.38,7.32,7.84,7.64,6.82,7.88,6.54,7.033333333,7.24,7.76,8.533333333,7.18,6.75,8,5.866666667,6.8,7.65,8,7.233333333,7.033333333,6.8,6.133333333,7.48,7.42,8.3,7.2,7.48,7.64,7.125,7.64,6.5,6.6,6.933333333,7.866666667,9.92,8.38,6.68,7.78,10.18,8.62,8.64,8.8,8.92,8.74,9.52,7.166666667,6.62,7.78,9.46,5.62,6.86,6.68,7.72,8,8.08,9.68,8.74,7.34,8.7,5.88,7.08,6.566666667,6.875,7.1,6.65,6.72,8.02,6.78,8.38,4.4,7.244,8.1,7.448,7.328,6.962666667,8.363666667,7.808,7.264,6.844,7.268,7.55,8.8,6.58,8.84,7.28,7.44,9.4,9.32,9.2,8.34,8.78,9.74,9.16,9.26,8.68,8.42,9.08,8.2,9.65,10.1,10.85,10,9.52,10.66,9.02,9.36,10.4,7.82,9.64,9.56,9.1,8.76,10.18,9.42,7.36,10.06,8.34,9.5,9.08,8.66,9.46,10.04,9.06,9.12,10.16,9.62,9.02,8.9,9.6,8.6,9.08,7.72,8.84,8.06,8.7,8.08,7.8,6.8,7.56,9.04,7.26,9.18)

CWI_core = c(8.189,8.681,7.032666667,9.504,7.64,8.34,6.5,7.1,7.8,7.34,7.966666667,7.68,6.68,7,7.6,7.04,8.12,8.3,6.64,8.44,7.55,6.15,7.55,8.02,7.5,8.28,7.975,7.125,8.066666667,6.933333333,7.4,7.34,7.534666667,8.525,6.8,7.575,7.533333333,7.775,7.6,8.5,6.78,11.43333333,7.68,8.918666666,8.225,6.92,7.766666667,7.22,6.02,7.26,8.696,7.672,7.325,7.4,7.18,8,6.86,8.575,7,7.28,7.08,7.925,6.4,6.6,7.1,6.8,6.98,6.82,7.76,7.64,7.125,8.38,6.68,7.78,10.18,8.62,8.64,8.92,8.74,9.52,9.46,7.72,8,9.68,8.74,7.34,8.7,5.88,8.1,7.264,8.84,9.2,9.26,8.42,9.52,10.66,8.76,10.04,8.9,8.6,8.06,8.7)

res.CWI = wilcox.test(CWI_whole , CWI_core)

res.CWI



# HIN

attach(data_whole_core)
glm.HIN <- glm(HIN ~ Type , family = "binomial")
glm.HIN <- glm(HIN ~ Type , family = "quasibinomial")

anova(glm.HIN, test = "Chisq")


# CNLA

glm.CNLA <- glm(CNLA ~ Type, family = poisson())
glm.CNLA <- glm(CNLA ~ Type, family = quasipoisson(link = "log")) 
anova(glm.CNLA, test = "Chisq")


#NLC


glm.NLC <- glm(NLC ~ Type, family = poisson())
glm.NLC <- glm(NLC ~ Type, family = quasipoisson(link = "log"))

anova(glm.NLC, test = "Chisq")


#### Repeatablitity


library(dlookr)

normality(data_acp[2:17])

plot_normality(data_acp[2:17])


library(rptR)


#### Data

data_acp= read.csv("ACP.csv", h=T, sep = ",")
data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
View(data_acp)

names(data_acp)
names(data_qr)
phe_rep = rpt(Plant_height ~ Accession , grname = "Accession", 
          data = data_qr, datatype = "Poisson", 
          nboot = 1000, npermut = 0)

phe_rep


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

### Normality and descriptive statistics of oil data

### Package

library(dlookr)

### Descriptive statistics

descriptive_stats = describe(data_pca[1:8])


write.csv(descriptive_stats, "descriptives.csv")

### Normality test


normality_test = normality(data_pca[1:8])


write.csv(normality_test, "normality_test.csv")



### Analysis of variance for normal distributed data


data_aov = read.csv("acp_oil_agro_core.csv", h=T)

names(data_aov)

## Palmitic acid

model_palm = glm(Palmitic_acid ~ Accession, data = data_aov)

model_palm

anova(model_palm)


### Repetability 


library(rptR)

rpt_palm <- rptGaussian(Palmitic_acid ~ (1|Accession), grname="Accession", 
                       data=data_aov, nboot=3, npermut=3, ratio = FALSE)



# I got this

# number of levels of each grouping factor must be < number of observations (problems: Accession)
# So It is not good to perform analysis of variance or repetability whenthe factor modality > number of observation.

# So just the PCA is enough
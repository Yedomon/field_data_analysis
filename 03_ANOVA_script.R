##--Script for boxplot
##--By Yedomon Ange Bovys Zoclanclounon | twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Science | Department of Genomics | Department of Genomics |RDA | Republic of South Korea


### Libraries

library(lmerTest)


### Data importation

data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
data_nqr1 = read.csv("data_nqr1.csv", h=T)
data_capsule_length_width = read.csv("capsule_length_and_width.csv", h=T)


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


### The following traits are analyzed following the environment only since the data was collected at unit plot level

### Dried_Biomass

library(dplyr) # Data manipulation


dried_biomass_jeonju = data_nqr1 %>%
  select(Dried_Biomass, Site) %>%
  filter(Site == 'Jeonju')


dried_biomass_miryang = data_nqr1 %>%
  select(Dried_Biomass, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(dried_biomass_jeonju$Dried_Biomass),na.omit(dried_biomass_miryang$Dried_Biomass))


### Dried_Seed_Weight



Dried_Seed_Weight_jeonju = data_nqr1 %>%
  select(Dried_Seed_Weight, Site) %>%
  filter(Site == 'Jeonju')


Dried_Seed_Weight_miryang = data_nqr1 %>%
  select(Dried_Seed_Weight, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Dried_Seed_Weight_jeonju$Dried_Seed_Weight),na.omit(Dried_Seed_Weight_miryang$Dried_Seed_Weight))





### Harvest_Index


Harvest_Index_jeonju = data_nqr1 %>%
  select(Harvest_Index, Site) %>%
  filter(Site == 'Jeonju')


Harvest_Index_miryang = data_nqr1 %>%
  select(Harvest_Index, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Harvest_Index_jeonju$Harvest_Index),na.omit(Harvest_Index_miryang$Harvest_Index))



### Thousand_Seed_Weight



Thousand_Seed_Weight_jeonju = data_nqr1 %>%
  select(Thousand_Seed_Weight, Site) %>%
  filter(Site == 'Jeonju')


Thousand_Seed_Weight_miryang = data_nqr1 %>%
  select(Thousand_Seed_Weight, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Thousand_Seed_Weight_jeonju$Thousand_Seed_Weight),na.omit(Thousand_Seed_Weight_miryang$Thousand_Seed_Weight))


### Flowering
Flowering_jeonju = data_nqr1 %>%
  select(Flowering, Site) %>%
  filter(Site == 'Jeonju')


Flowering_miryang = data_nqr1 %>%
  select(Flowering, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Flowering_jeonju$Flowering),na.omit(Flowering_miryang$Flowering))


### Maturity
Maturity_jeonju = data_nqr1 %>%
  select(Maturity, Site) %>%
  filter(Site == 'Jeonju')


Maturity_miryang = data_nqr1 %>%
  select(Maturity, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Maturity_jeonju$Maturity),na.omit(Maturity_miryang$Maturity))


### Flowering_to_Maturity

Flowering_to_Maturity_jeonju = data_nqr1 %>%
  select(Flowering_to_Maturity, Site) %>%
  filter(Site == 'Jeonju')


Flowering_to_Maturity_miryang = data_nqr1 %>%
  select(Flowering_to_Maturity, Site) %>%
  filter(Site == 'Miryang')


wilcox.test(na.omit(Flowering_to_Maturity_jeonju$Flowering_to_Maturity),na.omit(Flowering_to_Maturity_miryang$Flowering_to_Maturity))



























### Try ggstatsplot package to get images

library(ggstatsplot)
library(ggplot2)


### Dried_Biomass

DBI = data_nqr1 %>% 
  select(Accession, Dried_Biomass, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Dried_Biomass,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = "(A)"
  ) +
  xlab("Environment") + ylab("Dried biomass (g)") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


DBI


### Dried_Seed_Weight

DSW = data_nqr1 %>% 
  select(Accession, Dried_Seed_Weight, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Dried_Seed_Weight,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (B)"
  ) +
  xlab("Environment") + ylab("Dried seed weight (g)") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


DSW



### Harvest_Index


HI = data_nqr1 %>% 
  select(Accession, Harvest_Index, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Harvest_Index,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (C)"
  ) +
  xlab("Environment") + ylab("Harvest index") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


HI



### Thousand_Seed_Weight

TSW = data_nqr1 %>% 
  select(Accession, Thousand_Seed_Weight, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Thousand_Seed_Weight,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (D)"
  ) +
  xlab("Environment") + ylab("Thousand Seed Weight (g)") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


TSW


### Flowering

FLO = data_nqr1 %>% 
  select(Accession, Flowering, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Flowering,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (E)"
  ) +
  xlab("Environment") + ylab("Number of days to 50% flowering") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


FLO

### Maturity


MAT = data_nqr1 %>% 
  select(Accession, Maturity, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Maturity,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (E)"
  ) +
  xlab("Environment") + ylab("Number of days to 50% maturity") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


MAT


### Flowering_to_Maturity

FMAT = data_nqr1 %>% 
  select(Accession, Flowering_to_Maturity, Site)%>% 
  ggstatsplot::ggbetweenstats(
    x = Site,
    y = Flowering_to_Maturity,
    messages = TRUE,
    type = "np", # wILCOXON TEST
    outlier.tagging = TRUE, 
    outlier.label = Accession,
    title = " (E)"
  ) +
  xlab("Environment") + ylab("Number of days from flowering to maturity") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#581845"))


FMAT



#How to impute using machine learning


library(missRanger)

data_nqr1_imputed <- missRanger(
  data_nqr1, 
  formula = . ~ . ,
  num.trees = 1000, 
  verbose = 1, seed = 111)


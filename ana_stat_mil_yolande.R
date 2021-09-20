library(ggplot2)
library(lme4)
library(languageR)
library(lmerTest)
library(car)

getwd()
setwd("C:/Users/HP/Desktop/Dossier Sript R pour premier chapitre")

###setwd("D:/4_Theses/Thèse Yolande/paper1/v1/")

dy <- read.table("AnaMilNiebeV3.csv", sep=";", header=T)
dy
View(dy)
### POUR LE MIL

# Datacooking  -------
# Irrigated ------------
# data cooking 
dy_ir <- dy[dy$IrrigueON == "VRAI",]
dy_ir <- dy_ir[grepl("d2", dy_ir$trait) | grepl("m1_", dy_ir$trait),] # keep only d2 # but here you loose the sole millet 
dy_ir <- dy_ir[dy_ir$sp == "mil",] # keep only millet
dy_ir
nrow(dy_ir)

dy_ir$sc <- ifelse(grepl("m1_", dy_ir$trait), "m", 
                   ifelse(grepl("m1n1", dy_ir$trait), "mn1", "mn2"))
dy_ir$sc
# need to create rep(fert)
dy_ir$rep_fert <- paste0(dy_ir$codeFert,"_", dy_ir$rep)
dy_ir$rep_fert_year <- paste0(dy_ir$codeFert,"_", dy_ir$rep, "_", dy_ir$ansemis)


# Rainfed -----------

# data cooking 
dy_rf <- dy[dy$IrrigueON == "FAUX",]
dy_rf <- dy_rf[grepl("d2", dy_rf$trait) | grepl("m1_", dy_rf$trait),] # keep only d2 # but here you loose the sole millet 
dy_rf <- dy_rf[dy_rf$sp == "mil",] # keep only millet

nrow(dy_rf)

dy_rf$sc <- ifelse(grepl("m1_", dy_rf$trait), "m", 
                   ifelse(grepl("m1n1", dy_rf$trait), "mn1", "mn2"))


# need to create rep(fert)
dy_rf$rep_fert <- paste0(dy_rf$codeFert,"_", dy_rf$rep)
dy_rf$rep_fert_year <- paste0(dy_rf$codeFert,"_", dy_rf$rep, "_", dy_rf$ansemis)


# ##########Descriptive plots ---------

# some explorations
# irrigated
# grain
par(mfrow=c(1,3))
boxplot(dy_ir$rdtg ~ dy_ir$sc)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$ansemis)
### grain mil pour les facteurs qui sont significatifs
par(mfrow=c(1,3))
boxplot(dy_ir$rdtg ~ dy_ir$sc)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert*dy_ir$ansemis)
## GRAINS MIL
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert*dy_ir$ansemis)

# biomass
par(mfrow=c(1,3))
boxplot(dy_ir$Biotot ~ dy_ir$sc)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$ansemis)

# LAI
par(mfrow=c(1,3))
boxplot(dy_ir$laiRD ~ dy_ir$sc)
boxplot(dy_ir$laiRD ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$laiRD ~ dy_ir$sc*dy_ir$ansemis)

# rainfed
# grain
par(mfrow=c(1,3))
boxplot(dy_rf$rdtg ~ dy_rf$sc)
boxplot(dy_rf$rdtg ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$rdtg ~ dy_rf$sc*dy_rf$ansemis)

# biomass
par(mfrow=c(1,3))
boxplot(dy_rf$Biotot ~ dy_rf$sc)
boxplot(dy_rf$Biotot ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$Biotot ~ dy_rf$sc*dy_rf$ansemis)

# LAI
par(mfrow=c(1,3))
boxplot(dy_rf$laiRD ~ dy_rf$sc)
boxplot(dy_rf$laiRD ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$laiRD ~ dy_rf$sc*dy_rf$ansemis)


# #####Statistical analysis -----
# Irrigated --------
# Grain yield -------

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(rdtg ~  sc +
                 codeFert + 
                 ansemis +
                 codeFert:sc  + 
                 ansemis:sc + 
                 ansemis:codeFert+
                 ansemis:codeFert:sc +
                 rep_fert +
                 (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(rdtg ~  sc +
                 codeFert + 
                 ansemis +
                 codeFert:sc  + 
                 ansemis:sc + 
                 ansemis:codeFert+
                 ansemis:codeFert:sc +
                 rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(rdtg ~  sc +
               codeFert + 
               ansemis +
               codeFert:sc  + 
               ansemis:sc + 
               ansemis:codeFert+
               ansemis:codeFert:sc +
               rep_fert+
              (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 rep_fert_year +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert is better

# test fixed effects (experimental treatments)

model_full <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 rep_fert+
                                 (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_grain_yield <- anova(model_full, type="II") # this is type II
summary_grain_yield

summary_grain_yield <- as.data.frame(summary_grain_yield)
summary_grain_yield
summary_grain_yield$factor <- rownames(summary_grain_yield)
summary_grain_yield
summary_grain_yield$variable <- "Grain yield"
summary_grain_yield

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
OU

emmeans::lsmeans(model_full,"sc")
### interaction 

multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# representation graphique
library(readr)
library(multcompView)
library(dplyr)

radon_summary <-read_csv("data_summary.csv")
radon_summary
# grain
par(mfrow=c(1,3))
boxplot(dy_ir$rdtg ~ dy_ir$sc)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$ansemis)

# biomass : 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(Biotot ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(Biotot ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(Biotot ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(Biotot ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(Biotot ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(Biotot ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_biomass_yield <- anova(model_full, type="II") # this is type II
summary_biomass_yield
summary_biomass_yield <- as.data.frame(summary_biomass_yield)
summary_biomass_yield
summary_biomass_yield$factor <- rownames(summary_biomass_yield)
summary_biomass_yield$factor
summary_biomass_yield$variable <- "AGB"
summary_biomass_yield$variable
###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# LAI 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(laiRD ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(laiRD ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is greater - no need to include random, but let's keep it for consistency


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(laiRD ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(laiRD ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(laiRD ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(laiRD ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_LAI_yield <- anova(model_full, type="II") # this is type II 
summary_LAI_yield 
summary_LAI_yield <- as.data.frame(summary_LAI_yield)
summary_LAI_yield 
summary_LAI_yield$factor <- rownames(summary_LAI_yield)
summary_LAI_yield$variable <- "maximum LAI"


summary_all_irr <- rbind(summary_biomass_yield, summary_LAI_yield, summary_grain_yield)
summary_all_irr$irrigation <- "yes"

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)
OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# rainfed -------

# Grain yield -------

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(rdtg ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(rdtg ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(rdtg ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(rdtg ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(rdtg ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_grain_yield <- anova(model_full, type="II") # this is type II
summary_grain_yield 
summary_grain_yield <- as.data.frame(summary_grain_yield)

summary_grain_yield$factor <- rownames(summary_grain_yield)
summary_grain_yield$variable <- "Grain yield"

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# biomass : 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(Biotot ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(Biotot ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(Biotot ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(Biotot ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(Biotot ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(Biotot ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_biomass_yield <- anova(model_full, type="II") # this is type II
summary_biomass_yield
summary_biomass_yield <- as.data.frame(summary_biomass_yield)
summary_biomass_yield
summary_biomass_yield$factor <- rownames(summary_biomass_yield)
summary_biomass_yield$factor
summary_biomass_yield$variable <- "AGB"
summary_biomass_yield$variable
###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# LAI 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(laiRD ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(laiRD ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is greater - no need to include random, but let's keep it for consistency


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(laiRD ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(laiRD ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(laiRD ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(laiRD ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_LAI_yield <- anova(model_full, type="II") # this is type II 
summary_LAI_yield
summary_LAI_yield <- as.data.frame(summary_LAI_yield)
summary_LAI_yield
summary_LAI_yield$factor <- rownames(summary_LAI_yield)
summary_LAI_yield$variable <- "maximum LAI"


summary_all_rf <- rbind(summary_biomass_yield, summary_LAI_yield, summary_grain_yield)
summary_all_rf$irrigation <- "no"

summary_all <- rbind(summary_all_irr, summary_all_rf)

write.table(summary_all, "summary_all.csv", sep=";", row.names=F)

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)
OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


######################## pour LE NIEBE #############################

library(ggplot2)
library(lme4)
library(languageR)
library(lmerTest)
library(car)

getwd()
setwd("C:/Users/HP/Desktop/Dossier Sript R pour premier chapitre")

dy <- read.table("AnaMilNiebeV3_1.csv", sep=";", header=T)
dy
View(dy)
# Datacooking  -------
# Irrigated ------------
# data cooking 
dy_ir <- dy[dy$IrrigueON == "VRAI",]
#dy_ir <- dy_ir[grepl("d2", dy_ir$trait) | grepl("m1_", dy_ir$trait),] # keep only d2 # but here you loose the sole millet 
#dy_ir <- dy_ir[dy_ir$sp == "niebe",] # keep only millet
dy_ir
nrow(dy_ir)


#dy_ir$sc <- ifelse(grepl("m1_", dy_ir$trait), "m", 
                  # ifelse(grepl("m1n1", dy_ir$trait), "mn1", "mn2"))
dy_ir$sc
# need to create rep(fert)
dy_ir$rep_fert <- paste0(dy_ir$codeFert,"_", dy_ir$rep)
dy_ir$rep_fert_year <- paste0(dy_ir$codeFert,"_", dy_ir$rep, "_", dy_ir$ansemis)



# Rainfed -----------

# data cooking 
dy_rf <- dy[dy$IrrigueON == "FAUX",]
#dy_rf <- dy_rf[grepl("d2", dy_rf$trait) | grepl("m1n1_", dy_rf$trait)| grepl("m1n2_", dy_rf$trait)| grepl("n1_", dy_rf$trait)| grepl("n2_", dy_rf$trait),] # keep only d2 # but here you loose the sole millet 
#dy_rf <- dy_rf[dy_rf$sp == "niebe",] # keep only millet
dy_rf
nrow(dy_rf)

#dy_rf$sc <- ifelse(grepl("m1_", dy_rf$trait), "m", 
                   ifelse(grepl("m1n1", dy_rf$trait), "mn1", "mn2"))
dy_rf$sc

# need to create rep(fert)
dy_rf$rep_fert <- paste0(dy_rf$codeFert,"_", dy_rf$rep)
dy_rf$rep_fert_year <- paste0(dy_rf$codeFert,"_", dy_rf$rep, "_", dy_rf$ansemis)




# Descriptive plots ---------

# some explorations
# irrigated
# grain
dy_ir$rdtg <- as.numeric(paste(dy_ir$rdtg))

par(mfrow=c(1,3))
boxplot(dy_ir$rdtg ~ dy_ir$sc)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$ansemis)
### grain mil pour les facteurs qui sont significatifs
par(mfrow=c(1,3))
boxplot(dy_ir$rdtg ~ dy_ir$sc)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert*dy_ir$ansemis)
## GRAINS MIL
boxplot(dy_ir$rdtg ~ dy_ir$sc*dy_ir$codeFert*dy_ir$ansemis)

# biomass
par(mfrow=c(1,3))
boxplot(dy_ir$Biotot ~ dy_ir$sc)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$ansemis)
### Biot mil pour les facteurs qui sont significatifs
par(mfrow=c(1,3))
boxplot(dy_ir$Biotot ~ dy_ir$sc)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$Biotot ~ dy_ir$sc*dy_ir$codeFert*dy_ir$ansemis)

# LAI
par(mfrow=c(1,3))
boxplot(dy_ir$laiRD ~ dy_ir$sc)
boxplot(dy_ir$laiRD ~ dy_ir$sc*dy_ir$codeFert)
boxplot(dy_ir$laiRD ~ dy_ir$sc*dy_ir$ansemis)

# rainfed
# grain facteur significatif
par(mfrow=c(1,2))
boxplot(dy_rf$rdtg ~ dy_rf$sc)
boxplot(dy_rf$rdtg ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$rdtg ~ dy_rf$sc*dy_rf$ansemis)

# biomass
par(mfrow=c(1,3))
boxplot(dy_rf$Biotot ~ dy_rf$sc)
boxplot(dy_rf$Biotot ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$Biotot ~ dy_rf$sc*dy_rf$ansemis)

# LAI
par(mfrow=c(1,3))
boxplot(dy_rf$laiRD ~ dy_rf$sc)
boxplot(dy_rf$laiRD ~ dy_rf$sc*dy_rf$codeFert)
boxplot(dy_rf$laiRD ~ dy_rf$sc*dy_rf$ansemis)





# Statistical analysis -----
# Irrigated --------
# Grain yield -------

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(rdtg ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(rdtg ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(rdtg ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(rdtg ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(rdtg ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_grain_yield <- anova(model_full, type="II") # this is type II
summary_grain_yield

summary_grain_yield <- as.data.frame(summary_grain_yield)
summary_grain_yield
summary_grain_yield$factor <- rownames(summary_grain_yield)
summary_grain_yield
summary_grain_yield$variable <- "Grain yield"
summary_grain_yield

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)
# biomass : 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(Biotot ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(Biotot ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(Biotot ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(Biotot ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(Biotot ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(Biotot ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_biomass_yield <- anova(model_full, type="II") # this is type II
summary_biomass_yield
summary_biomass_yield <- as.data.frame(summary_biomass_yield)
summary_biomass_yield
summary_biomass_yield$factor <- rownames(summary_biomass_yield)
summary_biomass_yield$factor
summary_biomass_yield$variable <- "Biotot"
summary_biomass_yield$variable

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"codeFert"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)

###pairs(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)

# LAI 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(laiRD ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_ir, REML = TRUE)

model_without_random <- lm(laiRD ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_ir)

AIC(model_random, model_without_random) # AIC of random model is greater - no need to include random, but let's keep it for consistency


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(laiRD ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_ir, REML=FALSE)


model_without_rep_fert <- lmer(laiRD ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_ir, REML=FALSE)

model_with_rep_fert_year <- lmer(laiRD ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_ir, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(laiRD ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_ir, REML=TRUE)


summary_LAI_yield <- anova(model_full, type="II") # this is type II 
summary_LAI_yield
summary_LAI_yield <- as.data.frame(summary_LAI_yield)
summary_LAI_yield
summary_LAI_yield$factor <- rownames(summary_LAI_yield)
summary_LAI_yield$factor
summary_LAI_yield$variable <- "laiRD"
summary_LAI_yield$variable

summary_all_irr <- rbind(summary_biomass_yield, summary_LAI_yield, summary_grain_yield)### obtenir le résumé de l'analyse de variance de toutes les variables
summary_all_irr
summary_all_irr$irrigation <- "yes"
summary_all_irr$irrigation

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)

# rainfed -------

# Grain yield -------

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(rdtg ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(rdtg ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(rdtg ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(rdtg ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(rdtg ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(rdtg ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_grain_yield <- anova(model_full, type="II") # this is type II
summary_grain_yield
summary_grain_yield <- as.data.frame(summary_grain_yield)
summary_grain_yield
summary_grain_yield$factor <- rownames(summary_grain_yield)
summary_grain_yield$factor
summary_grain_yield$variable <- "Grain yield"
summary_grain_yield$variable

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,"ansemis"), Letters = letters)
OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# biomass : 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(Biotot ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(Biotot ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is lower - keep random effect


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(Biotot ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(Biotot ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(Biotot ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert_year is better

# test fixed effects (experimental treatments)

model_full <- lmer(Biotot ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert_year+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_biomass_yield <- anova(model_full, type="II") # this is type II
summary_biomass_yield
summary_biomass_yield <- as.data.frame(summary_biomass_yield)
summary_biomass_yield
summary_biomass_yield$factor <- rownames(summary_biomass_yield)
summary_biomass_yield$factor
summary_biomass_yield$variable <- "AGB"
summary_biomass_yield$variable

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)


# LAI 

# test if idgeo2 is meaningful as a random factor
model_random <- lmer(laiRD ~  sc +
                       codeFert + 
                       ansemis +
                       codeFert:sc  + 
                       ansemis:sc + 
                       ansemis:codeFert+
                       ansemis:codeFert:sc +
                       rep_fert +
                       (1|idgeo2),  data= dy_rf, REML = TRUE)

model_without_random <- lm(laiRD ~  sc +
                             codeFert + 
                             ansemis +
                             codeFert:sc  + 
                             ansemis:sc + 
                             ansemis:codeFert+
                             ansemis:codeFert:sc +
                             rep_fert ,  data= dy_rf)

AIC(model_random, model_without_random) # AIC of random model is greater - no need to include random, but let's keep it for consistency


# test if rep_fert and rep_fert_year need to be added to the model
model_rep_fert <- lmer(laiRD ~  sc +
                         codeFert + 
                         ansemis +
                         codeFert:sc  + 
                         ansemis:sc + 
                         ansemis:codeFert+
                         ansemis:codeFert:sc +
                         rep_fert+
                         (1|idgeo2),  data= dy_rf, REML=FALSE)


model_without_rep_fert <- lmer(laiRD ~  sc +
                                 codeFert + 
                                 ansemis +
                                 codeFert:sc  + 
                                 ansemis:sc + 
                                 ansemis:codeFert+
                                 ansemis:codeFert:sc +
                                 (1|idgeo2),  data= dy_rf, REML=FALSE)

model_with_rep_fert_year <- lmer(laiRD ~  sc +
                                   codeFert + 
                                   ansemis +
                                   codeFert:sc  + 
                                   ansemis:sc + 
                                   ansemis:codeFert+
                                   ansemis:codeFert:sc +
                                   rep_fert_year +
                                   (1|idgeo2),  data= dy_rf, REML=FALSE)

anova(model_rep_fert, model_with_rep_fert_year, model_without_rep_fert) # model with rep_fert is better

# test fixed effects (experimental treatments)

model_full <- lmer(laiRD ~  sc +
                     codeFert + 
                     ansemis +
                     codeFert:sc  + 
                     ansemis:sc + 
                     ansemis:codeFert+
                     ansemis:codeFert:sc +
                     rep_fert+
                     (1|idgeo2),  data= dy_rf, REML=TRUE)


summary_LAI_yield <- anova(model_full, type="II") # this is type II 
summary_LAI_yield
summary_LAI_yield <- as.data.frame(summary_LAI_yield)
summary_LAI_yield
summary_LAI_yield$factor <- rownames(summary_LAI_yield)
summary_LAI_yield$factor
summary_LAI_yield$variable <- "maximum LAI"
summary_LAI_yield$variable

summary_all_rf <- rbind(summary_biomass_yield, summary_LAI_yield, summary_grain_yield)
summary_all_rf
summary_all_rf$irrigation <- "FAUX"
summary_all_rf$irrigation

summary_all <- rbind(summary_all_irr, summary_all_rf)

write.table(summary_all, "summary_all.csv", sep=";", row.names=F)

###calcules des moyennes ajustées SUIVANT les types de FACTEURS qu'on a par Lsmeans

multcomp::cld(emmeans::lsmeans(model_full,"sc"), Letters = letters)

OU

emmeans::lsmeans(model_full,"sc")
### interaction 
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ codeFert *ansemis ),Letters = letters)
multcomp::cld(emmeans::lsmeans(model_full,pairwise ~ sc *codeFert *ansemis ),Letters = letters)











































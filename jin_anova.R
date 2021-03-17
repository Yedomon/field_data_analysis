# Package loading 

library(agricolae)


# Data importation

data_y = read.csv("YS3386.csv",sep = "," , h = T)
names(data_y)

# ANOVA

model<-aov(Height~Type, data=data_y)
anova(model)
LSD.test(model, "Type", group=FALSE, p.adj= "bon",console=TRUE)

model<-aov(Diameter~Type, data=data_y)
anova(model)
LSD.test(model, "Type", group=FALSE, p.adj= "bon",console=TRUE)

model<-aov(Leaf_length~Type, data=data_y)
anova(model)
LSD.test(model, "Type", group=FALSE, p.adj= "bon",console=TRUE)

model<-aov(Leaf_wide~Type, data=data_y)
anova(model)
LSD.test(model, "Type", group=FALSE, p.adj= "bon",console=TRUE)


model<-aov(Chlorophyll_content~Type, data=data_y)
anova(model)
LSD.test(model, "Type", group=FALSE, p.adj= "bon",console=TRUE)






# Drawing plot


# Data importation

# Plant height

data_h = read.csv("height.csv",sep = "," , h = T)


# plot

library(ggplot2)
library(ggsignif)

h = ggplot(data_h, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(x = "Treatment", y = "Plant height (cm)")+
  geom_errorbar(aes(ymin = Mean-sd, ymax = Mean + sd), width = 0.2)+
  geom_signif(comparisons = list(c("Control", "Infected")), annotations="***", y_position = 10, tip_length = 0.03) +
  theme_bw()+
  theme(axis.text = element_text(colour = "black", face = "bold"), 
        axis.title = element_text(colour = "black", face = "bold") ) +
  ggtitle('A')+
  scale_fill_manual(values=c("#0E6251", "#FF5733"))

h



# Diameter

# Diameter

data_d = read.csv("diameter.csv",sep = "," , h = T)


d = ggplot(data_d, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(x = "Treatment", y = "Stem diameter (mm)")+
  geom_errorbar(aes(ymin = Mean-sd, ymax = Mean + sd), width = 0.2)+
  geom_signif(comparisons = list(c("Control", "Infected")), annotations="***", y_position = 3.5, tip_length = 0.03) +
  theme_bw()+
  theme(axis.text = element_text(colour = "black", face = "bold"), 
        axis.title = element_text(colour = "black", face = "bold") ) +
  ggtitle('B')+
  scale_fill_manual(values=c("#0E6251", "#FF5733"))


d




# Leaf length

data_ll = read.csv("leaf_length.csv",sep = "," , h = T)


ll = ggplot(data_ll, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(x = "Treatment", y = "Leaf length (cm)")+
  geom_errorbar(aes(ymin = Mean-sd, ymax = Mean + sd), width = 0.2)+
  geom_signif(comparisons = list(c("Control", "Infected")), annotations="***", y_position = 5.7, tip_length = 0.03) +
  theme_bw()+
  theme(axis.text = element_text(colour = "black", face = "bold"), 
        axis.title = element_text(colour = "black", face = "bold") ) +
  ggtitle('C')+
  scale_fill_manual(values=c("#0E6251", "#FF5733"))

ll







# Leaf width

data_lw = read.csv("leaf_width.csv",sep = "," , h = T)


lw = ggplot(data_lw, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(x = "Treatment", y = "Leaf width (cm)")+
  geom_errorbar(aes(ymin = Mean-sd, ymax = Mean + sd), width = 0.2)+
  geom_signif(comparisons = list(c("Control", "Infected")), annotations="***", y_position = 4, tip_length = 0.03) +
  theme_bw()+
  theme(axis.text = element_text(colour = "black", face = "bold"), 
        axis.title = element_text(colour = "black", face = "bold") ) +
  ggtitle('D')+
  scale_fill_manual(values=c("#0E6251", "#FF5733"))

lw




# chloro_content

data_cc = read.csv("chloro_content.csv",sep = "," , h = T)


cc = ggplot(data_cc, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(x = "Treatment", y = "Chlorophyll content")+
  geom_errorbar(aes(ymin = Mean-sd, ymax = Mean + sd), width = 0.2)+
  geom_signif(comparisons = list(c("Control", "Infected")), annotations="***", y_position = 35, tip_length = 0.03) +
  theme_bw()+
  theme(axis.text = element_text(colour = "black", face = "bold"), 
        axis.title = element_text(colour = "black", face = "bold") ) +
  ggtitle('E') +
  scale_fill_manual(values=c("#0E6251", "#FF5733"))

cc


# Group graphs 


library(patchwork)

(h | d | ll) /
  (lw | cc | cc)


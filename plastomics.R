#---Chloroplast assembler benchmarking


#---Library loading

library(ggplot2)
library(ggthemes)
library(scales)

#--Data importation

mydata = read.csv("comp.csv", h=T, sep = ",")

#--Chart 1: Plastome length

ggplot(mydata, aes(fill = Tool, y=Plastome_length, x=Tool)) + 
  geom_bar(stat="identity") +
  geom_label(aes(x = Tool, y = Plastome_length, label = Plastome_length, hjust = 1.5), size = 6, colour = "white")+
  labs(y = "Plastome length (bp)") +
  facet_wrap(~Species) +
  coord_flip()+
  theme_bw() +
  theme(
  axis.text = element_text(colour = "black"),
  axis.title.x = element_text(color = "black", size = 10, face = "bold"),
  axis.title.y = element_text(color = "black", size = 10, face = "bold")
       ) +
  scale_fill_manual(values = c("#4d4dff", "#ff4d4d")) +
  guides(
    fill = guide_legend(
      title = "Organelle assembler tool",
      override.aes = aes(label = "")
    )
   ) +
  theme(legend.position = "top")


#--Chart 2: IR Length

ggplot(mydata, aes(fill = Tool, y=IR_length, x=Tool)) + 
  geom_bar(stat="identity")+
  geom_text(aes(x = Tool, y = IR_length, label = IR_length, hjust = 1.5))+
  labs(y = "Inverted repeat length (bp)") +
  facet_wrap(~Species) +
  coord_flip()+
  theme_bw() 

#--Chart 3: SSC length

ggplot(mydata, aes(fill = Tool, y=SSC_length, x=Tool)) + 
  geom_bar(stat="identity")+
  geom_text(aes(x = Tool, y = SSC_length, label = SSC_length, hjust = 1.5))+
  labs(y = "Short single copy length (bp)") +
  facet_wrap(~Species) +
  coord_flip()+
  theme_bw() 

#--Chart 4: LSC length


ggplot(mydata, aes(fill = Tool, y=LSC_length, x=Tool)) + 
  geom_bar(stat="identity")+
  geom_text(aes(x = Tool, y = LSC_length, label = LSC_length, hjust = 1.5))+
  labs(y = "Long single copy length (bp)") +
  facet_wrap(~Species) +
  coord_flip()+
  theme_bw() 

#---Chart 5: Whole plastome with LSC, SSC and IR length

#--Data importation

mydata1 = read.csv("comp.whole.csv", h=T, sep = ",")


ggplot(mydata1, aes(fill = Part, y= Length, x= Tool, label = Length)) +
  geom_bar(position="stack", stat="identity")+
  geom_label(size = 5, position = position_stack(vjust = 0.5), color = "white")+
  labs(y = "Length (bp)") +
  facet_wrap(~Species) +
  coord_flip()+
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 10, face = "bold")
  )+
  scale_fill_manual(values = c("#4d4dff", "#ff4d4d","#00b35a")) +
  guides(
  fill = guide_legend(
    title = "Plastome parts",
    override.aes = aes(label = "")
   )
  ) +
scale_y_continuous(labels = comma) +
theme(legend.position = "top")
  




 





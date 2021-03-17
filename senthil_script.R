### Figure Review India

##### Package loading

library(ggplot2)
library(ggrepel)
library(bbplot)
library(patchwork)

##### Theme setting

theme_set(
  theme_bw() +
    theme(legend.position = "right")
)



### Funding

data_funding = read.csv("funding_abb.csv",sep = "," , h = T)

names(data_funding)



plot0 = ggplot(data_funding, aes(x = Year, y = No_of_Funding_Resources)) +
  #geom_point(aes(shape = Field))+#, color = Sequencing_Technology)) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#9a004d","#8b709e","#00d68f","#b00b1e", "#bf4040"))+ 
  #"#00d68f","#b00b1e","#9a004d","#8b709e","#00d68f","#b00b1e",
  #"#0e2f44","#ff00ff","#0033cc"," #00ff00","#ff5500","#0080ff","#bf4040")) +
  geom_point(aes(color = Field, size = No_of_Funding_Resources))+#, alpha = 0.5) +
  geom_text_repel(aes(label = Field, size = 3), show.legend  = F )+   #   ,  color = cyl), size = 3)+
  #scale_size(range = c(0.5, 12)) +
  labs(y = "No of Funding Resources", x = "Year's", color = "Field")+
  scale_x_continuous(breaks = seq(2010, 2021, by = 1),
                                  labels = c("2010", "2011", "2012", "2013",
                                "2014", "2015", "2016", "2017",
                                "2018", "2019", "2020", "2021"))
  

plot0



##### Data importation

data = read.csv("review_india.csv",sep = "," , h = T)

names(data)


##### Plot

plot1 <- ggplot(data, aes(x = Year, y = Number_of_project)) +
  #geom_point(aes(shape = Sequencing_Technology, color = Sequencing_Technology)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#9a004d","#8b709e","#00d68f","#b00b1e", "#bf4040"))+ 
  #"#00d68f","#b00b1e","#9a004d","#8b709e","#00d68f","#b00b1e",
  #"#0e2f44","#ff00ff","#0033cc"," #00ff00","#ff5500","#0080ff","#bf4040")) +
  geom_point(aes(color = Field.of.study, size = Number_of_project), alpha = 0.5) +
  geom_text_repel(aes(label = Field.of.study, size = 3), show.legend  = F )+   #   ,  color = cyl), size = 3)+
  #scale_size(range = c(0.5, 12)) +
  labs(y = "Number of project", x = "Year", color = "Field.of.study")+
  bbc_style()+
  ggtitle("(A)")

plot1




### Plot 2

data_state = read.csv("state_by_year.csv",sep = "," , h = T)

names(data_state)

plot2 = ggplot(data_state, aes(x = reorder(State, Number), y = Number)) +
          geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
          labs(y = "Number of project", x = "States name") +
          coord_flip()+
          bbc_style()+
          ggtitle("(B)")

plot2


### Plot 3

data_year= read.csv("year_by_number.csv",sep = "," , h = T)

names(data_year)

plot3 = ggplot(data_year, aes(x = reorder(Year, Number), y = Number)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(y = "Number of project", x = "Year") +
  coord_flip()+
  bbc_style() +
  ggtitle("(C)")

plot3

### Plot 4


data_sci= read.csv("sci_by_number.csv",sep = "," , h = T)

names(data_sci)

plot4 = ggplot(data_sci, aes(x = reorder(Type, Number), y = Number)) +
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
  labs(y = "Number of project", x = "Citation") +
  coord_flip()+
  bbc_style() +
  ggtitle("(D)")

plot4





(plot1) /
  (plot2 | plot3 | plot4)


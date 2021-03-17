# Genome size estimation of Sesamum schinzianum

#load the data into dataframe19
dataframe19 <- read.table("Sesamum_schinzianum_19mer_histo") 

#plots the data points 1 through 200 in the dataframe19 using a line
plot(dataframe19[5:200,], type="l") 

#plot the data points from 5 through 100
points(dataframe19[5:100,]) 


#plots the data points 30 through 40 in the dataframe19 using a line
plot(dataframe19[30:40,], type="l") 

#plot the data points from 30 through 40
points(dataframe19[30:40,]) 

# after this zooming operation, I found graphically that v1 = 37 is the peak so the estimated genome size might be

sum(as.numeric(dataframe19[5:10001,1]*dataframe19[5:10001,2]))/37

# we got a size of 773328733 bp so 773 Mp

## ggplot2 style

library(ggplot2)
library(hrbrthemes)
dataggplot2 = dataframe19[5:200,]
ggplot(data=dataggplot2, aes(x=V1, y=V2)) +
  #geom_line
  geom_line(color="#69b3a2",size = 1) +
  #first peak
  geom_hline(yintercept=19692068, color="blue",linetype = "dotted", size=.5) +
  geom_vline(xintercept = 37, color="blue",linetype = "dotted", size=.5)+
  annotate(geom="text", x= 37, y=21000000, 
           label="1st peak \nat coverage=37")+
  annotate(geom="point", x=37 ,y=19800000, size=10, shape=21, fill="transparent") +
  #2nd peak
  geom_hline(yintercept=1935249, color="red",linetype = "dotted", size=.5) +
  geom_vline(xintercept = 75, color="red",linetype = "dotted", size=.5)+
  geom_line(color="#69b3a2") +
  annotate(geom="text", x= 75, y=3000000, 
           label="2nd peak \nat coverage=75")+
  annotate(geom="point", x=75 ,y=1950000, size=10, shape=21, fill="transparent") +
  # Genome size annotation
  annotate(geom="text", x= 100, y=19000000, 
           label="Estimated Genome Size \n 773,328,733 bp",
           face = "bold", size = 5)+
  # Labelling x and y axes
  labs(x = "Coverage", y = "K-mer frequency") +
  theme_ipsum()

#----------------------------------------------------------------------------------------------------

# Genome size estimation of Sesamum radiatum ghana ecotype

#load the data into dataframe19
dataframe19 <- read.table("Sesamum_radiatum_ghana_19mer_histo") 

str(dataframe19) # we have 10001 observation

View(dataframe19)

#plots the data points 1 through 200 in the dataframe19 using a line
plot(dataframe19[5:100,], type="l") 

#plot the data points from 5 through 100
points(dataframe19[5:100,]) 


#plots the data points 30 through 40 in the dataframe19 using a line
plot(dataframe19[50:60,], type="l") 

#plot the data points from 30 through 40
points(dataframe19[50:60,]) 

# after this zooming operation, I found graphically that v1 = 54 is the peak so the estimated genome size might be

sum(as.numeric(dataframe19[5:10000,1]*dataframe19[5:10000,2]))/54

# we got a size of 699367696 bp so 699 Mp

### ggplot style

## Inspiration https://www.r-graph-gallery.com/line_chart_annotation.html
## Inspiration annotation https://ggplot2-book.org/annotations.html
library(ggplot2)
library(hrbrthemes)
dataggplot2 = dataframe19[8:200,]
ggplot(data=dataggplot2, aes(x=V1, y=V2)) +
  #geom_line
  geom_line(color="#69b3a2",size = 1) +
  #first peak
  geom_hline(yintercept=16279453, color="blue",linetype = "dotted", size=.5) +
  geom_vline(xintercept = 54, color="blue",linetype = "dotted", size=.5)+
  annotate(geom="text", x= 54, y=17300000, 
           label="1st peak \nat coverage=54")+
  annotate(geom="point", x=54 ,y=16300000, size=10, shape=21, fill="transparent") +
  #2nd peak
  geom_hline(yintercept=1585011, color="red",linetype = "dotted", size=.5) +
  geom_vline(xintercept = 110, color="red",linetype = "dotted", size=.5)+
  geom_line(color="#69b3a2") +
  annotate(geom="text", x= 110, y=2800000, 
           label="2nd peak \nat coverage=110")+
  annotate(geom="point", x=110 ,y=1630000, size=10, shape=21, fill="transparent") +
  # Genome size annotation
  annotate(geom="text", x= 110, y=16279453, 
           label="Estimated Genome Size \n 699,367,696 bp",
           face = "bold", size = 5)+
  # Labelling x and y axes
  labs(x = "Coverage", y = "K-mer frequency") +
  theme_ipsum()


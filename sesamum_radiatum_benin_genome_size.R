# Genome size estimation of Sesamum schinzianum

#load the data into dataframe19
dataframe19 <- read.table("Sesamum_radiatum_Benin_19mer.histo") 

#plots the data points 1 through 200 in the dataframe19 using a line
plot(dataframe19[5:200,], type="l") 

#plot the data points from 5 through 100
points(dataframe19[5:100,]) 


#plots the data points 30 through 40 in the dataframe19 using a line
plot(dataframe19[5:100,], type="l") 

#plot the data points from 30 through 40
points(dataframe19[5:50,]) 

# after this zooming operation, I found graphically that v1 = 37 is the peak so the estimated genome size might be

sum(as.numeric(dataframe19[5:10001,1]*dataframe19[5:10001,2]))/37

# we got a size of 773328733 bp so 773 Mp
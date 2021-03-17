# Data importation

data =  read.csv("data_p1.csv", h=T, sep = ",")


# Attach data

attach(data)

# Names

names(data)


# t-test
res <- t.test(Height ~ Mother, data = data, var.equal = TRUE)
res

#--------------------------------------------Again


# step 1: Import your data

my_data = read.csv("data_p1.csv", header = TRUE, sep = ",")


# t-test
res2 <- t.test(Height ~ Mother, data = my_data, var.equal = TRUE)
res2

#---------------------------------------------------


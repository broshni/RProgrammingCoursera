## In the dataset provided for this Quiz, what are the column names of the dataset?
data_1 <- read.csv("hw1_data.csv")
names(data_1)

## How many observations (i.e. rows) are in this data frame?
nrow(data_1)

## Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
tail(data_1)

## What is the value of Ozone in the 47th row? 
data_1$Ozone[47]

## How many missing values are in the Ozone column of this data frame?
miss_Ozone <-is.na(data_1$Ozone)
ozone_m<-data_1$Ozone[miss_Ozone] 
length(ozone_m)

## What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
ozone_nom <- data_1$Ozone[!miss_Ozone]
mean(ozone_nom)

## Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
temp_o <- subset(data_1, Ozone>31)
temp_t<- subset(temp_o, Temp>90)
mean(temp_t$Solar.R)

## What is the mean of "Temp" when "Month" is equal to 6?
temp_m <- subset(data_1, Month==6)
mean(temp_m$Temp)

## What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
temp_may <- subset(data_1, Month == 5)
sort(temp_may$Ozone, decreasing = TRUE)




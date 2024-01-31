install.packages("tidyverse")
install.packages("ggplot2")
install.packages("shiny")
install.packages("caret")
install.packages("1071")
install.packages("plotly")
install.packages("tidyquant")

# Load Libraries 
library(ggplot2)
library(lubridate)
library(tidyverse)
library(moments)
library(data.table)

#Upload Dataframes here

q1_2019 <- read.csv("C:/Users/GBEMUDU .A. ANTHONY/Downloads/Divvy 2019/Divvy_Trips_2019_Q1.csv")
q2_2019 <- read.csv("C:/Users/GBEMUDU .A. ANTHONY/Downloads/Divvy 2019/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("C:/Users/GBEMUDU .A. ANTHONY/Downloads/Divvy 2019/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("C:/Users/GBEMUDU .A. ANTHONY/Downloads/Divvy 2019/Divvy_Trips_2019_Q4.csv")

# Wrangle Data

names(q1_2019)
names(q2_2019)
names(q3_2019)
names(q4_2019)

# Rename Columns on each table to ensure consistency

names(q1_2019) <- c("Trip_ID","Start_Time","End_time","Bike_Id","Trip_Duration_In_Seconds","Start_Station_ID","Rental_Station_Name","End_Station_ID","End_Station_Name","User_Type","Member_Gender","Memeber_BirthYear")
names(q2_2019) <- c("Trip_ID","Start_Time","End_time","Bike_Id","Trip_Duration_In_Seconds","Start_Station_ID","Rental_Station_Name","End_Station_ID","End_Station_Name","User_Type","Member_Gender","Memeber_BirthYear")
names(q3_2019) <- c("Trip_ID","Start_Time","End_time","Bike_Id","Trip_Duration_In_Seconds","Start_Station_ID","Rental_Station_Name","End_Station_ID","End_Station_Name","User_Type","Member_Gender","Memeber_BirthYear")
names(q4_2019) <- c("Trip_ID","Start_Time","End_time","Bike_Id","Trip_Duration_In_Seconds","Start_Station_ID","Rental_Station_Name","End_Station_ID","End_Station_Name","User_Type","Member_Gender","Memeber_BirthYear")

# Inspect Dataframe for Consistencies in Table Name and Datatypes

str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)

# Union of all dataframes into One

Divvy_trips_2019 <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)

#Data Cleaning

#Inspect new table

str(Divvy_trips_2019)    # View column names and datatype
names(Divvy_trips_2019)  # View only column names
nrow(Divvy_trips_2019)   # Number of rows in the data frame?
dim(Divvy_trips_2019)    # Dimensions(Number of rows by Number of columns) of the data frame?
head(Divvy_trips_2019)   # Shows the first 6 rows of data in the dataframe.
tail(Divvy_trips_2019)   # Shows the last 6 rows of data in the data frame.
summary(Divvy_trips_2019)# Shows Statistical summary of dataframe
view(Divvy_trips_2019)

#Check for Duplicates

duplicated(Divvy_trips_2019)

#Split the date from start_Time and convert to date data type as Start_Time column is a string column
Divvy_trips_2019$Start_date <- as.Date(Divvy_trips_2019$Start_Time) #The default date format in R is 'yyyy-mm-dd'

#Extract Month
Divvy_trips_2019$Start_month <- format(as.Date(Divvy_trips_2019$Start_date), "%b")

#Extract Day of the weekin words
Divvy_trips_2019$Start_day <- format(as.Date(Divvy_trips_2019$Start_date), "%a")

#Extract Year
Divvy_trips_2019$Start_Year <- format(as.Date(Divvy_trips_2019$Start_date), "%Y")

#Day of month in Numbers
Divvy_trips_2019$day_of_week <- format(as.Date(all_trips$date), "%d")

#Find Ride Lenght in Minutes
Divvy_trips_2019$Length_of_Ride <- difftime(Divvy_trips_2019$End_time,Divvy_trips_2019$Start_Time)

#Change Date type from character to Numeric
Divvy_trips_2019$Length_of_Ride <- as.numeric(as.character(Divvy_trips_2019$Length_of_Ride))

# Check datatype conversion
is.numeric(Divvy_trips_2019$Length_of_Ride)

summary(Divvy_trips_2019$Length_of_Ride)


 #Data Analysis

#Calculate Mean between the Two User types
aggregate(Divvy_trips_2019$Length_of_Ride ~ Divvy_trips_2019$User_Type, FUN = mean)

#Calculate Median between the two USer types
aggregate(Divvy_trips_2019$Length_of_Ride ~ Divvy_trips_2019$User_Type, FUN = median)

#Calculate the Min ride length between the two user types
aggregate(Divvy_trips_2019$Length_of_Ride ~ Divvy_trips_2019$User_Type, FUN = min)

#Calculate the Max ride length between the two user types
aggregate(Divvy_trips_2019$Length_of_Ride ~ Divvy_trips_2019$User_Type, FUN = max)


#Analayzing User Usage by Day of the week
User_Useage_by_DOW <- aggregate(Divvy_trips_2019$Length_of_Ride ~ Divvy_trips_2019$User_Type + Divvy_trips_2019$Start_day, FUN = mean)

#Export Result to file location
write.csv(User_Useage_by_DOW, file = 'C:/Users/GBEMUDU .A. ANTHONY/Downloads/Divvy 2019/avg_ride_length.csv')

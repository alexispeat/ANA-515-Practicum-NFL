#ANA 515 Practicum Assignment

#set working directory
setwd("C:\\Users\\cmbjp\\OneDrive\\Desktop\\ANA 515")

#check working directory
getwd()

#install readxl
install.packages("readxl")
library(readxl)

#reviewed nfl dataset in Excel first
#formatted 'date' column to include date values only rather than $ value

#read both pages of NFL predictions data using read_excel
nfl1 <- read_excel("C:\\Users\\cmbjp\\OneDrive\\Desktop\\ANA 515\\nfl_elo_latest.xlsx", sheet=1)
nfl2<- read_excel("C:\\Users\\cmbjp\\OneDrive\\Desktop\\ANA 515\\nfl_elo_latest.xlsx", sheet=2)

#combine the 2 excel sheets
nfltotal <- rbind(nfl1, nfl2)

#View new combined nfl dataset to understand what needs to be cleaned
View(nfltotal)

#check that all values in 'season are 2020'
#check structure of data
str(nfltotal$season)

#install and load lubridate
install.packages("lubridate")
library(lubridate)

#find rows where year is not 2020
non2020dates<-nfltotal[nfltotal$season != 2020, ]
View(non2020dates)

#correct the columns with values 2 or 20 to reflect 2020
nfltotal$season[nfltotal$season == 2] <- 2020
nfltotal$season[nfltotal$season == 20] <- 2020

#View data to ensure that these values were corrected
View(nfltotal)

#Run non2020dates again to ensure that these values were corrected
non2020dates<-nfltotal[nfltotal$season != 2020, ]
View(non2020dates) #no values showing, all values were corrected

#Replace NA values with "Unknown" in playoff column
nfltotal$playoff <- replace(nfltotal$playoff, is.na(nfltotal$playoff), "Unknown")
View(nfltotal)

#get number of variables in dataset
numvariables <- ncol(nfltotal)
print(numvariables)

#I have cleaned columns 1-4 individually
#Due to a lack of context for this dataset, I have chosen to replace all other NA values with "Unknown"
#Now I want to clean variables 5-30 all at the same time
#Replace NA values with "Unknown" in columns 5 to 30
nfltotal[5:30] <- lapply(nfltotal[5:30], function(x) replace(x, is.na(x), "Unknown"))

#I do not want to remove rows with NA because this removes many rows of the dataset

#create new dataset name for clean dataset
cleaned_nfltotal <- nfltotal
View(cleaned_nfltotal)

#Check if there are any NA values in columns 1 to 30
any(is.na(nfltotal[1:30])) #result is FALSE, there are no NA values left

#check for duplicate rows in dataset and remove them
cleaned_nfltotal <- cleaned_nfltotal[!duplicated(cleaned_nfltotal), ]

#check data types
sapply(cleaned_nfltotal, class)

#correct data type for variable 'date'
cleaned_nfltotal$date <- as.Date("2025-03-08")
class(cleaned_nfltotal$date)

#in team2, Houston and Oakland are not correctly abbreviated
#Recode abbreviations for HOU and OAK using dplyr
library(dplyr)
cleaned_nfltotal <- cleaned_nfltotal %>%
  mutate(team2 = recode(team2,
                        "Houston" = "HOU",
                        "Oakland" = "OAK",
                        "OAKLAND" = "OAK"))
View(cleaned_nfltotal)

#I've decided to keep the city abbreviations in capital letters
#This could be changed using tolower() if I chose to do this

#Rename qb1 and qb2 variables
cleaned_nfltotal<-rename(cleaned_nfltotal, QB1=qb1, QB2=qb2)

#subset the datatset where score1 and score2 both > 20
score20 <- subset(cleaned_nfltotal, score1 > 20 & score2 > 20)
View(score20)

#Start checking for anomalies
#View summary of data
summary(cleaned_nfltotal)

#many of the variables did not provide many summary values as they are incorrectly
#classified as characters

#change variables class types from character to numeric
#work with columns 7-14 first
cleaned_nfltotal[7:14] <- lapply(cleaned_nfltotal[7:14], as.numeric)

#columns 15 and 16 are quarterback names, these can remain as character

#work with columns 17-30 next
cleaned_nfltotal[17:30] <- lapply(cleaned_nfltotal[17:30], as.numeric)

#view summary of data to check for outliers
summary(cleaned_nfltotal)

#there could be potential outliers. there are some variables with values that seem
#to be too high or too low. there are some values that are negative but maybe shouldn't be.
#I do not have much context to use when looking at these values so I am not sure if they are incorrect.

#View the data using graphs
#load tidyverse package
library(tidyverse)

#View distribution of QB1
ggplot(data= cleaned_nfltotal, aes(x = QB1)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequency of QB1", x = "QB1", y = "Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#View distribution of QB2
ggplot(data= cleaned_nfltotal, aes(x = QB2)) +
  geom_bar(fill = "magenta") +
  labs(title = "Frequency of QB2", x = "QB2", y = "Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plot numeric data to assess outliers
#create boxplot of elo1_post
boxplot(cleaned_nfltotal$elo1_post, main= "elo1_post", xlab="elo1_post", ylab="Values")

#create boxplot of elo2_post
boxplot(cleaned_nfltotal$elo2_post, main= "elo2_post", xlab="elo2_post", ylab="Values")

#there are similar outliers in elo1_post and elo2_post

#create boxplot of qbelo1_post
boxplot(cleaned_nfltotal$qbelo1_post, main= "qbelo1_post", xlab="qbelo1_post", ylab="Values")

#create boxplot of qbelo2_post
boxplot(cleaned_nfltotal$qbelo2_post, main= "qbelo2_post", xlab="qbelo2_post", ylab="Values")

#There is a clear outlier in qbelo1_post
#There is not a clear outlier in qbelo2_post

#plot score1 and score2
#score1
ggplot(data = cleaned_nfltotal, aes(x=score1)) +
  geom_histogram(binwidth = 1, fill= "skyblue", color="black")+
  labs(title = "Frequency of score1",
       x = "score1",
       y="Frequency")+
  theme_minimal()

#score2
ggplot(data = cleaned_nfltotal, aes(x=score2)) +
  geom_histogram(binwidth = 1, fill= "magenta", color="black")+
  labs(title = "Frequency of score2",
       x = "score2",
       y="Frequency")+
  theme_minimal()

#As stated before, it is difficult to assess outliers as I do not know the context of the values
#once we understand the cause of the outliers, we could remove them
#this step could be applied to many of the variables in this dataset if they have outliers

#for example, I will remove the outlier (15) from qbelo1_post
# Calculate the IQR and bounds for detecting outliers
# Calculate lower and upper bounds for outliers
# Identify the minimum value (outlier in this case)
# Filter out the minimum value
# Check the cleaned data

#write to csv
write.csv(cleaned_nfltotal, "cleanednflpredictions.csv", row.names = FALSE)

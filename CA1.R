#PART 1 - LOADING THE DATASET

#we load the data in heart.csv
heartattack <- read.csv("heart.csv", na = "")
heartattack

#PART 2 - PRELIMNARY ANALYSIS

print(is.data.frame(heartattack))
#finding out the number of columns in the data frame
print(ncol(heartattack))
#finding out the number of rows in the data frame
print(nrow(heartattack))
#Identifying the schema of the data frame
str(heartattack)
#finding the count of na values
sum(is.na(heartattack))
#As per our initial analysis we understand that there are 303 rows and 14 columns
#the values are in numerical format (integers and floats)
#we also find that there are no Nan values too in our data set

#PART 3 - EXPLORATORY DATA ANALYSIS


#Since we do not have a information about the sex , we consider 1 as male and 0 as female
males <- subset( heartattack, sex == "1")
print(males)
female <- subset( heartattack, sex == "0")
print(female)
#finding the number of males and females in the data set
table(heartattack$sex)

#finding the categorical and numerical variables
#------------categorical----------------
unique(heartattack$sex)
unique(heartattack$cp)
unique(heartattack$fbs)
unique(heartattack$restecg)
unique(heartattack$exng)
unique(heartattack$slp)
unique(heartattack$caa)
unique(heartattack$thall)
unique(heartattack$output)
#------------numerical------------------
unique(heartattack$age)
unique(heartattack$trtbps)
unique(heartattack$chol)
unique(heartattack$thalachh)
unique(heartattack$oldpeak)
unique(heartattack$slp)

#taking a deep look into each of the variables
# Creates the plot
install.packages("ggplot2")
# Colour each chart point witha colour palette
install.packages("viridis")
library(ggplot2)
library(viridis)

#age
#finding out the maximum and minimum age of in the records
max_age <- max(heartattack$age)
max_age
min_age <- min(heartattack$age)
min_age
#plotting the graph for better visualizations
hist(heartattack$age, main = "Histogram for age", xlab = "Age")
#calculating the mean 
meanage <- (29 + 77)/2
meanage
summary(heartattack$age)
# we get the calculated mean and the mean from summary
#the target variable
prop.table(table(heartattack$output))

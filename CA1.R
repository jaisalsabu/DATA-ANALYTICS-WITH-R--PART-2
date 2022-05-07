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

#-------------------------------------------------------------------------------

#age
#finding out the maximum and minimum age of in the records
max_age <- max(heartattack$age)
max_age
min_age <- min(heartattack$age)
min_age
#plotting the graph for better visualizations
hist(heartattack$age, main = "Histogram for age", xlab = "Age")
summary(heartattack$age)
#we use a boxplot for the age variable
boxplot(heartattack$age)

#-------------------------------------------------------------------------------

#sex
#Since we do not have a information about the sex 
#we consider 1 as male and 0 as female
males <- subset( heartattack, sex == "1")
print(males)
female <- subset( heartattack, sex == "0")
print(female)
#finding the number of males and females in the data set
table(heartattack$sex)
#using bar plot to understand the sex variable
sexcounts <- table(heartattack$sex)
barplot(sexcounts, main="sex")
#analysis- the number of males are higher in the data set than females 

#-------------------------------------------------------------------------------

#cp - Chest pain(types)
#4 types
cptype1 <- subset( heartattack, cp == "0")
print(cptype1)
cptype2 <- subset( heartattack, cp == "1")
print(cptype2)
cptype3 <- subset( heartattack, cp == "2")
print(cptype3)
cptype4 <- subset( heartattack, cp == "3")
print(cptype4)
#finding the count of different types of chest pain within the data
table(heartattack$cp)
#using bar plot to understand the chest pain (cp) variable
cpcounts <- table(heartattack$cp)
barplot(cpcounts, main="chest pain")
#analysis- Most number of patients (143) face type 1 chest pain (typical angina) 
#a very few patients (23) have type 4 chest pain (asymptomatic) 
#type 2 chest pain is experienced by 50 patients
#type 3 chest pain is experienced by 87 patients

#-------------------------------------------------------------------------------
  
#trtbps
#finding out the maximum and minimum resting blood pressure
max_restingbloodpressure <- max(heartattack$trtbps)
max_restingbloodpressure
min_restingbloodpressure <- min(heartattack$trtbps)
min_restingbloodpressure
#plotting the graph for better visualizations
hist(heartattack$trtbps, main = "Histogram for Resting Blood Pressure", xlab = "Resting Blood pressure")
summary(heartattack$trtbps)
#we use a boxplot for the age variable
boxplot(heartattack$trtbps)
#for a clear understanding we will use density plot
trtbps <- density(heartattack$trtbps) # returns the density data
plot(trtbps, 
     main="Density plot for resting blood pressure",
     xlab = "Resting blood pressure")

#-------------------------------------------------------------------------------

#chol (cholesterol)
#finding out the maximum and cholesterol
max_cholesterol <- max(heartattack$chol)
max_cholesterol
min_cholesterol <- min(heartattack$chol)
min_cholesterol
#plotting the graph for better visualizations
hist(heartattack$chol, main = "Histogram for Cholesterol", xlab = "Cholesterol")
summary(heartattack$chol)
#we use a boxplot for the age variable
boxplot(heartattack$chol)
#for a clear understanding we will use density plot
chol <- density(heartattack$chol) # returns the density data
plot(chol, 
     main="Density plot for Cholesterol",
     xlab = "Cholesterol")

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

#UNI-VARIATE ANALYSIS

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
hist(heartattack$trtbps, main = "Histogram for Resting Blood Pressure",
     xlab = "Resting Blood pressure")
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
#we use a boxplot for the cholesterol variable
boxplot(heartattack$chol)
#for a clear understanding we will use density plot
chol <- density(heartattack$chol) # returns the density data
plot(chol, 
     main="Density plot for Cholesterol",
     xlab = "Cholesterol")
#analysis - Cholesterol value in most patients is between 200 and 280.
# Values after 380 are to be considered as outliers.

#-------------------------------------------------------------------------------

#fbs (fasting blood sugar)
#if fasting blood sugar more than 120md/dl the value is 1(True) or else 0(False)
fbstrue <- subset( heartattack, fbs == "1")
print(fbstrue)
fbsfalse <- subset( heartattack, fbs == "0")
print(fbsfalse)
#count of patients with fbs values
table(heartattack$fbs)
#we use a bar plot to visualize the data clearly
fbsplot <- table(heartattack$fbs)
barplot(fbsplot, main="Fasting Blood Sugar")
#analysis - a major share of patients have value 0 ~ 85%
#the blood glucose levels of majority of patients is below 120mg/dl
#15% patients have a high blood glucose level

#-------------------------------------------------------------------------------

#restecg(resting electrocardiographic results)
restecg0 <- subset( heartattack, restecg == "0")
print(restecg0)
restecg1 <- subset( heartattack, restecg == "1")
print(restecg1)
restecg2 <- subset( heartattack, restecg == "2" )
print(restecg2)
#count of patients with fbs values
table(heartattack$fbs)
#we use a bar plot to visualize the data clearly
restecgplot <- table(heartattack$restecg)
barplot(restecgplot, main="resting electrocardiographic results")
#analysis - the patients with value 2 is very negligible compared to the rest
#the value 1 and 0 are almost equal with value 1 having a slight head
#~ 50.2% of patients have ST-T wave abnormality
#~48.5% of patients have a normal resting electrocardiogram result

#-------------------------------------------------------------------------------

#thalachh (max heart rate)
max_heartrate <- max(heartattack$thalachh)
max_hearrate
min_heartrate <- min(heartattack$thalachh)
min_heartrate
#plotting the graph for better visualizations
hist(heartattack$thalachh, main = "Histogram for maximum heart rate achieved", 
     xlab = " Max Heart Rate")
summary(heartattack$thalachh)
#we use a boxplot for the heartrate variable
boxplot(heartattack$thalachh)
#for a clear understanding we will use density plot
restecgplot <- density(heartattack$thalachh) # returns the density data
plot(restecgplot, 
     main="Density plot for Max Heart Rate",
     xlab = "Heart Rate")
#analysis - maximum heart rate achieved in most patients is between 145 and 170. 


#-------------------------------------------------------------------------------

#exng (Angina induced Exercise)
exng0 <- subset( heartattack, exng == "0")
print(exng0)
exng1 <- subset( heartattack, exng == "1")
print(exng1)
#count of patients with Angina induced exercise values
table(heartattack$exng)
#we use a bar plot to visualize the data clearly
exang <- table(heartattack$exng)
barplot(exang, main="Exercise Induced Angina")
#analysis - Most of the patients do not have exercise induced angina 

#BI-VARIATE ANALYSIS

#-------------------------------------------------------------------------------

#PART 4 _ DATA TRANSFORMATION
#we take a look at the column names 
names(heartattack)
#we rename a few columns to make the data more understandable
names(heartattack)[names(heartattack)=="sex"] <- "Gender"
names(heartattack)[names(heartattack)=="cp"] <- "Chest_pain"
names(heartattack)[names(heartattack)=="trtbps"] <- "Resting_blood_pressure"
names(heartattack)[names(heartattack)=="chol"] <- "Cholesterol"
names(heartattack)[names(heartattack)=="fbs"] <- "Fasting_blood_sugar"
names(heartattack)[names(heartattack)=="thalachh"] <- "Maximum_heart_rate"
names(heartattack)[names(heartattack)=="exng"] <- "agina_exercise"
names(heartattack)[names(heartattack)=="output"] <- "Target"
#after changing the column names
names(heartattack)
#We will not be consider all the variables for our hypothesis , so we have to
#remove a few variables from our data set
heart_attack_analysis <- subset(heartattack, select = -c(oldpeak, slp, caa, thall))
#after removing all the unnecessary columns from our data frame
heart_attack_analysis
#names of the columns in new data frame 
names(heart_attack_analysis)

# PART 5 - HYPOTHESIS TESTING 

# Q1.  
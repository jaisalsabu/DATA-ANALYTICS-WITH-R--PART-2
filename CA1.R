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

#---------------------------------------------------------------------------------------------------------------------------------

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
boxplot(heartattack$age, ylab= "Age in Years")

#---------------------------------------------------------------------------------------------------------------------------------

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
barplot(sexcounts, main="sex", ylab = "no of females", xlab = "Gender")
#analysis- the number of males are higher in the data set than females 

#---------------------------------------------------------------------------------------------------------------------------------
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
barplot(cpcounts, main="chest pain", xlab = "Types of chest pain", ylab ="No of patients")
#analysis- Most number of patients (143) face type 1 chest pain (typical angina) 
#a very few patients (23) have type 4 chest pain (asymptomatic) 
#type 2 chest pain is experienced by 50 patients
#type 3 chest pain is experienced by 87 patients

#---------------------------------------------------------------------------------------------------------------------------------
  
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

#---------------------------------------------------------------------------------------------------------------------------------

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

#---------------------------------------------------------------------------------------------------------------------------------

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

#---------------------------------------------------------------------------------------------------------------------------------

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

#---------------------------------------------------------------------------------------------------------------------------------

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


#---------------------------------------------------------------------------------------------------------------------------------

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


#---------------------------------------------------------------------------------------------------------------------------------

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
#summary of new dataframe
summary(heart_attack_analysis)

#--------------------------------------------------------------------------------------------------------------------------------

# PART 5 - HYPOTHESIS TESTING 

#--------------------------------------------------------------------------------------------------------------------------------

# Q1. Is there any relation between Gender and Chance of having heart attack ?

#H0 : Gender has no impact on chances of having heart attack
#H1 : Gender has an impact on chances of having heart attack

#ANALYSIS OF Q1
#The variables we consider for the research question is Gender and Target variable
#The Gender variable is a categorical variable with 2 Values 
#The Target variable is a categorical variable with 2 Values
#Here our Dependent Variable is Target (i.e Chances of having heart attack)
#and our Independent Variable is Gender (i.e Male (1) or Female(0)) 

#Since both our variables are Categorical we will be selecting  Pearsons Chi- Squared Test 
#we create a way table for much clear understanding of the situation
gentar <-table(heart_attack_analysis$Gender, heart_attack_analysis$Target)
gentar
chisq.test(gentar)
#we can also set correct = FALSE to turn off Yates Continuity correction
chisq.test(table(heart_attack_analysis$Gender, heart_attack_analysis$Target), correct = FALSE)

#we get a p value of less than the value of significance(0.05) 
#X-squared = 23.914
#df = 1
#p value = 1.007e-06
#we reject the null hypothesis and accept the alternate hypothesis

# OUTPUT : From our data we understand that the gender of a patient has an impact 
#on the chances of having a heart attack

 
#Q2. Does cholesterol have an effect on the maximum heart rate a person has reached ?

#H0 - Cholesterol does not have a effect on the maximum heart rate of a patient
#H1 - Cholesterol has a effect on the maximum heart rate of a patient
 
#our dependent variable is maximum heart rate and our independent variable is Cholesterol

# we conduct a  normality test on our dependent variable 
#Shapiro-Wilk test
 # Null hypothesis: the data are normally distributed
 # Alternative hypothesis: the data are not normally distributed
#normality for maximum heart rate
normality_test_maximumheartrate<- shapiro.test(heart_attack_analysis$Maximum_heart_rate)
normality_test_maximumheartrate$p.value
#the p value is 6.620818e-05, which is less than the value of significance 0.05
#so we can conclude that our dependent variable is not normally distributed


#normality for cholesterol
normality_test_cholesterol<- shapiro.test(heart_attack_analysis$Cholesterol)
normality_test_cholesterol$p.value
#the p value is 5.364848e-09, which is less than the value of significance 0.05
#so we can conclude that our dependent variable is not normally distributed


#qqplot for maximum heart rate achieved
qqnorm(heart_attack_analysis$Maximum_heart_rate,main="QQ plot for Maximum Heart Rate", pch=19, ylab = "Maximum Heart Rate")
qqline(heart_attack_analysis$Maximum_heart_rate)

#qqplot for cholesterol
qqnorm(heart_attack_analysis$Cholesterol,main="QQ plot for Cholesterol",pch=19, ylab = "Cholesterol")
qqline(heart_attack_analysis$Cholesterol)

#Since our depndent and independent variables are continuous and they both are not normally distributed
#we will be using spearmans rank correlation

res <- cor.test(heart_attack_analysis$Cholesterol, heart_attack_analysis$Maximum_heart_rate, method = "spearman")         
res
res$p.value
#The correlation coefficient between cholesterol and maximum heart rate achieved by a patient is -0.04676639

#The p value of the test is 0.4173 which is more than the value of significance
#we accept the null hypothesis and reject the alternate hypothesis 

#OUTPUT - We find out that cholesterol doesnot have an effect on the maximum heart rate a person has achieved

#Q3.Does age have any relation to cholesterol levels in a patient?

#H0 - Age does not have any relation to Cholesterol levels
#H1 - Age has relation to cholesterol levels

#our dependent variable is cholesterol and our independent variable is age

#we have to check the normality for out variables
# we conduct a  normality test on our dependent variable 
#Shapiro-Wilk test
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed

#normality for cholesterol
normality_test_cholesterol<- shapiro.test(heart_attack_analysis$Cholesterol)
normality_test_cholesterol$p.value
#the p value is 5.364848e-09, which is less than the value of significance 0.05
#so we can conclude that our dependent variable is not normally distributed

#normality for age
normality_test_age<- shapiro.test(heart_attack_analysis$age)
normality_test_age$p.value
#the p value is 0.005798359, which is higher than the value of significance 0.05
#so we can conclude that our independent variable is  normally distributed

#since our dependent variable and independent variable is continuous 
#our dependent variable is not normally distributed 
#we have to use Spearmans rank correlation 
res <- cor.test(heart_attack_analysis$age, heart_attack_analysis$Cholesterol, method = "spearman")         
res
res$p.value

#from the spearmans rank correlation we can understand that  


#The p value of the test is 0.0006099143 which is less than the value of significance
#we reject the null hypothesis and accept the alternate hypothesis 

#OUTPUT - The age has a impact on the cholesterol levels in patients

#Q4 - Does Gender affect the resting blood pressure of a patient ?

#H0 : Gender has no effect on the resting blood pressure of patient
#H1 : Gender has a effect on the resting blood pressure of patient

#Here,  our dependent variable is resting blood pressure
#and the independent variable is gender

#we have to take a normality test for our dependent variable 
#we perform the shapiro-wilk test
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed

normality_test_restingbp<- shapiro.test(heart_attack_analysis$Resting_blood_pressure)
normality_test_restingbp$p.value

#the p-value is 1.458097e-06 which is less that 0.05
#the resting blood pressure variable is not normally distributed

#so we will be selecting man whitney test 
heart_attack_analysis$Resting_blood_pressure
wilcox.test(Resting_blood_pressure ~ Gender, data=heart_attack_analysis)
#from the Mann Whitney Wilcoxcon Test we get a p value of 0.3579
#the p value is higher than the significance of 0.05 
#so we reject the alternate hypothesis and accept the null hypothesis.

#OUTPUT - Gender has no impact or effect on the resting blood pressure of the patient

#Q5 - Does the type of Chest Pain have a effect on the chances of having a heart attack?

#H0 : Chest Pain and Heart attack are not related to each other
#H1 : Chest Pain and Heart attack are related to each other 

#The variables we consider for the research question is Chest Pain and Heart attack variable
#The Chest Pain variable is a categorical variable with 4 Values 
#The Heart attack variable is a categorical variable with 2 Values
#Here our Dependent Variable is Target (i.e Chances of having heart attack)
#and our Independent Variable is Chest Pain 

#Since both our variables are Categorical we will be selecting  Pearsons Chi- Squared Test 
#we create a way table for much clear understanding of the situation
cptar <-table(heart_attack_analysis$Chest_pain, heart_attack_analysis$Target)
cptar
chisq.test(cptar)

#we can also set correct = FALSE to turn off Yates Continuity correction
chisq.test(table(heart_attack_analysis$Chest_pain, heart_attack_analysis$Target), correct = FALSE)

#we get a p value of less than the value of significance(0.05) 
#X-squared = 23.914
#df = 1
#p value < 2.2e-16
#From the results of the test we understand that the p value is less than that 
#of the significant value 0.05, 
#We reject the null hypothesis and accept the alternate hypothesis

#OUTPUT - The type of Chest pain is related to the target variable(chance of having heart attack)

#**********************************************************************************************************************************************


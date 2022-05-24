#PART 1 - LOADING THE DATASET

#we load the data in heart.csv
heartattack <- read.csv("heart.csv", na = "")
heartattack

#https://www.rpubs.com/GiftMtambo/857615

#PART 2 - ANALYSIS AND TRANSFORMATION


#getting to know the column names 
colnames(heartattack)
#understanding the type of values
sapply(heartattack, class)
#we can understand that every value is of integer/numerical type 

#listing the first 20 rows, to understand the data better
head(heartattack,n=20)


library(dplyr)
#classify the variables into categorical and numerical variables 
#select the numerical variables
numeric_var <-heartattack %>% 
  select("age","trtbps","chol","thalachh","oldpeak")
numeric_var
#select the categorical values and factorising 
library(dplyr)
categorical_var<- heartattack %>%
  select("sex","cp","fbs","restecg","exng","slp","caa",
         "thall","output")%>%
  mutate_if(is.numeric, as.factor)

categorical_var

#combine the categorical  and numerical values
heartattack_prediction = cbind(categorical_var,numeric_var)

heartattack_prediction$output <- as.numeric(heartattack_prediction$output)
sapply(heartattack_prediction, class)

#to find the missing values , complete and incomplete cases , we use the library DataExplorer
#we also get to know the continuous and discrete variables in detail
install.packages("DataExplorer")
library(DataExplorer)
plot_intro(heartattack_prediction,title="Dataset Information")

#descriptive statistics
install.packages("psych")

library(psych)
describeBy(heartattack_prediction)
#from this we conculde that our dataset is highly skewed


#pairplot
pairs(heartattack_prediction)
# Initial investigation of data variable
pairs.panels(heartattack_prediction,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
#we will implement a correlation plot to check the correlation betweeen variables
correlation_tab <- cor(heartattack)
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#png(file="corr2.png",res=150,width=900,height=700)                        
corrplot(correlation_tab, method = "color", shade.col = NA, tl.col = "black", tl.srt = 45,tl.cex =1,cl.cex=1,col = col(200), addCoef.col = "black", order = "AOE",number.cex = .5)

#we now plot our output to understand the balance of our data set
table(heartattack$output)
#around 138 people have the no chances of a heart attack
#while 165 people from our observation have the risk of heart attack
#around 54% of people have the chances of heart attack in our dataset

#PART 3 - OUTLIER DETETCTION AND SOLUTION
#There are multiple ways to detect outliers within a dataset. 
#scatter plots and bar plots are quite commonly used
#we will be trying to use the boxplot analysis to detect outliers.

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,3))
attach(heartattack_prediction)
boxplot(heartattack_prediction$age,
        main = "Age",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$age)$out)
)

boxplot(heartattack_prediction$chol,
        main = "Cholesterol",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$chol)$out)
)
#from the boxplot for chol variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$chol)$out

boxplot(heartattack_prediction$oldpeak,
        main = "Oldpeak",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$oldpeak)$out)
)
#from the boxplot for variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$oldpeak)$out

boxplot(heartattack_prediction$thalachh,
        main = "Maximum heart rate achieved",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$thalachh)$out)
)
#from the boxplot for thalachh variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$thalachh)$out

boxplot(heartattack_prediction$trtbps,
        main = "Resting Blood pressure",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$trtbps)$out)
)
#from the boxplot for trtbps variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$trtbps)$out
detach(cars)
par <- opar

#since we have found out all the outliers in the variables
#we have to rmove them from the data prepared for prediction.


#-----------------------left here at 8:45 on  24-05-22 --------------------------------------
#splitting the dataset
set.seed(100)
split = sample.split(heartattack_prediction$output, SplitRatio = 0.80)
train_set = subset(heartattack_prediction, split == TRUE)
test_set = subset(heartattack_prediction, split == FALSE)

model <- lm(output ~ age + sex + cp + chol + thall, data = heartattack_prediction)
summary(model)

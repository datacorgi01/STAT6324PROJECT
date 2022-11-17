# 6324 Final Project
#Version 1

#Data dictionary for the heart disease dataset
#https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease

#Clear Environment
remove(list = ls())

#Add libraries
library(mice)
library(ggplot2)
library(corrplot)
library(gmodels)

#Set working directory (you can add yours and comment the rest out)
#setwd("/Users/nicoleng/Dropbox/SMU/MASDA/STAT 6324/Project") #Nicole

#Read in dataset
heart <- read.csv("/Users/allisonking/Downloads/heart_2020_cleaned.csv") #Allison

#Set seed number
set.seed(1234)

#Preliminary inspection of data
str(heart)
summary(heart)

#EDA
#Describe variables and distribution
#Ex: Comparing explanatory variables to variable:heartdisease (like race/age/diabetes etc...)
#We could also check for multicollinearity for between variables

#Visualize missing data - no missing values!!
md.pattern(heart)
simple_aggr = aggr(heart, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(heart), cex.axis=.7, 
                   gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#Boxplot of bmi by heart disease - outliers check
boxplot(BMI ~ HeartDisease, data=heart, horizontal=T, col=c("yellow"), 
        main='Boxplots of BMI Score by Heart Disease Diagnosis')

heart$CDC <- ifelse(heart$BMI<=18.50, "Underweight",
                    ifelse(18.51<=heart$BMI & heart$BMI<=24.99, "Healthy",
                    ifelse(25.00<=heart$BMI & heart$BMI<=29.99, "Overweight",
                    ifelse(heart$BMI>=30.00, "Obese", NA))))

#All heart data BMI
ggplot(heart, aes(x = CDC, fill = HeartDisease)) + geom_bar() + ggtitle("CDC BMI Classification by Heart Disease") 

#A huge majority of the data is people who do not have heart disease. However, we're interested in those who do.
#Splitting the data and only doing further EDA on heart disease positive people
#Of those that have heart disease:
dfHD <- heart[which(heart$HeartDisease == "Yes"),]

#Heart disease positive only BMI
ggplot(dfHD, aes(x = CDC)) + geom_bar(fill="lightblue") + ggtitle("CDC BMI Classification Among People with Heart Disease")

#Boxplot of sleep by heart disease - outliers check
boxplot(SleepTime ~ HeartDisease, data=heart, horizontal=T, col=c("orange"), 
        main='Boxplots of Sleep by Heart Disease Diagnosis')

#Boxplot of physical health by heart disease - outliers check
boxplot(PhysicalHealth ~ HeartDisease, data=heart, horizontal=T, col=c("deeppink"), 
        main='Boxplots of Physical Health Score by Heart Disease Diagnosis')

#Boxplot of mental health by heart disease - outliers check
boxplot(MentalHealth ~ HeartDisease, data=heart, horizontal=T, col=c("lightblue"), 
        main='Boxplots of Mental Health Score by Heart Disease Diagnosis')

#Correlation matrix on numeric data only
num_df <- data.frame(heart$BMI, heart$PhysicalHealth, heart$MentalHealth, heart$SleepTime)
M <- cor(num_df)
corrplot(M, method="circle")

#Finding how many correlations are bigger than 0.70
k = 0
for(i in 1:4) {
  for(r in 1:4){
    if(M[i,r]> 0.70 & i != r){
      k= k + 1
    }
  }  }
print(k/2)

#Bar graph showing class distribution of heart disease column
barplot(prop.table(table(heart$HeartDisease)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#Save a table of heart disease and smoking
HeartSmoke <- table(heart$HeartDisease,
                  heart$Smoking)

#Barplot for smoking for people who have heart disease
barplot(HeartSmoke,
        beside = TRUE,
        main = "Smoking Distribution by Heart Disease Group",
        xlab = "Smoker",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 200000),
        args.legend=list(title="Heart Disease"),
        col = c("deeppink", "orange"))

# Save a table of heart disease and alcohol
HeartAlc <- table(heart$HeartDisease,
                    heart$AlcoholDrinking)

#Barplot of alcohol consumption for people who have heart disease
barplot(HeartAlc,
        beside = TRUE,
        main = "Alcohol Distribution for Heart Disease Positive People",
        xlab = "Alcohol Drinker",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("green", "yellow"))

# Save a table of age for people who have heart disease
HeartAge <- table(heart$HeartDisease,
                  heart$AgeCategory)

#Barplot for age vs heart disease
barplot(HeartAge,
        beside = TRUE,
        main = "Age Distribution for Heart Disease Positive People",
        xlab = "Age",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 31000),
        args.legend=list(title="Heart Disease"),
        col = c("lightblue", "purple"))

# Save a table of walking and heart disease
HeartWalk <- table(heart$HeartDisease,
                  heart$DiffWalking)

#Barplot for walking for people who have heart disease
barplot(HeartWalk,
        beside = TRUE,
        main = "Difficulty Walking Distribution for Heart Disease Positive People",
        xlab = "Difficulty Walking",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("yellow", "blue"))

# Save a table of race and heart disease
HeartRace <- table(heart$HeartDisease,
                   heart$Race)

#Barplot for race vs heart disease
barplot(HeartRace,
        beside = TRUE,
        main = "Race Distribution for Heart Disease Positive People",
        xlab = "Race",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 250000),
        args.legend=list(title="Heart Disease"),
        col = c("pink", "red"))

# Save a table of heart disease and diabetes
HeartDia <- table(heart$HeartDisease, heart$Diabetic)

#Barplot for heart disease and diabetes
barplot(HeartDia,
        beside = TRUE,
        main = "Diabetes Distribution for Heart Disease Positive People",
        xlab = "Diabetes",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 260000),
        args.legend=list(title="Heart Disease"),
        col = c("orange", "lightgreen"))

# Save a table of heart disease and sex
Heartsex <- table(heart$HeartDisease, heart$Sex)

#Barplot for heart disease and diabetes
barplot(Heartsex,
        beside = TRUE,
        main = "Sex Distribution for Heart Disease Positive People",
        xlab = "Sex",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 170000),
        args.legend=list(title="Heart Disease"),
        col = c("deeppink", "blue"))

# Save a table of heart disease and general health
HeartGE <- table(heart$HeartDisease, heart$GenHealth)

#Barplot for heart disease and diabetes
barplot(HeartGE,
        beside = TRUE,
        main = "General Health Distribution for Heart Disease Positive People",
        xlab = "General Health",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 120000),
        args.legend=list(title="Heart Disease"),
        col = c("lightblue", "green"))

# Save a table of heart disease and stroke
Heartst <- table(heart$HeartDisease, heart$Stroke)

#Barplot for heart disease and stroke
barplot(Heartst,
        beside = TRUE,
        main = "Stroke Distribution for Heart Disease Positive People",
        xlab = "Stroke",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("orange", "yellow"))

# Save a table of heart disease and skin cancer
Heartskin <- table(heart$HeartDisease, heart$SkinCancer)

#Barplot for heart disease and skin cancer
barplot(Heartskin,
        beside = TRUE,
        main = "Skin Cancer Distribution for Heart Disease Positive People",
        xlab = "Skin Cancer",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("blue", "red"))

# Save a table of heart disease and kidney disease
Heartkid <- table(heart$HeartDisease, heart$KidneyDisease)

#Barplot for heart disease and kidney disease
barplot(Heartkid,
        beside = TRUE,
        main = "Kidney Disease Distribution for Heart Disease Positive People",
        xlab = "Kidney Disease",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 300000),
        args.legend=list(title="Heart Disease"),
        col = c("lightpink", "yellow"))

# Save a table of heart disease and asthma
Heartast <- table(heart$HeartDisease, heart$Asthma)

#Barplot for heart disease and asthma
barplot(Heartast,
        beside = TRUE,
        main = "Asthma Distribution for Heart Disease Positive People",
        xlab = "Asthma",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 270000),
        args.legend=list(title="Heart Disease"),
        col = c("red", "yellow"))

# Save a table of heart disease and physical activity
Heartpa <- table(heart$HeartDisease, heart$PhysicalActivity)

#Barplot for heart disease and physical activity
barplot(Heartpa,
        beside = TRUE,
        main = "Physical Activity Distribution for Heart Disease Positive People",
        xlab = "Physical Activity",
        ylab = "Count",
        legend = TRUE,
        ylim = c(0, 250000),
        args.legend=list(title="Heart Disease"),
        col = c("purple", "green"))


#2-Way Cross Tabulation for balloon plot
df <- data.frame(CrossTable(heart$AgeCategory, heart$Race)) 
new_df <- df[,c("t.x","t.y","t.Freq")]
heartdat <- pivot_wider(new_df, names_from = t.x, values_from = t.Freq)

#Balloon plot
ggballoonplot(
  df, x = "t.x", y = "t.y",
  size = "t.Freq", fill = "t.Freq",
  ggtheme = theme_bw()
)

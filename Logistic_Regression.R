# THE AIM OF THIS PROJECT IS TO DECIDE ON THE IMPORTANT FACTORS WHICH ARE
# STATISTICALLY SIGNIFICANT IN EXPLAINING WHETHER A PERSON HAS GOT 
# HEART DISEASE. AFTER SELECTING FACTORS WHICH ARE SIGNIFICANT, SIMILAR
# DATA CAN BE USED FOR CLASSIFICATION PURPOSES. PEOPLE WILL BE CLASSFIED
# AS "HEALTHY" OR "UNHEALTHY". 

# the data was extracted from kagle on 
# https://www.kaggle.com/miguelfzzz/heart-failure-prediction-classification-models/data


# ATTACHING THE DATA FILE
attach(heart)

## Viewing how the data look like
View(heart)

## checking the structure of the data set that we are working with
str(heart)

## Converting different variables to read as factors
heart$Sex<-as.factor(heart$Sex)
heart$Age<-as.factor(heart$Age)
heart$ChestPainType<-as.factor(heart$ChestPainType)
heart$RestingBP<-as.factor(heart$RestingBP)
heart$Cholesterol<-as.factor(heart$Cholesterol)
heart$FastingBS<- ifelse(test=heart$FastingBS == 0,yes = "Absent",no="Present")
heart$FastingBS<-as.factor(heart$FastingBS)
heart$RestingECG<-as.factor(heart$RestingECG)
heart$MaxHR<-as.factor(heart$MaxHR)
heart$ExerciseAngina<-as.factor(heart$ExerciseAngina)
heart$Oldpeak<-as.factor(heart$Oldpeak)
heart$ST_Slope<-as.factor(heart$ST_Slope)
heart[heart$HeartDisease==0]$HeartDisease
heart$HeartDisease<- ifelse(test=heart$HeartDisease == 0,yes = "Healthy",no="Unhealthy")
heart$HeartDisease<-as.factor(heart$HeartDisease)

## Removing na values
heart<-heart[!(is.na(heart$Cholesterol) | is.na(heart$Cholesterol)),]

## checking the structure of the data after fixes
str(heart)

## checking the number of observations
nrow(heart)

## checking whether both female and males have healthy and unhealthy people
xtabs(~HeartDisease+Sex, data = heart)
### The data contains 143 healthy and 50 unhealthy females, and 267 healthy and 
### 458 unhealthy Males. Therefore we proceed by considering both females and males 
### in model building, if gender had one condition eg all females being healthy we
### were going to remove that gender in model building because it would not add
### value.

xtabs(~HeartDisease+ChestPainType, data = heart)
### The data contains both healthy and unhealthy from the people with different
### types of chest pain.

xtabs(~HeartDisease+FastingBS, data = heart)
### The data contains both healthy and unhealthy from the people with different 
### levels of fasting blood sugar.

xtabs(~HeartDisease+RestingECG, data = heart)
### The data contains both healthy and unhealthy from the people with different 
### levels of RestingECG ( a simple, quick and painless test).

xtabs(~HeartDisease+ExerciseAngina, data = heart)
### The data contains both healthy and unhealthy from the people with different 
### levels of ExerciseAngina ( pain in the chest).

xtabs(~HeartDisease+ST_Slope, data = heart)
### The data contains both healthy and unhealthy from the people with different 
### levels ofST_slope.

# MODEL BUILDING (SIMPLE LOGISTIC MODEL)
Logistic_Model1<-glm(HeartDisease~Sex, data = heart, family = "binomial")
summary(Logistic_Model1)
### The residuals looks okay, they are nearly  centered at 0.

### Since the p values for the coefficients are < 0.05, therefore the coefficients
### are significant for the model. We can have the following equations
### HeartDisease = -1.0508  for females. Thus the log (odds) that a female has
### a HeartDisease is -1.0508
### HeartDisease = -1.0508 + 1.5904 (1) for male. The second term is the 
### log(odds ratio) of the odds that a male will have heart disease over the 
### odds that a female will have heart disease.
### HeartDisease = -1.0508 + 1.5904

# MODEL BUILDING WITH SEX AND AGE
Logistic_Model2<- glm(HeartDisease~Sex+Age,data = heart, family = "binomial")
summary(Logistic_Model2)
### Since all p values for different ages are >0.05, age is not statistically
### significant in determing whether the person has heart disease or not

# MODEL BUILDING WITH SELECTED FACTORS
Logistic_Model4<-glm(HeartDisease~Sex+ChestPainType+RestingBP+Cholesterol, data = heart, family = "binomial")
summary(Logistic_Model4)
### RestingBP is not statistically significant in explaining whether
### someone has a heart disease

Logistic_Model5<-glm(HeartDisease~Sex+ChestPainType+ST_Slope+RestingECG, data = heart, family = "binomial")
summary(Logistic_Model5)
#### RestingECG is not statistically significant in explaining whether 
#### someone has a heart disease

Logistic_Model6<-glm(HeartDisease~Sex+ChestPainType+ST_Slope+MaxHR, data = heart, family = "binomial")
summary(Logistic_Model6)
### MaxHR is not statistically significant in explaining whether
### someone has a heart disease

Logistic_Model7<-glm(HeartDisease~Sex+ChestPainType+ST_Slope+ExerciseAngina, data = heart, family = "binomial")
summary(Logistic_Model7)
### Exercise Angina Y is statistically significant in explaining whether
### someone has a heart disease

Logistic_Model8<-glm(HeartDisease~Sex+ChestPainType+ST_Slope+ExerciseAngina+ Oldpeak, data = heart, family = "binomial")
summary(Logistic_Model8)
### Oldpeak is not statistically significant for in explaining whether
### someone has a heart disease

# THE FINAL MODEL HAS THE FOLLOWING
Final_Logistic_Model<-glm(HeartDisease~Sex+ChestPainType+ST_Slope+ExerciseAngina+ Cholesterol, data = heart, family = "binomial")
summary(Final_Logistic_Model) 
### Sex, ChestPainType, ST_Slope, ExerciseAngina and Cholestrol are important
### factors which are statistically significant in explaining whether someone 
### has got a heart disease.

ll.null<- Final_Logistic_Model$null.deviance/-2
ll.proposed<- Final_Logisticl_Model$deviance/-2
(ll.null-ll.proposed)/ll.null
### the model explains 66.7% of the data. Therefore we can use other methods 
### and see whether our prediction accuracy will improve

1 - pchisq(2*(ll.proposed-ll.null), df=(length(Final_Logistic_Model$coefficients)-1))

# the chi square shows that the model is statistically significant


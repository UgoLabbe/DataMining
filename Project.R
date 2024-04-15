setwd("C:/Master/M1_S2/Data Mining/Projet")

df <- read.csv("LCA_FY_2022.csv")

###Data Cleaning
summary(df)
table(df$Unit_Of_Pay)

#set wage on the same time frame
df$Prevailing_Wage[df$Unit_Of_Pay == "Month"] <- df$Prevailing_Wage[df$Unit_Of_Pay == "Month"]*12
df$Prevailing_Wage[df$Unit_Of_Pay == "Week"] <- df$Prevailing_Wage[df$Unit_Of_Pay == "Week"]*52
df$Prevailing_Wage[df$Unit_Of_Pay == "Bi-Week"] <- df$Prevailing_Wage[df$Unit_Of_Pay == "Bi-Week"]*26
df$Prevailing_Wage[df$Unit_Of_Pay == "Hour"] <- df$Prevailing_Wage[df$Unit_Of_Pay == "Hour"]*26
df$Prevailing_Wage[df$Unit_Of_Pay == "Hour"] <- df$Prevailing_Wage[df$Unit_Of_Pay == "Hour"]*2080 #40x52

df$Unit_Of_Pay <- NULL
table(df$Full_Time_Position)

#lower every string variables
df$Employer_Name <- tolower(df$Employer_Name)
df$SOC_Title <- tolower(df$SOC_Title)
df$Job_Title <- tolower(df$Job_Title)
df$Worksite <- tolower(df$Worksite)
df$Employer_Location <- tolower(df$Employer_Location)

#create column city and column state
df$City <- sub("\\,.*", "", df$Employer_Location)
df$State <- sub(".*, ", "", df$Employer_Location)
df$Worksite <- NULL

#Data description
table(df$Employer_Country) #issue with Georgia -> country may be the state
table(df$Visa_Class)
table(df$Case_Status)
head(sort(table(df$Employer_Name),decreasing = T), 10)
head(sort(table(df$Job_Title),decreasing = T), 10)
head(sort(table(df$Employer_Location),decreasing = T), 10)
head(sort(table(df$State),decreasing = T), 10)

state_count <- as.data.frame(table(df$State[df$Case_Status=="Certified"]))
state_count <- state_count[-1,] #remove missing state
colnames(state_count)[colnames(state_count) == 'Var1'] <- 'state'


#Data visualisation
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(tidyverse)
library(ggeasy)

plot_usmap(data = state_count, 
           regions="state", 
           values="Freq", labels = T, label_color = "white", color="black")+
          scale_fill_viridis_c(name = "Frequency", 
                       limits = c(500,150000))+
          easy_move_legend(to = c("right")) +
          labs(title = "Frequency of certified visa in 2022") +
          theme(panel.background = element_rect(colour = "black"))

##Data Analysis

#ratio of denied visa
table(df$Case_Status)

denied_count <- as.data.frame(table(df$State[df$Case_Status=="Denied"]))
colnames(denied_count)[colnames(denied_count) == 'Var1'] <- 'state'

state_count <- merge(state_count, denied_count, by="state")
colnames(state_count)[colnames(state_count) == 'Freq.x'] <- 'certif'
colnames(state_count)[colnames(state_count) == 'Freq.y'] <- 'denied'

state_count$ratio <- state_count$denied/(state_count$certif+state_count$denied)

plot_usmap(data = state_count, 
           regions="state", 
           values="ratio", labels = T, label_color = "black", color="black")+
  scale_fill_continuous(low = "white", high = "red", 
                        name = "Ratio", 
                        limits = c(0,0.03))+
  easy_move_legend(to = c("right")) +
  labs(title = "Ratio of denied visa over certified visa") +
  theme(panel.background = element_rect(colour = "black"))

#ratio of denied visa over certified visa per company
denied_count <- as.data.frame(table(df$Employer_Name[df$Case_Status=="Denied"]))
comp_count <- as.data.frame(table(df$Employer_Name[df$Case_Status=="Certified"]))
comp_count <- merge(comp_count, denied_count, by="Var1", all=T)
denied_count <- NULL

colnames(comp_count)[colnames(comp_count) == 'Freq.x'] <- 'certif'
colnames(comp_count)[colnames(comp_count) == 'Freq.y'] <- 'denied'
comp_count$denied <- replace_na(comp_count$denied, 0)
comp_count$certif <- replace_na(comp_count$certif, 0)

comp_count$ratio <- comp_count$denied/(comp_count$certif+comp_count$denied)

#ratio of denied visa over certified visa per job
denied_count <- as.data.frame(table(df$Job_Title[df$Case_Status=="Denied"]))
job_count <- as.data.frame(table(df$Job_Title[df$Case_Status=="Certified"]))
job_count <- merge(job_count, denied_count, by="Var1", all=T)
denied_count <- NULL

colnames(job_count)[colnames(job_count) == 'Freq.x'] <- 'certif'
colnames(job_count)[colnames(job_count) == 'Freq.y'] <- 'denied'
job_count$denied <- replace_na(job_count$denied, 0)
job_count$certif <- replace_na(job_count$certif, 0)
job_count$ratio <- job_count$denied/(job_count$certif+job_count$denied)

#Difference between people with denied visa and people with certified visa

#In median wage

median(df$Prevailing_Wage[df$Case_Status=="Denied"])
median(df$Prevailing_Wage[df$Case_Status=="Certified"])

#In ratio full time/part time

table(df$Full_Time_Position[df$Case_Status=="Denied"])
178/length(df$Case_Status[df$Case_Status=="Denied"])
# 5,75%

table(df$Full_Time_Position[df$Case_Status=="Certified"])
7385/length(df$Case_Status[df$Case_Status=="Certified"])
# 1,28%

##Machine Learning
#Model on data job, using data transformation to create numerical data

data_df <- subset(df, grepl("data", Job_Title))
data_df <- subset(data_df, Case_Status %in% c("Certified","Denied"))

#ratio of denial company wise
for (i in 1:length(data_df$Employer_Name)){
    data_df$comp_ratio[i] <- comp_count$ratio[comp_count$Var1==data_df$Employer_Name[i]]
}
#ratio of denial state wise
data_df$state_ratio <- 0
for (i in 1:length(data_df$Visa_Class)){
  if (data_df$State[i] %in% state_count$state){
    data_df$state_ratio[i] <- state_count$ratio[state_count$state==data_df$State[i]]
  }
}

#ratio of denial job wise
data_df$job_ratio <- 0
for (i in 1:length(data_df$Visa_Class)){
    data_df$job_ratio[i] <- job_count$ratio[job_count$Var1==data_df$Job_Title[i]]
}

#if employer country = US

data_df$Full_Time_Position[data_df$Full_Time_Position=="Y"]<-1 
data_df$Full_Time_Position[data_df$Full_Time_Position=="N"]<-0
data_df$Quarter[data_df$Quarter=="Q1"]<-1 
data_df$Quarter[data_df$Quarter=="Q2"]<-2
data_df$Quarter[data_df$Quarter=="Q3"]<-3 
data_df$Quarter[data_df$Quarter=="Q4"]<-4 

data_df = subset(data_df, select = c(Quarter, Full_Time_Position, Prevailing_Wage, job_ratio, comp_ratio, state_ratio, Case_Status))

#rescale salary
library(scales)
data_df$Prevailing_Wage <- rescale(data_df$Prevailing_Wage)
table(data_df$Case_Status)
#Class Distribution
barplot(prop.table(table(data_df$Case_Status)),
        col = rainbow(4),
        ylim = c(0, 1),
        main = "Class Distribution")

#train and test set
data_df$Case_Status <- as.factor(data_df$Case_Status)
set.seed(123)
ind <- sample(2, nrow(data_df), replace = TRUE, prob = c(0.8, 0.2))
train <- data_df[ind==1,]
test <- data_df[ind==2,]

#balance train data
library(caret)
library(class)
library(MLmetrics)

train <- downSample(x = train[, -8],
                     y = train$Case_Status)

barplot(prop.table(table(train$Class)),
        col = rainbow(4),
        ylim = c(0, 0.6),
        main = "New Class Distribution")

X_train <- train[,-8]
y_train <- train$Class
X_test <- test[,-8]
y_test <- test$Case_Status

#PCA on balanced data
library(ggfortify)
pca_res <- prcomp(data_df[3:6], scale. = TRUE)
autoplot(pca_res, data = data_df, colour="Case_Status")

#classifiers
#knn 

knnModel <- train(
  Class ~ ., 
  data = train, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = c(3,5,7,10,15))
)

best_model<- knn3(
  Class ~ .,
  data = train,
  k = knnModel$bestTune$k
)

predictions <- predict(best_model, X_test,type = "class")

# Calculate confusion matrix and metrics
table(y_test)
cm_knn <- confusionMatrix(predictions, y_test)
cm_knn$overall["Accuracy"] #100%
cm_knn
F1_Score(y_pred = predictions, y_true = y_test) #100%

#SVM
library(e1071)

#Train and Tune the SVM
svmModel <- svm(x=X_train[3:6], y=y_train, kernel ="radial", degree = 3,
    coef0 = 0, cost = 1, nu = 0.5) #exclude factors variables

predictions <- predict(svmModel, X_test[3:6],type = "class")
cm_svm <- confusionMatrix(predictions, y_test)
cm_svm
cm_svm$overall["Accuracy"] #95%
F1_Score(y_pred = predictions, y_true = y_test) #97.43%

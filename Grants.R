##Harvard X Capstone project:  Choose Your Own
##Alberta Government Grant Data
##Alana Schoepp, June 9, 2020


if (!require(tidyr)) install.packages('tidyr')
library(tidyr)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(stringr)) install.packages('stringr')
library(stringr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)

#***************************************************
#Can we predict large funding allocations?
#***************************************************

#Read the data and 
#grants = read.csv("C://Users/Alana.Schoepp/OneDrive - Shell/Documents/edX/CYO Data/grants.csv")
#grants = filter(grants, Amount>100)  #Study grants that are greater than $100
#grants = filter(grants, LotteryFund==TRUE)
#write.csv(grants, "C://Users/Alana.Schoepp/OneDrive - Shell/Documents/edX/CYO Data/grantsSmall.csv")
grants = read.csv("~/edX/CYO Grants/grantsSmall.csv")


#=======================Data wrangling
#create a column, "yearstart", that is the beginning year of the fiscal year range ("DisplayFiscalYear")
grants  = grants %>% 
  mutate(yearstart = word(as.character(DisplayFiscalYear), 1))  #create start year of fiscal year

#change the PaymentDate to a date 
grants$PaymentDate = as_date(as.character(grants$PaymentDate), format="%M/%d/%Y", tz="UTC")

#Create the amountbin feature to flag a high grant allocation
grants$amountbin = as.factor(case_when(
   grants$Amount<=50000 ~ "50+",
  TRUE ~ "50-" 
 ))

#Drop columns unrelated to the study.  
drops = c( "DisplayFiscalYear")
grants = grants[(names(grants) %in% drops)==FALSE]


#=======================Data exploration/Data visualziation
#understand how many factors there are in each feature
length(unique(grants$Ministry))
length(unique(grants$BUName))
length(unique(grants$Recipient))
length(unique(grants$Program))
length(unique(grants$LotteryFund))

#Quantify NA data
which(is.na(grants$Ministry))  #none
which(is.na(grants$BUName))    #none
which(is.na(grants$Recipient)) #none
which(is.na(grants$Program))  #none
length(which(is.na(grants$LotteryFund)))  #44012 are NA

#summarize the grant amounts
grants %>% summary()
length(which(grants$Amount>1000000))/dim(grants)[1]*100  #1.8% of grants are greater than 1 million
length(which(grants$Amount<0))  #Some grants are -ve
length(which(grants$Amount<100)) #some grants are very small (even less than $1)

#understand how many years in the data and if the month data is meaningful
unique(grants$yearstart)  #The data has 6 years:  2014 - 2019
unique(month(grants$PaymentDate))  #All payments are made in July

#plot histogram of grants (include 98.2% of grants)
grants %>% 
  ggplot(aes(Amount)) + 
  geom_histogram( color = "black") +
  xlim(0, 1000000 ) +
  ylim(0,10000)

#print median values
median(grants$Amount)  # $1600
View(grants[order(-grants$Amount),])


#========================== scatter plots
 #plot mean amount by features Ministry, BUName, yearstart, Recipient and Program
 grants %>%
  group_by(Ministry) %>%
  summarize(meanAmount = mean(Amount)) %>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=Ministry))

grants %>%
  group_by(BUName) %>%
  summarize(meanAmount = mean(Amount))%>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=BUName))

grants %>%
  group_by(yearstart) %>%
  summarize(meanAmount = mean(Amount))%>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=yearstart))

grants %>%
  group_by(Recipient) %>%
  summarize(meanAmount = mean(Amount))%>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=Recipient))

grants %>%
  group_by(LotteryFund) %>%
  summarize(meanAmount = mean(Amount))%>%
  #filter(n()>=100) %>%
  ggplot(aes(meanAmount)) + 
  geom_histogram(bins = 30, color = "black")

grants %>%
  group_by(Ministry) %>%
  summarize(meanAmount = mean(Amount)) %>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=Ministry))

grants %>%
  group_by(Program) %>%
  summarize(meanAmount = mean(Amount))%>%
  ggplot() %>%  + 
  geom_point(aes(x=meanAmount, y=Program))


#========================Examine influence of time with Scatterplots
#plot amount by PaymentDate and the yearstart
grants %>%
  ggplot(aes(x=PaymentDate, y=Amount)) + 
  geom_point() +
  ylim(0,1000000000)

#plot by date
grants %>%
  ggplot(aes(x=yearstart, y=Amount)) + 
  geom_point() +
  geom_smooth(color="red", span=0.15, method=loess)

#===========================================Model
# generate training and test sets
set.seed(9, sample.kind = "Rounding")
test_index <- createDataPartition(grants$Amount, times = 1, p = 0.5, list = FALSE)
test_set <- grants[test_index, ]
train_set <- grants[-test_index, ]

#========================random forest=======================
#Model 1:  randomForest algorithm with Ministry feature.  Calculate the model, generate predictions and quantify the accuracy
train_rf <- randomForest(amountbin ~ Ministry, data=train_set)
z=predict(train_rf, test_set)
acc1 = confusionMatrix(z, test_set$amountbin)$overall["Accuracy"]
acc_results <- tibble(method = "Random Forest (randomForest) + Ministry ", Accuracy = acc1)

#Model 2:  randomForest algorithm with Ministry and yearstart features.  Calculate the model, generate predictions and quantify the accuracy
train_rf <- randomForest(amountbin ~ yearstart+Ministry, data=train_set)
z=predict(train_rf, test_set)
acc2 = confusionMatrix(z, test_set$amountbin)$overall["Accuracy"]
acc_results <- bind_rows(acc_results, data_frame(method="Random Forest + Ministry + year", Accuracy = acc2 ))

#Model 3:  randomForest algorithm with Ministry, year and BUName features.  Calculate the model, generate predictions and quantify the accuracy
train_rf <- randomForest(amountbin ~ Ministry+yearstart+BUName, data=train_set)
z=predict(train_rf, test_set)
acc3 = confusionMatrix(z, test_set$amountbin)$overall["Accuracy"]
acc_results <- bind_rows(acc_results,  data_frame(method="Random Forest + Ministry + year + BUName",  Accuracy = acc3 ))

#Tune the random forest model 
#tuneRF(train_set$amountbin, train_set[,], mtryStart, ntreeTry=50, stepFactor=2, improve=0.05,
       #trace=TRUE, plot=TRUE, doBest=FALSE, ...)

#===========================Try caret random forest on  ministry and year
#Try the caret implementation of the random forest algorithm with year and ministry features
#fit the model, predict the binned amounts, report the final accuracy, mtry value and the variable importance
train_rf <- train(amountbin ~ yearstart+Ministry,  method = "rf", data = train_set)
y_hat_rf <- predict(train_rf, test_set)
acc_rf = confusionMatrix(y_hat_rf, test_set$amountbin)$overall["Accuracy"]
train_rf$bestTune
varImp(train_rf)
acc_results <- bind_rows(acc_results,data_frame(method="Caret rf + Ministry + year", Accuracy = acc_rf ))


#========================Loess on the year
#Try the caret implementation of the loess algorithm with year feature
#fit the model, predict the binned amounts, report the final accuracy, mtry value and the variable importance
train_loess <- train(as.factor(amountbin) ~ Ministry,  method = "gamLoess", data = train_set)
y_hat_loess <- predict(train_loess, test_set)
acc_loess = confusionMatrix(y_hat_loess, test_set$amountbin)$overall["Accuracy"]
acc_results <- bind_rows(acc_results,data_frame(method="Caret loess + year", Accuracy = acc_loess ))


#============================Results
#print the results
print("The accuracy of the models is:")
print(acc_results)
print(varImp(train_rf))

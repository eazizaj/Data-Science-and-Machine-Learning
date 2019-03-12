library(ggplot2)
library(reshape2)
library(arules)
library(fpc)
library(kernlab)
library(caret)
#https://www.kaggle.com/arhart/improving-adoption-rates

setwd("/Users/mymacbook/Desktop/toti/hws/hw3/")

train_data <- read.csv("shelter_cat_outcome_training.csv", na.strings = c("NA",""), stringsAsFactors = F)
test_data <- read.csv("shelter_cat_outcome_testing.csv", na.strings = c("NA",""), stringsAsFactors = F)



#------------------Support Vector Machine-----------------
#modifying data and converting to the right type of data
train_data$breed.fix <- as.numeric(as.factor(train_data$breed))
train_data$color.fix <- as.numeric(as.factor(train_data$color))
train_data$outcome_weekday.fix <- as.numeric(as.factor(train_data$outcome_weekday))

train_data$coat_pattern <- ifelse(is.na(train_data$coat_pattern), "EMPTY", train_data$coat_pattern)
train_data$coat_pattern.fix <- as.numeric(as.factor(train_data$coat_pattern))

train_data$name.fix <- ifelse(is.na(train_data$name), 0, 1)
train_data$sex.fix <- as.numeric(as.factor(train_data$sex))
train_data$Spay.Neuter.fix <- as.numeric(as.factor(train_data$Spay.Neuter))
train_data$age_group.fix <- as.numeric(as.factor(train_data$age_group))
train_data$outcome_weekday.fix <- as.numeric(as.factor(train_data$age_group))
#



#lets keep only the variables we need
train_set <- train_data[,c(9,5,6,13:15,20:21,23:24, 26:34)]


#lets keep only the categories required
train_set <- subset(train_set, outcome_type%in%c("Transfer","Adoption","Return to Owner","Euthanasia"))

#convert the class variable to categories
train_set$outcome_type <- as.factor(train_set$outcome_type)

#lets chech the data types now
str(train_set)

#get rid of NAs
train_set <- na.omit(train_set)

summary(train_set)
#set.seed(2348)
#train_set$random <- runif(nrow(train_set))
#sample_data <- subset(train_set, random<=0.05)
#sample_data$random <- NULL
#train_set$random <- NULL

train_scaled <- data.frame(train_set$outcome_type, as.data.frame(scale(train_set[-1])))

#------------------model by SVM linear------------
mSVM <- train(train_set.outcome_type~.,  #outcome_type
              data=train_scaled, #train_set
              'svmLinear',
              trControl=trainControl(method='cv',number=10))
print(mSVM)
#  Accuracy   Kappa    
#0.7340792  0.5136052
variables <- colnames(train_set)

#lets try to remove variables one by one and check if performance changes
mSVM_1<- train(outcome_type~domestic_breed+Periods+Period.Range+outcome_age_.days.+dob_year+
                 dob_month+outcome_month+outcome_year+outcome_hour+breed.fix+color.fix+
                 coat_pattern.fix+name.fix+sex.fix+Spay.Neuter.fix,
              data=sample_data,
              'svmLinear',
              trControl=trainControl(method='cv',number=10))

print(mSVM_1)

#cfa_breed+domestic_breed+Periods+Period.Range+outcome_age_.days.+dob_year+
#dob_month+outcome_month+outcome_year+outcome_hour+breed.fix+color.fix+outcome_weekday.fix+
#  coat_pattern.fix+name.fix+sex.fix+Spay.Neuter.fix+age_group.fix,

#------------------model by SVM radial------------
mSVM_radial_2 <- train(train_set.outcome_type~., #outcome_type
                       data=train_scaled,       #train_set
                       'svmRadial',
                       trControl=trainControl(method='cv',number=10))
print(mSVM_radial_2)


#lets try to remove variables one by one and check if performance changes
rmSVM_radial_2 <- train(train_set.outcome_type~domestic_breed+Periods+Period.Range+outcome_age_.days.+dob_year+
                       dob_month+outcome_month+outcome_year+outcome_hour+breed.fix+color.fix+outcome_weekday.fix+
                       coat_pattern.fix+name.fix+sex.fix+Spay.Neuter.fix+age_group.fix,
                     data=train_scaled,
                     'svmRadial',
                     trControl=trainControl(method='cv',number=10))
print(rmSVM_radial_2)

#--------------------------Random Forests-------------------
#lets keep only the variables we need
train_data_2 <- train_data[,c(9,3:7,11:15,19:21,23:24, 25:26, 31)]

train_set_2 <- subset(train_data_2, outcome_type%in%c("Transfer","Adoption","Return to Owner","Euthanasia"))

#converting to the right type
train_set_2$outcome_type <- as.factor(train_set_2$outcome_type)
train_set_2$breed <- as.factor(train_set_2$breed)
train_set_2$color <- as.factor(train_set_2$color)
train_set_2$outcome_weekday <- as.numeric(as.factor(train_set_2$outcome_weekday))
train_set_2$coat_pattern <- as.factor(train_set_2$coat_pattern)
train_set_2$coat_pattern <- as.factor(train_set_2$coat_pattern)
train_set_2$sex <- as.factor(train_set_2$sex)
train_set_2$Spay.Neuter <- as.factor(train_set_2$Spay.Neuter)
train_set_2$age_group <- as.factor(train_set_2$age_group)


str(train_set_2)

train_set_2 <- na.omit(train_set_2)



#seperate the training set into trainig and validation set,
set.seed(4578)
inTraining <- createDataPartition(train_set_2$outcome_type, p=0.8, list = FALSE) #80% of the training data will be training set

#actual actual traing set have 605 observations
training <- train_set_2[inTraining,]
str(training)

#validation set has 151
validation <- train_set_2[-inTraining,] #test model on the validation set and see how good the model is
str(validation)

#resetting the seed
set.seed(73)
trctrl <- trainControl(method = "cv", number = 10) # 10 fold
randomf_fit <- train(outcome_type ~., 
                     data = training, method = "rf",   #notice the difference of the method, is "rf"() instead of "rpart"
                     parms = list(split = "information"),
                     metric="Accuracy",        #Kappa because the dataset was unbalanced
                     trControl=trctrl,
                     tuneLength = 10)
print(randomf_fit)

#test it on the validation set
validation$pred_outcome_type <- predict(randomf_fit, newdata = validation) #test_data
confusionMatrix(as.factor(validation$pred_outcome_type), as.factor(validation$outcome_type))  #check accuracy


#testin on testing set
#lets make the same modifications as before
test_data$name.fix <- ifelse(is.na(test_data$name), 0, 1)

str(test_data)

#keep only variables interested 
test_data_2 <- test_data[,c(2:7,10:14, 18:20, 22:26)]

#test_data[,colnames(train_data_2)[2:19]]

str(test_data_2)
#we need to get rid of new levels of data that the model does not recognize
test_data_2 <- test_data_2[!test_data_2$breed%in%c("domestic shorthair/siamese",
                                                   "domestic longhair/domestic longhair","havana brown"),]

#get rid of these 10 categories since they are not known by the model
test_data_2 <- test_data_2[!test_data_2$color%in%c("black /black","brown /blue","chocolate/brown","flame /cream",
                                             "lynx /tan","orange /","orange /brown", "silver /black",
                                              "silver lynx /gray","white/lilac",
                                             "blue cream/buff", "blue/cream", "blue/gray", "orange tiger", "sable"),]

#make same modification oas before
test_data_2$coat_pattern <- ifelse(is.na(test_data_2$coat_pattern), "EMPTY", test_data_2$coat_pattern)

test_data_2 <- na.omit(test_data_2)

#now lets apply it to the testing set but before we have to convert it variables to the right category
test_data_2$breed <- as.factor(test_data_2$breed)
test_data_2$color <- as.factor(test_data_2$color)
test_data_2$outcome_weekday <- as.numeric(as.factor(test_data_2$outcome_weekday))
test_data_2$coat_pattern <- as.factor(test_data_2$coat_pattern)
test_data_2$coat_pattern <- as.factor(test_data_2$coat_pattern)
test_data_2$sex <- as.factor(test_data_2$sex)
test_data_2$Spay.Neuter <- as.factor(test_data_2$Spay.Neuter)
test_data_2$age_group <- as.factor(test_data_2$age_group)


str(test_data_2)




#now predict the class on the testin set
test_data_2$pred_outcome_type <- predict(randomf_fit, newdata = test_data_2) 

write.csv(test_data_2,"test_data_2.csv")





#--------------------------Naive Bayes-------------------
library(e1071)
model_nb = naiveBayes(outcome_type ~., data=training)

print(model_nb)

library(klaR)
library(httpuv)
train_data_3 <- na.omit(train_data)
model_nb_2 <- train(outcome_type ~., data=train_data_3,'nb',trControl=trainControl(method='cv',number=10))

print(model_nb_2)





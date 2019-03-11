library(ggplot2)
library(graphics)
library(gridExtra)
library(stargazer)

setwd("/Users/mymacbook/Desktop/toti/hws/hw1/")

data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", na.strings=c('NA','','?'), stringsAsFactors = F)
colnames(data) <- c("mpg", "cylinders","displacement", "horsepower","weight", "acceleration", "model_year", "origin", "car_name" )

sapply(data, class)
summary(data)

#checking the number of missing values
sum(is.na(data))

#specifying data types
data$cylinders <- factor(data$cylinders)
data$model_year <- factor(data$model_year)
data$origin <- factor(data$origin)
sapply(data, class)
summary(data)

#removing NA`s
data_cleaned <- na.omit(data)
summary(data_cleaned)

#computing mean and sd for numerical attributes
means <- as.table(sapply(data_cleaned[,c(1,3,4,5,6)], mean))
std_dev <- as.table(sapply(data_cleaned[,c(1,3,4,5,6)], sd))
table <- rbind(means, std_dev)
print(table)

#computing the correlation matrix
corr_matrix <- cor(data_cleaned[-c(2,7,8,9)])
stargazer(corr_matrix)



#scaterplot for mpg and horsepower
par(mfrow=c(2,2))
p1 <- ggplot(data_cleaned, aes(mpg, horsepower)) + geom_point()
p2 <- ggplot(data_cleaned, aes(displacement, weight)) + geom_point()
p3 <- ggplot(data_cleaned, aes(mpg, horsepower, colour = cylinders)) + geom_point()
p4 <- ggplot(data_cleaned, aes(displacement, weight, colour = cylinders)) + geom_point() + guides(colour="none")

grid.arrange(p1,p2,p3,p4,nrow=2)

#histograms and density plots for 3, 4 and 5th attribute
sapply(data_cleaned[,c(3,4,5)], sd)
p5 <- ggplot(data_cleaned) + geom_histogram( aes(displacement), binwidth=50, fill="gray")
p6 <- ggplot(data_cleaned, aes(displacement)) + geom_density()
grid.arrange(p5,p6,ncol=1)

p7 <- ggplot(data_cleaned) + geom_histogram(aes(horsepower), binwidth=18, fill="gray")
p8 <- ggplot(data_cleaned, aes(horsepower)) + geom_density()
grid.arrange(p7,p8,ncol=1)

p9 <- ggplot(data_cleaned) + geom_histogram(aes(weight), binwidth=405, fill="gray")
p10 <- ggplot(data_cleaned, aes(weight)) + geom_density()
grid.arrange(p9,p10,ncol=1)


#proble 6 boxplots
p11 <- ggplot(data_cleaned, aes(x="",y=horsepower)) + geom_boxplot()
p12 <- ggplot(data_cleaned, aes(x="",y=horsepower/as.numeric(as.factor(cylinders)))) + geom_boxplot()
p13 <- ggplot(data_cleaned, aes(x="",y=horsepower/as.numeric(as.factor(origin)))) + geom_boxplot()
grid.arrange(p11,p12,p13, nrow=1)

p14 <- ggplot(data_cleaned, aes(x="",y=weight)) + geom_boxplot()
p15 <- ggplot(data_cleaned, aes(x="",y=weight/as.numeric(as.factor(cylinders)))) + geom_boxplot()
p16 <- ggplot(data_cleaned, aes(x="",y=weight/as.numeric(as.factor(origin)))) + geom_boxplot()
grid.arrange(p14,p15,p16, nrow=1)

p17 <- ggplot(data_cleaned, aes(x="",y=acceleration)) + geom_boxplot()
p18 <- ggplot(data_cleaned, aes(x="",y=acceleration/as.numeric(as.factor(cylinders)))) + geom_boxplot()
p19 <- ggplot(data_cleaned, aes(x="",y=acceleration/as.numeric(as.factor(origin)))) + geom_boxplot()
grid.arrange(p17,p18,p19, nrow=1)


#problem 7
#Splitting into test and training datasets using a random group mark
set.seed(456)
data_cleaned$gp <- runif(dim(data_cleaned)[1])
test_data <- subset(data_cleaned, data_cleaned$gp <= 0.25) # 25% of the data to test the model
training_data <- subset(data_cleaned, data_cleaned$gp > 0.25) # 75% of the data to train the model
sum(data_cleaned$gp>=0.25)/dim(data_cleaned)[1]

# part (b)
model1 <- lm(mpg ~ horsepower, data = training_data)
summary(model1)
coef(model1)
stargazer(model1)



training_data$pred_mpg <- predict(model1, newdata = training_data)

#R^2 function
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) } #y is the actual, f is the prediction
rsq(training_data$mpg ,training_data$pred_mpg)


#RMSE function
rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) } #y is the actual, f is the prediction
rmse(training_data$mpg, training_data$pred_mpg)

#part(d)
test_data$pred_mpg <- predict(model1, newdata = test_data)
rsq(test_data$mpg ,test_data$pred_mpg)
rmse(test_data$mpg ,test_data$pred_mpg)

#part(e)

model2 <- lm(mpg ~ horsepower + cylinders + displacement + weight + acceleration + model_year + origin, data = training_data)
summary(model2)

training_data$pred_mpg2 <- predict.lm(model2, newdata = training_data)
rsq(training_data$mpg ,training_data$pred_mpg2)
rmse(training_data$mpg ,training_data$pred_mpg2)

test_data$pred_mpg2 <- predict(model2, newdata = test_data)
rsq(test_data$mpg ,test_data$pred_mpg2)
rmse(test_data$mpg ,test_data$pred_mpg2)

#part(f)
model3 <- lm(mpg ~ horsepower + cylinders + weight + model_year + origin, data = training_data)
summary(model3)

training_data$pred_mpg3 <- predict.lm(model3, newdata = training_data)
rsq(training_data$mpg ,training_data$pred_mpg3)
rmse(training_data$mpg ,training_data$pred_mpg3)

test_data$pred_mpg3 <- predict(model3, newdata = test_data)
rsq(test_data$mpg ,test_data$pred_mpg3)
rmse(test_data$mpg ,test_data$pred_mpg3)












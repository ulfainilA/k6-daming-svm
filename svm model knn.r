data <- read.csv("train.csv", header = TRUE, sep = ",")
str(data)
summary(data)

boxplot(data$battery_power, main="Boxplot of Battery Power",
        ylab="battery_power")

boxplot(data$clock_speed, main="Boxplot of Clock Speed",
        ylab="clock_speed") 

boxplot(data$fc, main="Boxplot of Front Camera",
        ylab="fc") 

boxplot(data$int_memory, main="Boxplot of Internal Memory",
        ylab="int_memory") 

#mengetahui jumlah data yang mempunya missing value
library(mice)
md.pattern(data)


#cek outlier
outliers <- boxplot(data$battery_power, plot=FALSE)$out
outliers2 <- boxplot(data$clock_speed, plot=FALSE)$out
outliers3 <- boxplot(data$fc, plot=FALSE)$out
data<- data[-which(data$fc %in% outliers3),]

outliers4 <- boxplot(data$int_memory, plot=FALSE)$out
outliers5 <- boxplot(data$m_dep, plot=FALSE)$out
outliers6 <- boxplot(data$mobile_wt, plot=FALSE)$out
outliers7 <- boxplot(data$n_cores, plot=FALSE)$out
outliers8 <- boxplot(data$pc, plot=FALSE)$out
outliers9 <- boxplot(data$px_height, plot=FALSE)$out
data<- data[-which(data$px_height %in% outliers9),]

outliers10 <- boxplot(data$px_width, plot=FALSE)$out
outliers11 <- boxplot(data$ram, plot=FALSE)$out
outliers12 <- boxplot(data$sc_h, plot=FALSE)$out
outliers13 <- boxplot(data$sc_w, plot=FALSE)$out
outliers14 <- boxplot(data$talk_time, plot=FALSE)$out

library(class)
library(caret)
library(e1071)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(scales)
library(corrplot)

unique(data$price_range)

#data <- subset(data, select = -price_range)

a <-cor(data)
corrplot(a, method="color")

#data$price_range <- as.factor(data$price_range)
#data$dual_sim <- as.factor(data$dual_sim)
#data$four_g <- as.factor(data$four_g)
#data$blue <- as.factor(data$blue)
#data$touch_screen <- as.factor(data$touch_screen)

data$blue <- as.numeric(data$blue)
data$dual_sim <- as.numeric(data$dual_sim)
data$four_g <- as.numeric(data$four_g)
data$touch_screen <-as.numeric(data$touch_screen)
#data$price_range <- as.numeric(data$price_range)

smp_size <- floor(0.7 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
valid <- data[-train_ind, ]

x_train <- subset(train, select = -price_range)
y_train <- train$price_range
x_valid <- subset(valid, select = -price_range)
y_valid <- valid$price_range


#SVM Model
model <- svm(x_train,y_train, type = 'C-classification', 
             kernel = 'linear') 

print(model)
summary(model)

#evaluasi model
predvalid <- predict(model, x_valid)
predvalid <- as.factor(predvalid)

y_valid <- as.factor(y_valid)
confusionMatrix(y_valid,predvalid)

outOfSampleAccuracy <- sum(predvalid == valid$price_range)/length(predvalid)
outOfSampleError <- (1 - outOfSampleAccuracy)
print(outOfSampleAccuracy)
print(outOfSampleError)


#buat datatest
sampletest <- floor(0.5 * nrow(data))
test_ind <- sample(seq_len(nrow(data)), size=sampletest)
test <- data[test_ind, ]
test <- subset(test, select = -price_range)
test

predtest <- predict(model, test)
predtest

price_test <- c(predtest)
test$price_test <- price_test
test

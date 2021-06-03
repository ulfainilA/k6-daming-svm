data <- read.csv("train.csv", header = TRUE, sep = ",")
str(data)
summary(data)

#mengetahui jumlah data yang mempunya missing value
library(mice)
md.pattern(data)

library(infotheo)
#melakukan diskretisasi pada atribut quality dengan membagi menjadi 3 kategori
#dengan metode equal width
ew.price <- discretize(data$price_range, "equalwidth", 3)
ew.price

#Gabungkan hasil diskretisasi dengan data hasil praproses nomor 1
ew.price$x = as.factor(ew.price$X) #menggabungkan atribut baru kedalam data
data$price_discre = ew.price$X #memberi nama pada atribut baru
data

#transformasi
data$price_trans<-NULL
data$price_trans[data$price_discre == 1 ] <- "low"
data$price_trans[data$price_discre == 2 ] <- "medium"
data$price_trans[data$price_discre == 3 ] <- "high"
data

#cek outlier
boxplot(data$battery_power)
title("Boxplot of battery power")

boxplot(data$fc)
title("Boxplot of fc")

boxplot(data$mobile_wt)
title("Boxplot of mobile wt")

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

#normalisasi data
norm <-function(x){
  (x-min(x))/(max(x)-min(x))
}
data$fc = norm(data$fc)
data$clock_speed = norm(data$clock_speed)
data$int_memory = norm(data$int_memory)
data$mobile_wt = norm(data$mobile_wt)
data$n_cores = norm(data$n_cores)
data$pc = norm(data$pc)
data$px_height = norm(data$px_height)
data$px_width = norm(data$px_width)
data$ram = norm(data$ram)
data$sc_h = norm(data$sc_h)
data$sc_w = norm(data$sc_w)
data$talk_time = norm(data$talk_time)

library(class)
library(caret)
library(e1071)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(scales)
library(corrplot)
unique(data$price_discre)

data <- subset(data, select = -price_trans)

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
data$price_range <- as.numeric(data$price_range)

smp_size <- floor(0.75 * nrow(data))

# set the seed to make our partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

x_train <- subset(train, select = -price_range)
y_train <- train$price_range
x_test <- subset(test, select = -price_range)
y_test <- test$price_range

#SVC
model <- svm(x_train,y_train, type = 'C-classification', 
             kernel = 'linear') 

print(model)
summary(model)

# testing our model
pred <- predict(model, x_test)

pred <- as.factor(pred)

y_test <- as.factor(y_test)
confusionMatrix(y_test,pred)

#KNN-CLASSIFIER
train_scale <- scale(train[, 1:20])
test_scale <- scale(test[, 1:20])

#fitting
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train$price_range, #cl arguement
                      k = 45)


# Confusion Matrix
cm <- table(test$price_range, classifier_knn)
cm

#MODEL EVALUATION AND CALCULATING SAMPLE ERROR
# K = 45

misClassError <- mean(classifier_knn != test$price_range)
(paste('Accuracy =', 1-misClassError))

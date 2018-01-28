getwd()
setwd("/Users/chen/downloads")
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

#Will split data into training, testing and validataion
library(caret)
set.seed(200)

inTrain = createDataPartition(training$classe, p = 0.8, list = FALSE)
test <- training[-inTrain,]
Tr <- training[inTrain,]
intrain2 = createDataPartition(Tr$classe, p = 0.75, list = FALSE)
training2 <- Tr[intrain2,]
valid <- Tr[-intrain2,]

#Inspect data

summary(training2)
str(training2)
summary(training2$classe)


# # I can see many factor variables are in fact numerical. I will convert them to numerical
# 
# num <- c('kurtosis_roll_belt', 'kurtosis_picth_belt', 'kurtosis_yaw_belt', 'skewness_roll_belt',
#          'skewness_roll_belt.1', 'max_yaw_belt', 'min_yaw_belt', 'kurtosis_roll_arm', 'kurtosis_picth_arm',
#          'kurtosis_yaw_arm', 'skewness_roll_arm', 'skewness_pitch_arm', 'skewness_yaw_arm',
#          'kurtosis_roll_dumbbell', 'kurtosis_picth_dumbbell', 'skewness_roll_dumbbell', 'skewness_pitch_dumbbell',
#          'max_yaw_dumbbell', 'min_yaw_dumbbell', 'kurtosis_roll_forearm', 'kurtosis_picth_forearm',
#          'skewness_roll_forearm', 'skewness_pitch_forearm', 'max_yaw_forearm', 'min_yaw_forearm'
#          )
# training[num] <- lapply(training[num], as.character)
# training[num] <- lapply(training[num], as.numeric)
# str(training)


lose_col <- c('X','user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 
          'new_window', ' kurtosis_roll_belt', 'kurtosis_picth_belt', 'kurtosis_yaw_belt',
          'skewness_roll_belt', 'skewness_roll_belt.1', ' skewness_yaw_belt', 'max_yaw_belt',
          'min_yaw_belt', 'amplitude_yaw_belt', 'kurtosis_roll_arm', 'kurtosis_picth_arm', 
          'kurtosis_yaw_arm', 'skewness_roll_arm', 'skewness_pitch_arm', 'skewness_yaw_arm',
          'kurtosis_roll_dumbbell', 'kurtosis_picth_dumbbell', 'kurtosis_yaw_dumbbell', 
          'skewness_roll_dumbbell', 'skewness_pitch_dumbbell', 'skewness_yaw_dumbbell',
          'max_yaw_dumbbell', 'min_yaw_dumbbell', 'amplitude_yaw_dumbbell', 'yaw_forearm',
          'kurtosis_roll_forearm', 'kurtosis_picth_forearm', 'kurtosis_yaw_forearm', 
          'skewness_roll_forearm', 'skewness_pitch_forearm', 'skewness_yaw_forearm', 'max_yaw_forearm',
          'min_yaw_forearm' ,'amplitude_yaw_forearm', 'skewness_yaw_belt', 'kurtosis_roll_belt')


#Inspecting some variables with a high amount of missing values to see if i should remove

table(training2$classe, training2$new_window)
table(training2$classe, training2$kurtosis_roll_belt)
table(training2$classe, training2$kurtosis_yaw_belt)

#Removing columns

training2 <- training2[ , !(names(training2) %in% lose_col)]
str(training2)
str(training2[,90:123])

#There are still a lot of useless columns

colsToDel = names(training2)[colMeans(is.na(training2))>0.9]
training2 <- training2[ , !(names(training2) %in% colsToDel)]
str(training2)


# 1 Fit RF model and gbm

mod1 <- train(classe ~., method= 'rf', data = training2,na.action = na.exclude)
predrf <- predict(mod1, test)
test$rfright <- test$classe == predrf
mean(test$rfright)

# I was planning on doing an ensemble model but the results were so good, 99.6% 
# accuracy on test that i will just stay with the random forest

# Test on validation set

predrf2 <- predict(mod1, valid)
valid$rfright <- valid$classe == predrf2
mean(valid$rfright)

# Also very high accuracy at 99.7%

# answers for the quiz
predrf3 <- predict(mod1, testing)
predrf3


# # 2 Fit GBM
# mod2 <- train(classe ~., method= 'gbm', data = training2, verbose = FALSE,na.action = na.exclude)
# predrf2 <- predict(mod2, test)
# test$gbmright <- test$classe == predrf2
# mean(test$gbmright)
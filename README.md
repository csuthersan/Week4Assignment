# Week4Assignment
Submission for Practical Machine Learning Assignment

## Steps taken to obtain the answer
My plan originally was to download the data then split into training, test and validation sets
After that I was going to do an stacked prediction of Random Forest and GBM
But after I did Random Forest I got an accuaracy of 99.6% so I decided that I didn't need to do the other files

## Split into training, test and validation

This was done with the following code

library(caret)
set.seed(200)

inTrain = createDataPartition(training$classe, p = 0.8, list = FALSE)
test <- training[-inTrain,]
Tr <- training[inTrain,]
intrain2 = createDataPartition(Tr$classe, p = 0.75, list = FALSE)
training2 <- Tr[intrain2,]
valid <- Tr[-intrain2,]

## Inspect data

summary(training2)
str(training2)
summary(training2$classe)

After looking at str, I say a whole bunch of factor variables that looked like numerical variables. 
I decided to convert them to numerical variables because i thought that would help me with my prediction
However this was a waste of time because these columns were largely full of missing values and I ended up removing these columns

## Removing factor columns with a lot of missing values

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


training2 <- training2[ , !(names(training2) %in% lose_col)]
str(training2)
str(training2[,90:123])

## There are still a lot of useless columns, which I then removed in a more systematic way

colsToDel = names(training2)[colMeans(is.na(training2))>0.9]
training2 <- training2[ , !(names(training2) %in% colsToDel)]
str(training2)


## 1 Fit RF model and gbm

mod1 <- train(classe ~., method= 'rf', data = training2,na.action = na.exclude)
predrf <- predict(mod1, test)
test$rfright <- test$classe == predrf
mean(test$rfright)

I was planning on doing an ensemble model but the results were so good, 99.6% accuracy on test that i will just stay with the random forest

## Test on validation set

predrf2 <- predict(mod1, valid)
valid$rfright <- valid$classe == predrf2
mean(valid$rfright)

Also very high accuracy at 99.7%

## answers for the quiz
predrf3 <- predict(mod1, testing)
predrf3


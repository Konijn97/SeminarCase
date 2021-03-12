#RECURRENT NEURAL NETWORK

#Download Packages
#install.packages("tidyverse")
library(tidyverse)          #Import package in R to use general utility functions
#install.packages("caret")
library(caret)              #Import package in R to use machine learning utility functions
#install.packages("dplyr")
library(dplyr)
#install.packages("keras")
library(keras)              #Import package in R to use deep learning
install_keras(tensorflow = "gpu")


#Set Model Parameters
lookback <- 360             #The number of previous observation that are used in the model (12 hours)
batch_size <- 128           #The number of samples used when training the data
epochs <- 10                #How many times the model looks at the whole data set while training
set.seed(34)                #Set the seed for reproduction




############################### THE NETHERLANDS ##############################
#Create Dummy for Advertisements
#TSNL$goodad[is.na(TSNL$add)] <- 0                            #Convert the NA's for bad advertisements to 0
add_effect = vector(length = length(TSNL$Freq))               #Create an empty vector for the effects of adds
for (i in 1:length(add_effect)){                              #Loop over this empty vector
  if(TSNL$ad[i]>=1){                                          #If there is a moment where there is a good add
    if (i<=length(TSNL$Freq)-5) {                             #And it is not near the end of the vector
      add_effect[i:(i+5)] = 1                                 #Set that moment and moments after to have an add effect
    } else {                                                  #If it is near the end of the vector
      add_effect[i:length(TSNL$Freq)] = 1                     #Stop marking the moments at the end of the vector
    }
  }
  else if(TSNL$ad[i]==0 & add_effect[i] == FALSE){            #If there is no add at that moment or shortly prior
    add_effect[i] = 0                                         #Set the dummy to zero
  }
}


#Create a Baseline
BL <- format(TSNL$date_time,"%H:%M")                          #Create baseline (BL) time series with only the time
BL <- as.data.frame(BL)                                       #Set BL as a data frame
BL <- cbind(BL, TSNL$Freq)                                    #Add the frequencies of adds to the BL data
colnames(BL) <-c("Time","Freq")                               #Rename the columns

for (i in 1:length(add_effect)){                              #Loop over the vector with add effects
  if (add_effect[i]==1){                                      #If there is no a effect
    BL$Freq[i] <- NA                                          #Replace the frequency for its' minute-based average
  }
}

for (i in 1:length(BL$Freq)){                                             #Loop over all frequencies
  if (is.na(BL$Freq[i])){                                                 #If a value is NA
    BL$Freq[i] <- mean(subset(BL$Freq, BL$Time==BL$Time[i]), na.rm=TRUE)  #Replace it with the minute-based mean
  }
}


#Normalize the BL Data
BL_nrm <- BL$Freq                                                   #Take the BL frequencies apart
BL_nrm <- scale(BL_nrm, center = mean(BL_nrm), scale = sd(BL_nrm))  #Scale this data
norm <- function(x) {                                               #Create a normalization function
  return ((x - min(x)) / (max(x) - min(x)))}
BL_nrm <- norm(BL_nrm)                                              #Normalize the BL data


#Create Sample Sequences
index <- seq(1, length(BL_nrm) - lookback)                          #Create the indexes for overlapping sample sequences
Smpl_Seq <- matrix(nrow = length(index), ncol = lookback+1)         #Create an empty matrix
for (i in 1:length(index)){                                         #Loop over all indexes
  Smpl_Seq[i,] <- BL_nrm[index[i]:(index[i] + lookback)]            #Fill in the matrix with the samples
}


#Split Data
X <- Smpl_Seq[,-ncol(Smpl_Seq)]                                                             #Create set with explanatory variables
Y <- Smpl_Seq[,ncol(Smpl_Seq)]                                                              #Create set with dependent variables
training_set <- createDataPartition(Y, p = .9, list = FALSE , times = 1)                    #Create a vector setting the training data
X_train <- array(X[training_set,], dim = c(length(training_set), lookback, 1))              #Create training data for X
Y_train <- Y[training_set]                                                                  #Create training data for Y
X_test <- array(X[-training_set,], dim = c(length(Y) - length(training_set), lookback, 1))  #Create testing data for X
Y_test <- Y[-training_set]                                                                  #Create testing data for Y


#Create a RNN
model <- keras_model_sequential() %>%                             #Setup a RNN model
  layer_gru(units = 32,                                           #Set the size of the hidden layer
            dropout = 0,                                          #Set the percentage of nodes that will be dropped
            recurrent_dropout = 0,                                #Set the percentage of connections that will be dropped
            input_shape = dim(X_train)[2:3]) %>%                  #Set the input shape for the input layer
  layer_dense(units = 1)                                          #Set the size for the output layer

model %>% compile(                                                #Set the specifics for the RNN
  optimizer = optimizer_rmsprop(),                                #Set the optimizer
  loss = 'mae',                                                   #Use the Mean Absolute Error (MAE) as loss function
)


#Train the Model
trained_model <- model %>% fit(
  x = X_train,                                                    #Set the explanatory training data 
  y = Y_train,                                                    #Set the dependent training data 
  batch_size = batch_size,                                        #Set how many samples are used simultaneously
  epochs = epochs,                                                #Set how many times we'll look at the whole data set
  validation_split = 0.1)                                         #Set how much data is hold out for testing during the process
plot(trained_model)


#Forecast data
Y_pred <- model %>% predict(X_test)                               #Forecast the data of the test sample
Y_MAPE <- abs((Y_test-Y_pred)/Y_test)*100                         #Calculate the Mean Absolute Prediction Error (MAPE)
Y_MAPE[Y_MAPE == Inf] <- NA                                       #If a value is infinite, remove it
summary(Y_MAPE, na.rm=TRUE)                                       #Show the summary of the MAPE

plot_data <- as.data.frame(cbind(Y_test, Y_pred))                 #Make a data frame of the data that will be plotted
ggplot(plot_data[1:150,], aes(x = seq(1:150))) +                  #Plot the data with the corresponding lines
  geom_line(aes(y = Y_test[1:150]), colour = "blue") +
  geom_line(aes(y = Y_pred[1:150]), colour = "red")


#MAPE Tests
#First make data with the original advertisements
All_nrm <- TSNL$Freq                                                    #Take all frequencies apart
All_nrm <- scale(All_nrm, center = mean(All_nrm), scale = sd(All_nrm))  #Scale these frequencies
All_nrm <- norm(All_nrm)                                                #Normalize these frequencies

index <- seq(1, length(All_nrm) - lookback)                             #Create the indexes for overlapping sample sequences
Smpl_Seq_All <- matrix(nrow = length(index), ncol = lookback+1)         #Create an empty matrix
for (i in 1:length(index)){                                             #Loop over all indexes
  Smpl_Seq_All[i,] <- All_nrm[index[i]:(index[i] + lookback)]           #Fill in the matrix with the samples
}


#MAPE of only adds
Smpl_Seq_Add <- Smpl_Seq_All[cbind(which(TSNL$goodad==1)-lookback),]    #Subset the sample sequences with only times with adds
Y_Add <- Smpl_Seq_Add[,ncol(Smpl_Seq_Add)]                              #Create set with dependent variables
X_Add <- array(Smpl_Seq_Add[,-ncol(Smpl_Seq_Add)], dim = c(length(Y_Add), lookback, 1)) #Create set with explanatory variables

Y_Add_pred <- model %>% predict(X_Add)                                  #Forecast the data of the times with adds
Y_Add_MAPE <- abs((Y_Add-Y_Add_pred)/Y_Add)*100                         #Calculate the MAPE
Y_Add_MAPE[Y_Add_MAPE == Inf] <- NA                                     #If a value is infinite, remove it
summary(Y_Add_MAPE, na.rm=TRUE)


#MAPE of only no adds
NoAdd <- TSNL$goodad                                                    #Take the dummy for good adds apart
NoAdd[1:lookback]=2;                                                    #Initialize the times a look-back can't be formed
Smpl_Seq_NoAdd <- Smpl_Seq_All[cbind(which(NoAdd==0)-lookback),]        #Subset the sample sequences with only times without adds
Y_NoAdd <- Smpl_Seq_NoAdd[,ncol(Smpl_Seq_NoAdd)]                        #Create set with dependent variables
X_NoAdd <- array(Smpl_Seq_NoAdd[,-ncol(Smpl_Seq_NoAdd)], dim = c(length(Y_NoAdd), lookback, 1)) #Create set with explanatory variables

Y_NoAdd_pred <- model %>% predict(X_NoAdd)                              #Forecast the data of the times without adds
Y_NoAdd_MAPE <- abs((Y_NoAdd-Y_NoAdd_pred)/Y_NoAdd)*100                 #Calculate the MAPE
Y_NoAdd_MAPE[Y_NoAdd_MAPE == Inf] <- NA                                 #If a value is infinite, remove it
summary(Y_NoAdd_MAPE, na.rm=TRUE)


#MAPE of all data
Y_All <- Smpl_Seq_All[,ncol(Smpl_Seq_All)]                              #Create set with dependent variables
X_All <- array(Smpl_Seq_All[,-ncol(Smpl_Seq_All)], dim = c(length(Y_All), lookback, 1)) #Create set with explanatory variables

Y_All_pred <- model %>% predict(X_All)                                  #Forecast all data
Y_All_MAPE <- abs((Y_All-Y_All_pred)/Y_All)*100                         #Calculate the MAPE
Y_All_MAPE[Y_All_MAPE == Inf] <- NA                                     #If a value is infinite, remove it
summary(Y_All_MAPE, na.rm=TRUE)






############################### BELGIUM ###################################
#Create Dummy for Advertisements
TSBE$goodad[is.na(TSBE$add)] <- 0                             #Convert the NA's for advertisements to 0
TSBE$goodad[is.na(TSBE$goodad)] <- 0                          #Convert the NA's for bad advertisements to 0
add_effect = vector(length = length(TSBE$Freq))               #Create an empty vector for the effects of adds
for (i in 1:length(add_effect)){                              #Loop over this empty vector
  if(TSBE$ad[i]==1){                                          #If there is a moment where there is a good add
    if (i<=length(TSBE$Freq)-5) {                             #And it is not near the end of the vector
      add_effect[i:(i+5)] = 1                                 #Set that moment and moments after to have an add effect
    } else {                                                  #If it is near the end of the vector
      add_effect[i:length(TSBE$Freq)] = 1                     #Stop marking the moments at the end of the vector
    }
  }
  else if(TSBE$ad[i]==0 & add_effect[i] == FALSE){            #If there is no add at that moment or shortly prior
    add_effect[i] = 0                                         #Set the dummy to zero
  }
}


#Create a Baseline
BL <- format(TSBE$date_time,"%H:%M")                          #Create baseline (BL) time series with only the time
BL <- as.data.frame(BL)                                       #Set BL as a data frame
BL <- cbind(BL, TSBE$Freq)                                    #Add the frequencies of adds to the BL data
colnames(BL) <-c("Time","Freq")                               #Rename the columns

for (i in 1:length(add_effect)){                              #Loop over this empty vector
  if (add_effect[i]==1){                                      #If there is no a effect
    BL$Freq[i] <- NA                                          #Replace the frequency for its' minute-based average
  }
}

for (i in 1:length(BL$Freq)){                                             #Loop over all frequencies
  if (is.na(BL$Freq[i])){                                                 #If a value is NA
    BL$Freq[i] <- mean(subset(BL$Freq, BL$Time==BL$Time[i]), na.rm=TRUE)  #Replace it with the minute-based mean
  }
}


#Normalize the BL Data
BL_nrm <- BL$Freq                                                   #Take the BL frequencies apart
BL_nrm <- scale(BL_nrm, center = mean(BL_nrm), scale = sd(BL_nrm))  #Scale this data
norm <- function(x) {                                               #Create a normalization function
  return ((x - min(x)) / (max(x) - min(x)))}
BL_nrm <- norm(BL_nrm)                                              #Normalize the BL data


#Create Sample Sequences
index <- seq(1, length(BL_nrm) - lookback)                          #Create the indexes for overlapping sample sequences
Smpl_Seq <- matrix(nrow = length(index), ncol = lookback+1)         #Create an empty matrix
for (i in 1:length(index)){                                         #Loop over all indexes
  Smpl_Seq[i,] <- BL_nrm[index[i]:(index[i] + lookback)]            #Fill in the matrix with the samples
}


#Split Data
X <- Smpl_Seq[,-ncol(Smpl_Seq)]                                                             #Create set with explanatory variables
Y <- Smpl_Seq[,ncol(Smpl_Seq)]                                                              #Create set with dependent variables
training_set <- createDataPartition(Y, p = .9, list = FALSE , times = 1)                    #Create a vector setting the training data
X_train <- array(X[training_set,], dim = c(length(training_set), lookback, 1))              #Create training data for X
Y_train <- Y[training_set]                                                                  #Create training data for Y
X_test <- array(X[-training_set,], dim = c(length(Y) - length(training_set), lookback, 1))  #Create testing data for X
Y_test <- Y[-training_set]                                                                  #Create testing data for Y


#Create a RNN
model <- keras_model_sequential() %>%                             #Setup a RNN model
  layer_gru(units = 32,                                           #Set the size of the hidden layer
            dropout = 0,                                          #Set the percentage of nodes that will be dropped
            recurrent_dropout = 0,                                #Set the percentage of connections that will be dropped
            input_shape = dim(X_train)[2:3]) %>%                  #Set the input shape for the input layer
  layer_dense(units = 1)                                          #Set the size for the output layer

model %>% compile(                                                #Set the specifics for the RNN
  optimizer = optimizer_rmsprop(),                                #Set the optimizer
  loss = 'mae',                                                   #Use the Mean Absolute Error (MAE) as loss function
)


#Train the Model
trained_model <- model %>% fit(
  x = X_train,                                                    #Set the explanatory training data 
  y = Y_train,                                                    #Set the dependent training data 
  batch_size = batch_size,                                        #Set how many samples are used simultaneously
  epochs = epochs,                                                #Set how many times we'll look at the whole data set
  validation_split = 0.1)                                         #Set how much data is hold out for testing during the process
plot(trained_model)


#Evaluate Forecast Data
Y_pred <- model %>% predict(X_test)                               #Forecast the data of the test sample
Y_MAPE <- abs((Y_test-Y_pred)/Y_test)*100                         #Calculate the Mean Absolute Prediction Error (MAPE)
Y_MAPE[Y_MAPE == Inf] <- NA                                       #If a value is infinite, remove it
summary(Y_MAPE, na.rm=TRUE)                                       #Show the summary of the MAPE

plot_data <- as.data.frame(cbind(Y_test, Y_pred))                 #Make a data frame of the data that will be plotted
ggplot(plot_data[1:150,], aes(x = seq(1:150))) +                  #Plot the data with the corresponding lines
  geom_line(aes(y = Y_test[1:150]), colour = "blue") +
  geom_line(aes(y = Y_pred[1:150]), colour = "red")


#MAPE Tests
#First make data with the original advertisements
All_nrm <- TSBE$Freq                                                    #Take all frequencies apart
All_nrm <- scale(All_nrm, center = mean(All_nrm), scale = sd(All_nrm))  #Scale these frequencies
All_nrm <- norm(All_nrm)                                                #Normalize these frequencies

index <- seq(1, length(All_nrm) - lookback)                             #Create the indexes for overlapping sample sequences
Smpl_Seq_All <- matrix(nrow = length(index), ncol = lookback+1)         #Create an empty matrix
for (i in 1:length(index)){                                             #Loop over all indexes
  Smpl_Seq_All[i,] <- All_nrm[index[i]:(index[i] + lookback)]           #Fill in the matrix with the samples
}

#MAPE of only adds
Smpl_Seq_Add <- Smpl_Seq_All[cbind(which(TSBE$goodad==1)-lookback),]    #Subset the sample sequences with only times with adds
Y_Add <- Smpl_Seq_Add[,ncol(Smpl_Seq_Add)]                              #Create set with dependent variables
X_Add <- array(Smpl_Seq_Add[,-ncol(Smpl_Seq_Add)], dim = c(length(Y_Add), lookback, 1)) #Create set with explanatory variables

Y_Add_pred <- model %>% predict(X_Add)                                  #Forecast the data of the times with adds
Y_Add_MAPE <- abs((Y_Add-Y_Add_pred)/Y_Add)*100                         #Calculate the MAPE
Y_Add_MAPE[Y_Add_MAPE == Inf] <- NA                                     #If a value is infinite, remove it
summary(Y_Add_MAPE, na.rm=TRUE)


#MAPE of only no adds
NoAdd <- TSBE$goodad                                                    #Take the dummy for good adds apart
NoAdd[1:lookback]=2;                                                    #Initialize the times a look-back can't be formed
Smpl_Seq_NoAdd <- Smpl_Seq_All[cbind(which(NoAdd==0)-lookback),]        #Subset the sample sequences with only times without adds
Y_NoAdd <- Smpl_Seq_NoAdd[,ncol(Smpl_Seq_NoAdd)]                        #Create set with dependent variables
X_NoAdd <- array(Smpl_Seq_NoAdd[,-ncol(Smpl_Seq_NoAdd)], dim = c(length(Y_NoAdd), lookback, 1)) #Create set with explanatory variables

Y_NoAdd_pred <- model %>% predict(X_NoAdd)                              #Forecast the data of the times without adds
Y_NoAdd_MAPE <- abs((Y_NoAdd-Y_NoAdd_pred)/Y_NoAdd)*100                 #Calculate the MAPE
Y_NoAdd_MAPE[Y_NoAdd_MAPE == Inf] <- NA                                 #If a value is infinite, remove it
summary(Y_NoAdd_MAPE, na.rm=TRUE)


#MAPE of all data
Y_All <- Smpl_Seq_All[,ncol(Smpl_Seq_All)]                              #Create set with dependent variables
X_All <- array(Smpl_Seq_All[,-ncol(Smpl_Seq_All)], dim = c(length(Y_All), lookback, 1)) #Create set with explanatory variables

Y_All_pred <- model %>% predict(X_All)                                  #Forecast all data
Y_All_MAPE <- abs((Y_All-Y_All_pred)/Y_All)*100                         #Calculate the MAPE
Y_All_MAPE[Y_All_MAPE == Inf] <- NA                                     #If a value is infinite, remove it
summary(Y_All_MAPE, na.rm=TRUE)




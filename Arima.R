#install.packages("forecast")
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
library(readr)
library(readxl)
library(proto)
library(gsubfn)
library(RSQLite)
library(ggplot2)
library(forecast)
library(sqldf)
library(dplyr)
library(smooth)
library(Mcomp)


#initialize parameters
i=0
baseline = ts(replicate(nrow(TSNL), 0))
inversebaseline = ts(replicate(nrow(TSNL), 0))
addmin = 5
forecastwindow = 10
TS = TSNL

#loop through the whole dataset
for(i in 1:nrow(TS)){
  #If we find an add
  if(TS$ad[i] >= 1){
    #create a dynamic series from i-'forecast window' until i-1
    tempseries = ts(TS$Freq[max(1,i-forecastwindow):(i-1)])
    
    temparima = auto.arima(tempseries)
    tempforecast <- forecast(temparima, h=addmin)
    
    #loop through the predicted values and add them to the baseline series
    for(j in 1:addmin){
      baseline[min(i-1+j,nrow(TS))] = tempforecast$mean[j]
    }
    #print("Forecast done at place ")
    #print(i)
  } 
  #will take a few mins
} 


# Check how many times we have forecasted a baseline value
baselinedf = data.frame(baseline)
amount = sqldf("SELECT count(baseline) as count FROM baselinedf where baseline > 0")


#initialize matrix
check = matrix(, nrow = amount[1,1], ncol = 2)
counter = 1

#Create forecast with actuals in one matrix
for(k in 1:length(baseline)){
  if(baseline[k] > 0){
    check[counter,1] = TS$Freq[k]
    check[counter,2] = baseline[k]
    counter = counter + 1
  }
  #print(k)
}

# calculate the mape
mape_tsnl_add = mean(abs((check[,1]-check[,2])/check[,1])) * 100
print("mape for TSNL")
print(mape_tsnl_add)


# ------------------------------------------------------------------------------------------------------------------

#Check for periods without advertisements
#initialize parameters
i=0
inversebaseline = ts(replicate(nrow(TS), 0))
addmin = 5
forecastwindow = 10
TS = TSNL


#loop through the whole dataset for a little checky check
for(i in 10:nrow(TS)){
  #If we find an add
  if(TS$ad[i] == 0 && baseline[i] == 0){
    #create a dynamic series from i-'forecast window' until i-1
    tempseries = ts(TS$Freq[max(1,i-forecastwindow):(i-1)])
    
    #estimate a model and forecast 5 minutes in the future
    temparima = auto.arima(tempseries)
    tempforecast <- forecast(temparima, h=1)
    
    
    inversebaseline[i] = tempforecast$mean[1]
    baseline[i] = tempforecast$mean[1]
    #loop through the predicted values and add them to the baseline series
    
    #print("Forecast2 done at place ")
    #print(i)
  } 
  #will take long time bro
} 

#make dataframe
baselinedf2 = data.frame(inversebaseline)
amount2 = sqldf("SELECT count(inversebaseline) as count FROM baselinedf2 where inversebaseline > 0")


#initialize matrix
check2 = matrix(, nrow = amount2[1,1], ncol = 2)
counter = 1
#Create forecast with actuals in one matrix for the normal set
for(g in 1:length(inversebaseline)){
  if(inversebaseline[g] > 0){
    check2[counter,1] = TS$Freq[g]
    check2[counter,2] = inversebaseline[g]
    counter = counter + 1
  }
  #print(g)
}

mape_tsnl_no_add = mean(abs((check2[,1]-check2[,2])/check2[,1])) * 100
print("mape_tsnl_no_add")
print(mape_tsnl_no_add)

# ------------------------------------------------------------------------------------------------------------------

#make dataframe
baselinedf3 = data.frame(baseline)
amount3 = sqldf("SELECT count(baseline) as count FROM baselinedf3 where baseline > 0")

#initialize matrix
check3 = matrix(, nrow = amount3[1,1], ncol = 2)
counter = 1
#Create forecast with actuals in one matrix for the normal set
for(z in 1:length(baseline)){
  if(baseline[z] > 0){
    check3[counter,1] = TS$Freq[z]
    check3[counter,2] = baseline[z]
    counter = counter + 1
  }
  #print(g)
}

# ------------------------------------------------------------------------------------------------------------------

# calculate the mape
mape_tsnl_add = mean(abs((check[,1]-check[,2])/check[,1])) * 100
print("mape for TSNL")
print(mape_tsnl_add)

mape_tsnl_no_add = mean(abs((check2[,1]-check2[,2])/check2[,1])) * 100
print("mape_tsnl_no_add")
print(mape_tsnl_no_add)

mape_tsnl_all = mean(abs((check3[,1]-check3[,2])/check3[,1])) * 100
print("mape_tsnl_all")
print(mape_tsnl_all)

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ----------------------------------BELGIUM BELGIUM BELGIUM --------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------


#initialize parameters
i=0
baseline_BE = ts(replicate(nrow(TSBE), 0))
inversebaseline_BE = ts(replicate(nrow(TSBE), 0))
addmin = 6
forecastwindow = 10
TS = TSBE

#loop through the whole dataset
for(i in 1:nrow(TS)){
  #If we find an add
  if(TS$ad[i] >= 1){
    #create a dynamic series from i-'forecast window' until i-1
    tempseries = ts(TS$Freq[max(1,i-forecastwindow):(i-1)])
    
    #estimate a model and forecast 5 minutes in the future
    temparima = auto.arima(tempseries)
    tempforecast <- forecast(temparima, h=addmin)
    
    #loop through the predicted values and add them to the baseline series
    for(j in 1:addmin){
      baseline_BE[min(i-1+j,nrow(TS))] = tempforecast$mean[j]
    }
    #print("Forecast done at place ")
    #print(i)
  } 
  #will take a few mins
} 

baseline_BE[is.na(baseline_BE)] <- 0

# Check how many times we have forecasted a baseline value
baselinedf_be = data.frame(baseline_BE)
amount_be = sqldf("SELECT count(baseline_BE) as count FROM baselinedf_be where baseline_BE > 0")


#initialize matrix
check_be = matrix(, nrow = amount_be[1,1], ncol = 2)
counter = 1

#Create forecast with actuals in one matrix
for(k in 1:length(baseline_BE)){
  if(baseline_BE[k] > 0){
    check_be[counter,1] = TS$Freq[k]
    check_be[counter,2] = baseline_BE[k]
    counter = counter + 1
  }
  #print(k)
}

check_be = data.frame(check_be)
check_be = sqldf("SELECT * FROM check_be where X1 > 0")


# calculate the mape
mape_tsbe_add = mean(abs((check_be[,1]-check_be[,2])/check_be[,1])) * 100
print("mape for TSBE")
print(mape_tsbe_add)


# ------------------------------------------------------------------------------------------------------------------

#Check for periods without advertisements
#initialize parameters
i=0
inversebaseline_BE = ts(replicate(nrow(TS), 0))
addmin = 6
forecastwindow = 10
TS = TSBE


#loop through the whole dataset for a little checky check
for(i in 10:nrow(TS)){
  #If we find an add
  if(TS$ad[i] == 0 && baseline_BE[i] == 0){
    #create a dynamic series from i-'forecast window' until i-1
    tempseries = ts(TS$Freq[max(1,i-forecastwindow):(i-1)])
    
    #estimate a model and forecast 5 minutes in the future
    temparima = auto.arima(tempseries)
    tempforecast <- forecast(temparima, h=1)
    inversebaseline_BE[i] = tempforecast$mean[1]
    baseline_BE[i] = tempforecast$mean[1]
    #loop through the predicted values and add them to the baseline series
    
    #print("Forecast2 done at place ")
    #print(i)
  } 
  #will take long time bro
} 

#make dataframe
baselinedf2 = data.frame(inversebaseline_BE)
amount2_be = sqldf("SELECT count(inversebaseline_BE) as count FROM baselinedf2 where inversebaseline_BE > 0")


#initialize matrix
check2_be = matrix(, nrow = amount2_be[1,1], ncol = 2)
counter = 1
#Create forecast with actuals in one matrix for the normal set
for(g in 1:length(inversebaseline_BE)){
  if(inversebaseline_BE[g] > 0){
    check2_be[counter,1] = TS$Freq[g]
    check2_be[counter,2] = inversebaseline_BE[g]
    counter = counter + 1
  }
  #print(g)
}

check2_be = data.frame(check2_be)
check2_be = sqldf("SELECT * FROM check2_be where X1 > 0")

mape_tsbe_no_add = mean(abs((check2_be[,1]-check2_be[,2])/check2_be[,1])) * 100
print("mape_tsbe_no_add")
print(mape_tsbe_no_add)

# ------------------------------------------------------------------------------------------------------------------

#make dataframe
baselinedf3 = data.frame(baseline_BE)
amount3_be = sqldf("SELECT count(baseline_BE) as count FROM baselinedf3 where baseline_BE > 0")

#initialize matrix
check3_be = matrix(, nrow = amount3_be[1,1], ncol = 2)
counter = 1

#Create forecast with actuals in one matrix for the normal set
for(z in 1:length(baseline_BE)){
  if(baseline_BE[z] > 0){
    check3_be[counter,1] = TS$Freq[z]
    check3_be[counter,2] = baseline_BE[z]
    counter = counter + 1
  }
  #print(g)
}

check3_be = data.frame(check3_be)
check3_be = sqldf("SELECT * FROM check3_be where X1 > 0")

# ------------------------------------------------------------------------------------------------------------------

# calculate the mape
mape_tsbe_add = mean(abs((check_be[,1]-check_be[,2])/check_be[,1])) * 100
print("mape for TSBE")
print(mape_tsbe_add)

mape_tsbe_no_add = mean(abs((check2_be[,1]-check2_be[,2])/check2_be[,1])) * 100
print("mape_tsbe_no_add")
print(mape_tsbe_no_add)

mape_tsbe_all = mean(abs((check3_be[,1]-check3_be[,2])/check3_be[,1])) * 100
print("mape_tsbe_all")
print(mape_tsbe_all)

# ------------------------------------------------------------------------------------------------------------

# calculate the mape
mape_tsnl_add = mean(abs((check[,1]-check[,2])/check[,1])) * 100
print("mape for TSNL")
print(mape_tsnl_add)

mape_tsnl_no_add = mean(abs((check2[,1]-check2[,2])/check2[,1])) * 100
print("mape_tsnl_no_add")
print(mape_tsnl_no_add)

mape_tsnl_all = mean(abs((check3[,1]-check3[,2])/check3[,1])) * 100
print("mape_tsnl_all")
print(mape_tsnl_all)
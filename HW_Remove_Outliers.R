# Check after HW where Mape is large and eliminate those from data set

#Check all mapes
Mape_Vec_Before <- vector()
Mape_Vec_After <- vector()
temp <- cbind(df$Freq, yhat)

Mape_Vec_Before <- abs((df$Freq-yhat)/df$Freq)*100
Mape_Before <- data.frame(Mape_Vec_Before)
Mape_Before <- summary(Mape_Before)

#Eliminate NA values
Delete_Threshold <- 20
delete_vec <- 0

temp <- cbind(df$Freq, yhat, Mape_Vec_Before, Delete_Threshold, delete_vec)
for(i in 1:nrow(df)){
  if(temp[i,3] > temp[i,4]){
    temp[i,5] <- 1
  }
}

temp <- cbind(df,temp[,5],yhat)

for(i in 1:nrow(temp)){
  if(temp[i,5] == 1){
    temp[i,2] <- temp[i,6]
  }
}

df <- subset(temp, select = -c(5,6))
 
#Run HW from line 22
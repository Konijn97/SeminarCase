#install.packages("fastdummies")
library("fastDummies")
#####
#adds two columns to BDNL dataframe: 
#the relative and absolute cumulative impact per ad, distributed per GRP in case of double ads
#####abs and rel impact#####


df<-newdata #needs to contain: date_time, Freq, yhat, ad, goodad, outlierimpact, grp,

abs_peak<- df$Freq-df$yhat
BDNL$rel_impact<-NA
BDNL$abs_impact<-NA

# loop through data for ads
for(i in 1:nrow(df)){
   index<-which(BDNL$date_time==df$date_time[i]) #find corresponding ads in broadcasting data
   if(df$ad[i] == 1){
      if(df$spotlength[i]==1){
         BDNL$abs_impact[index] = sum(abs_peak[i:(i+2)])
         BDNL$rel_impact[index] = sum(abs_peak[i:(i+2)])/sum(df$yhat[i:(i+2)])
      }
      else if(df$spotlength[i]==2){
         BDNL$abs_impact[index] = sum(abs_peak[i:(i+5)])
         BDNL$rel_impact[index] = sum(abs_peak[i:(i+5)])/sum(df$yhat[i:(i+5)])
      }
      else if(df$spotlength[i]==3){
         BDNL$abs_impact[index] = sum(abs_peak[i:(i+8)])
         BDNL$rel_impact[index] = sum(abs_peak[i:(i+8)])/sum(df$yhat[i:(i+8)])
      }
      BDNL$abs_impact[index]<- ifelse(BDNL$abs_impact[index]<=0, 0,BDNL$abs_impact[index]) #make nonnegative
      BDNL$rel_impact[index]<- ifelse(BDNL$rel_impact[index]<=0, 0,BDNL$rel_impact[index]) #make nonnegative
   }
   else if(df$ad[i]>1){
      totalGRP<-sum(BDNL$gross_rating_point[index])
      for(j in index){
         if(totalGRP==0){
            ratio <- 1/length(index)
         }
         else{
            ratio<-BDNL$gross_rating_point[j]/totalGRP
         }
         
         if(BDNL$spotlength[j]==1){
            BDNL$abs_impact[j] = ratio*sum(abs_peak[i:(i+2)])
            BDNL$rel_impact[j] = ratio*sum(abs_peak[i:(i+2)])/sum(df$yhat[i:(i+2)])
         }
         else if(BDNL$spotlength[j]==2){
            BDNL$abs_impact[j] = ratio*sum(abs_peak[i:(i+5)])
            BDNL$rel_impact[j] = ratio*sum(abs_peak[i:(i+5)])/sum(df$yhat[i:(i+5)])
         }
         else if(BDNL$spotlength[j]==3){
            BDNL$abs_impact[j] = ratio*sum(abs_peak[i:(i+8)])
            BDNL$rel_impact[j] = ratio*sum(abs_peak[i:(i+8)])/sum(df$yhat[i:(i+8)])
         }
         BDNL$abs_impact[j]<- ifelse(BDNL$abs_impact[j]<=0, 0,BDNL$abs_impact[j]) #make nonnegative
         BDNL$rel_impact[j]<- ifelse(BDNL$rel_impact[j]<=0, 0,BDNL$rel_impact[j]) #make nonnegative
      }
   }
   
}

####causal impact####
library("CausalImpact")
library(zoo)

#create dataframe
df<-newdata #needs to contain: date_time, Freq, yhat, ad

# Set variables as timeseries
real = ts(df$Freq)
baseline = ts(df$yhat)

date_time = df$date_time

#make one dataset with the baseline and forecasts as timeseries
data = zoo(cbind(real, baseline))
head(data)

BDNL$significant_impact<-FALSE

for(i in 1:(nrow(data)-10)){
   if(newdata$ad[i] > 0){
      if(newdata$spotlength[i]==1){
         k=2
      }
      else if(newdata$spotlength[i]==2){
         k=5
      }
      else if(newdata$spotlength[i]==3){
         k=8
      }
      data_impact = data[(i-60):(i+k),]
      pre.period = c(i-60,i)
      post.period = c(i+1,i+k)
      impact = CausalImpact(data_impact, pre.period, post.period)
      if(impact$summary[2,1]>impact$summary[2,4]){
         index<-which(BDNL$date_time==df$date_time[i])
         BDNL$significant_impact[index]<-TRUE
      }
   }
}


#####ad dummies#####
BDNL <- dummy_cols(BDNL, select_columns = c("product_category", "channel", "position_in_break"))

















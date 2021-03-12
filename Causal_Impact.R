library("CausalImpact")
library(zoo)

#create dataframe
df<-newdata #needs to contain: date_time, Freq, yhat, ad

# Set variables as timeseries
real = ts(df$Freq)
baseline = ts(df$yhat)
ad = ts(df$ad)
date_time = df$date_time

#make one dataset with the baseline and forecasts as timeseries
data = zoo(cbind(real, baseline))
head(data)

outlierindices<-rep(FALSE,nrow(data))
for(i in 1:(nrow(data)-10)){
   if(newdata$goodad[i] > 0){
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
      print("cumulative actual:")
      print(impact$summary[2,1])
      print("cumulative upper:")
      print(impact$summary[2,4])
      if(impact$summary[2,1]>impact$summary[2,4]){
         print("outlier")
         outlierindices[i]<-TRUE
      }
   }
   
}
newdata<-cbind(newdata, outlierindices)
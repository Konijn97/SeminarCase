df<-TSNL #set dataframe

#perform simple t test to check wether spike is noise or signal
#one sided t-test at 5%

FRB<-(df$Freq-df$base)/df$base #Frequency relative to baseline in percent

startminute<-11 #set minute to start loop
windowsize<-10 #set window size in minutes
sign_spike<-vector()
AD_IN_EFFECT <- FALSE #boolean to check wether spike is caused by ad
for(i in startminute:nrow(df)){
   #standard error of moving window of visits relative to baseline
   st_err<-sqrt((1/windowsize)*mean((FRB[(i-windowsize-1):(i-1)])^2))
   #still needed: take care of ads in previous minutes
   spike<-FRB
   if((spike/st_err)>1.645){
      #t-test
      sign_spike[i]<-spike
      #check if spike is caused by ad
      if(AD_IN_EFFECT==FALSE&& max(df$ad[(i-1):i])>0){#if ad is now or in previous minute. may be edited to more previuos minutes.
         AD_IN_EFFECT<-TRUE #start ad effect
      }
      else if(AD_IN_EFFECT==FALSE && max(df$ad[(i-1):i])==0){#if there was no ad within the last two minutes
         sign_spike[i]<-NA #delete spike
      }
   }
   else{#if spike not significant
      AD_IN_EFFECT<-FALSE
      #may add condition to let one value in peak be not significant: if previous peak is caused by ad then adeffect is still true
   }
   
}





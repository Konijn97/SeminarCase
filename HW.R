#install.packages("MLmetrics")
library("MLmetrics")


#####starting values#####
ts<-TSNL##Time Series needs to include: date_time, Freq, ad, spotlength
alpha = 0.085
gamma = 0.01
delta = 0.04
adminutes1=2 #empty out minute of ad plus #minutes after
adminutes2=5
adminutes3=8

####code####
#add wintertime
wintertijd<-as.data.frame(matrix(data=NA,nrow=60,ncol=ncol(ts)))
colnames(wintertijd)<-colnames(ts)
wintertijd$Freq<-ts$Freq[126841:126900]
df<-rbind(ts[1:128280,],wintertijd,ts[128281:nrow(ts),])
rownames(df)<-1:nrow(df)
df$ad[is.na(df$ad)]<-0
df$goodad[is.na(df$goodad)]<-0

numobs = nrow(df) #length of TS

y<-df$Freq
#delete observations in y here....
for(j in 0:adminutes1){
   y[which(df$spotlength==1)+j]<-NA
}
for(j in 0:adminutes2){
   y[which(df$spotlength==2)+j]<-NA
}
for(j in 0:adminutes3){
   y[which(df$spotlength==3)+j]<-NA
}
#adjust y vector
while(length(y)>nrow(df)){
   lastobs<-length(y)
   y<-y[-lastobs]
}



p=1440 #seasonal length
indexedit<-p #for I and W vectors
numseasons<-numobs/p #number of seasons

k0<-1
k1<-2
q<-1

#arithmetic mean of all observations present in the kth season
Y_bar<-vector(length=numseasons)
for(i in 1:numseasons){
   Y_bar[i]<-mean(y[((i-1)*p)+(1:p)],na.rm=TRUE)
}

#m step forecast function
mstepfc<-function(thisy,thistn,thism,thisS,thisT,thisI,thisp,thisindexedit){
   #find tn+m star
   tnmstar<-thistn+thism-thisp
   while(is.na(thisy[max(1,tnmstar)])){
      tnmstar<-tnmstar-thisp
   }
   
   returnvalue<-thisS[thistn] + thism*thisT[thistn] + thisI[tnmstar+thisindexedit]
   #returnvalue<-thisS[thistn]  + thisI[tnmstar+thisindexedit]
   return(returnvalue)
}


S<-rep(NA,numobs)
Tvector<-rep(NA,numobs)
I<-rep(NA,(numobs+indexedit)) #all indices + indexvalue due to startingvalues
V<-rep(NA,numobs)
U<-rep(NA,numobs)
W<-rep(NA,(numobs+indexedit)) #all indices + indexvalue due to startingvalues

T0<-(Y_bar[k1]-Y_bar[k0])/((k1-k0)*p)
#T0=0

S0<-Y_bar[k0] - T0*((k0*p)-((p-1)/2))

V0<-1-(1-alpha)^q
U0<-1-(1-gamma)^q
#I and W startvalues
for(i in (-p+1):0){
   W[i+indexedit]<-1-(1-delta)^q
   I[i+indexedit]<-0
   for(k in 1:k1){
      I[i+indexedit]<-I[i+indexedit]+y[i+(k*p)]-(Y_bar[k]+(T0*(i+((p-1)/2))))
   }
   I[i+indexedit]<-I[i+indexedit]/k1
}

####model####
#first time period with starting values
V[1]<-V0/(V0+1-alpha)
U[1]<-U0/(U0+1-gamma)
tnstar<- 1-p
W[1+indexedit]<-W[tnstar+indexedit]/(W[tnstar+indexedit]+(1-delta)^((1-tnstar)/p))

S[1]<-V[1]*(y[1]-I[tnstar+indexedit])+(1-V[1])*(S0+T0)
Tvector[1]<-U[1]*(S[1]-S0)+(1-U[1])*T0
I[1+indexedit]<-W[1+indexedit]*(y[1]-S[1])+(1-W[1+indexedit])*I[tnstar+indexedit]

#restof time periods
tnprev=1
#rprev<-0
r<-vector(length=numobs)
r[1]<- 0
for(tn in 2:numobs){
   if(!is.na(y[tn])){
      #find tnstar
      tnstar=tn-p
      while(is.na(y[max(1,tnstar)])){
         tnstar=tnstar-p
      }
      
      #update weights
      V[tn]<- V[tnprev]/(V[tnprev]+(1-alpha)^(tn-tnprev))
      U[tn]<- U[tnprev]/(U[tnprev]+(1-gamma)^(tn-tnprev))
      W[tn+indexedit]<- W[tnstar+indexedit]/(W[tnstar+indexedit]+(1-delta)^((tn-tnstar)/p))
      
      #update normalising params
      #err<-y[tn]-S[tnprev]-(tn-tnprev)*Tvector[tnprev]-I[tnstar+indexedit]
      err<-y[tn]-S[tnprev]-I[tnstar+indexedit]
      #r[tn]<-(delta/p)*err + r[tnprev]
      r[tn]<-(W[tnprev+indexedit]/p)*err + r[tnprev]
      #print(r)
      
      #update params
      S[tn]<-V[tn]*(y[tn]-I[tnstar+indexedit]) + (1-V[tn])*(S[tnprev]+(tn-tnprev)*Tvector[tnprev]) #+ r[tn]
      Tvector[tn]<-U[tn]*((S[tn]-S[tnprev])/(tn-tnprev)) + (1-U[tn])*Tvector[tnprev]
      I[tn+indexedit]<- W[tn+indexedit]*(y[tn]-S[tn]) + (1-W[tn+indexedit])*I[tnstar+indexedit] #- r[tn]
      
      tnprev=tn
      #rprev=r
   }
}

####forecasting####
ypred<-vector(length=numobs) #1 step ahead forecasts
yhat<-vector(length=numobs)



tnprev<-0#set timevalue of last observation
for(tn in 1:numobs){
   if(is.na(y[tn])){
      ypred[tn]<-NA
      yhat[tn]<-mstepfc(y,tnprev,thism=tn-tnprev,S,Tvector,I,p,indexedit) #m-step for interpolated values
   }
   else{
      #find tnstar
      tnstar=tn-p
      while(is.na(y[max(1,tnstar)])){
         tnstar=tnstar-p
      }
      if(tnprev==0){
         ypred[tn]<-S0 + T0 + I[1] #fix for first observation where index tnprev=0
         yhat[tn]<-S0 + I[tn+indexedit]
      }
      else{
         ypred[tn]<-mstepfc(y,tnprev,thism = tn-tnprev,S,Tvector,I,p,indexedit) #m-step ahead forecast
         yhat[tn]<-S[tnprev]+I[tnprev+indexedit] #smoothed values
      }
      
      tnprev<-tn#update timevalue of last observation
   }
}

#maak een ziek mooie tabel
newdata<-cbind(df,ypred,yhat,y,S,I[(indexedit+1):length(I)],W[(indexedit+1):length(I)],r)
MAPE(yhat[is.na(y)],df$Freq[is.na(y)])
MAPE(ypred[!is.na(y)],df$Freq[!is.na(y)])
MAPE(yhat,df$Freq)

testie<-cbind(y,yhat)
vec<-matrix(ncol=2)
for(i in 2:nrow(testie)){
   if(is.na(testie[(i-1),1])&&!is.na(testie[i,1])){
      vec<-rbind(vec,testie[i,])
   }
}
vec<-vec[-1,]
MAPE(vec[,2],vec[,1])

#newdata$Freq = visits
#newdata$yhat = baseline

#check seasonal sum
#seasontest<-vector(length=180)
#for(day in 1:180){
#   seasontest[day]<- sum(newdata$`I[(indexedit + 1):length(I)]`[(1440*(day-1)+1):(1440*day)])/1440
#}
#plot(seasontest)
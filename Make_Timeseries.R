####PACKAGES####

library(readxl)
#install.packages("sqldf")
library(sqldf)
#install.packages("lubridate")
library(lubridate)

####LOAD DATASETS####
setwd("Directory")
BD<- read.csv("broadcasting_data.csv")
TD<- read.csv("traffic_data.csv")

####CLEAN DATA####
TD <- sqldf("SELECT * FROM TD WHERE visit_source = 'other' OR visit_source = 'direct' OR visit_source = 'search' OR visit_source = 'paid search';")
BD <- sqldf("SELECT * FROM BD WHERE gross_rating_point >= 0.2")
BD <- sqldf("SELECT * FROM BD WHERE date != '2019-02-04';")
BD <- sqldf("SELECT * FROM BD WHERE date != '2019-04-20';")
BD <- sqldf("SELECT * FROM BD WHERE date != '2019-04-21';")


####CREATE TIME SERIES####

#configure date and time
BD$date_time <- paste(BD$date, BD$time, sep=" ")
BD$date_time <- strptime(BD$date_time, "%Y-%m-%d %H:%M:%S")
BD$date_time <- as.POSIXct(BD$date_time)
BD$date_time <- ceiling_date(BD$date_time,unit="minutes") #round up to nearest minute

TD$date_time <- strptime(TD$date_time, "%Y-%m-%d %H:%M:%S")
TD$date_time <- as.POSIXct(TD$date_time)

#transform broadcast variables
BD$spotlength<-NA

for(i in 1:nrow(BD)){
  if(BD$length_of_spot[i]=="30"){
    BD$spotlength[i]<-1
    #print(i)
  }
  else if(BD$length_of_spot[i]=="30 + 10"){
    BD$spotlength[i]<-2
    #print(i)
  }
  else if(BD$length_of_spot[i]=="30 + 10 + 5"){
    BD$spotlength[i]<-3
    #print(i)
  }
  else{
    print("error length of spot")
  }
}



# Sort data by date time
TD <-TD[order(TD$date_time),]
BD <-BD[order(BD$date_time),]

#split by country
TDNL<-TD[TD$country=="Netherlands",]
TDBE<-TD[TD$country=="Belgium",]
BDNL<-BD[BD$country=="Netherlands",]
BDBE<-BD[BD$country=="Belgium",]
rownames(TDNL)<-1:nrow(TDNL)
rownames(TDBE)<-1:nrow(TDBE)

#####NL####
# Count all visits per minute and create a seperate timeseries
TSNL <- as.data.frame(table(TDNL$date_time))
colnames(TSNL)<-c("date_time","Freq")
TSNL$date_time <- strptime(TSNL$date_time, "%Y-%m-%d %H:%M:%S")
TSNL$date_time <- as.POSIXct(TSNL$date_time)

#Time series LEFT JOIN Broadcast data
TSNL<-merge(x=TSNL,y=BDNL,by="date_time", all.x = TRUE)

#create column with amount of ads in timeseries, removes double time stamps
TSNL$ad<-ifelse(is.na(TSNL$operator),0,1) #ad dummy
TSNL$goodad<-ifelse(TSNL$gross_rating_point>0,1,0) #set dummy whether ad has significant GRP and thus may be effective
TSNL$goodad[is.na(TSNL$goodad)] <- 0
i=2
while(i<=nrow(TSNL)){
  if(TSNL$date_time[i]==TSNL$date_time[i-1]){
    TSNL$ad[i-1]<-TSNL$ad[i-1]+1 #add one to the amount of ads in same minute but previous observation
    TSNL$goodad[i-1]<-TSNL$goodad[i-1]+1
    if(TSNL$spotlength[i]>TSNL$spotlength[i-1]){
      TSNL$spotlength[i-1]<-TSNL$spotlength[i]#select longest spot in timeseries
      TSNL$gross_rating_point[i-1]<-TSNL$gross_rating_point[i-1]+TSNL$gross_rating_point[i]
    }
    TSNL<-TSNL[-i,]
  }
  else{
    i=i+1
  }
}
rownames(TSNL)<-1:nrow(TSNL)

#####Create traffic summaries####

#app
appdata<- as.data.frame(table(TDNL$date_time[which(TDNL$medium=="app")]))
colnames(appdata)<-c("date_time","app")
appdata$date_time <- strptime(appdata$date_time, "%Y-%m-%d %H:%M:%S")
appdata$date_time <- as.POSIXct(appdata$date_time)
TSNL<-merge(x=TSNL,y=appdata,by="date_time", all.x = TRUE)
#website
webdata<- as.data.frame(table(TDNL$date_time[which(TDNL$medium=="website")]))
colnames(webdata)<-c("date_time","website")
webdata$date_time <- strptime(webdata$date_time, "%Y-%m-%d %H:%M:%S")
webdata$date_time <- as.POSIXct(webdata$date_time)
TSNL<-merge(x=TSNL,y=webdata,by="date_time", all.x = TRUE)
#direct
directdata<- as.data.frame(table(TDNL$date_time[which(TDNL$visit_source=="direct")]))
colnames(directdata)<-c("date_time","direct")
directdata$date_time <- strptime(directdata$date_time, "%Y-%m-%d %H:%M:%S")
directdata$date_time <- as.POSIXct(directdata$date_time)
TSNL<-merge(x=TSNL,y=directdata,by="date_time", all.x = TRUE)
#other
othersourcedata<- as.data.frame(table(TDNL$date_time[which(TDNL$visit_source=="other")]))
colnames(othersourcedata)<-c("date_time","other source")
othersourcedata$date_time <- strptime(othersourcedata$date_time, "%Y-%m-%d %H:%M:%S")
othersourcedata$date_time <- as.POSIXct(othersourcedata$date_time)
TSNL<-merge(x=TSNL,y=othersourcedata,by="date_time", all.x = TRUE)
#paid search
paidsearchdata<- as.data.frame(table(TDNL$date_time[which(TDNL$visit_source=="paid search")]))
colnames(paidsearchdata)<-c("date_time","paid search")
paidsearchdata$date_time <- strptime(paidsearchdata$date_time, "%Y-%m-%d %H:%M:%S")
paidsearchdata$date_time <- as.POSIXct(paidsearchdata$date_time)
TSNL<-merge(x=TSNL,y=paidsearchdata,by="date_time", all.x = TRUE)
#push notification
#pushdata<- as.data.frame(table(TDNL$date_time[which(TDNL$visit_source=="push notification")]))
#colnames(pushdata)<-c("date_time","push notification")
#pushdata$date_time <- strptime(pushdata$date_time, "%Y-%m-%d %H:%M:%S")
#pushdata$date_time <- as.POSIXct(pushdata$date_time)
#TSNL<-merge(x=TSNL,y=pushdata,by="date_time", all.x = TRUE)
#search
searchdata<- as.data.frame(table(TDNL$date_time[which(TDNL$visit_source=="search")]))
colnames(searchdata)<-c("date_time","search")
searchdata$date_time <- strptime(searchdata$date_time, "%Y-%m-%d %H:%M:%S")
searchdata$date_time <- as.POSIXct(searchdata$date_time)
TSNL<-merge(x=TSNL,y=searchdata,by="date_time", all.x = TRUE)
#bounce
bouncedata<- as.data.frame(table(TDNL$date_time[which(TDNL$bounces=="1")]))
colnames(bouncedata)<-c("date_time","bounces")
bouncedata$date_time <- strptime(bouncedata$date_time, "%Y-%m-%d %H:%M:%S")
bouncedata$date_time <- as.POSIXct(bouncedata$date_time)
TSNL<-merge(x=TSNL,y=bouncedata,by="date_time", all.x = TRUE)

TSNL$bouncerate = TSNL$bounces/TSNL$Freq.x
#TSNL<-subset(TSNL,select=c("date_time", "Freq", "ad","spotlength", "app", "website","direct","other source", "push notification", "search"))


####BE####
# Count all visits per minute and create a seperate timeseries
TSBE <- as.data.frame(table(TDBE$date_time))
colnames(TSBE)<-c("date_time","Freq")
TSBE$date_time <- strptime(TSBE$date_time, "%Y-%m-%d %H:%M:%S")
TSBE$date_time <- as.POSIXct(TSBE$date_time)

#Time series LEFT JOIN Broadcast data
TSBE<-merge(x=TSBE,y=BDBE,by="date_time", all.x = TRUE)

#create column with amount of ads in timeseries, removes double time stamps
TSBE$ad<-ifelse(is.na(TSBE$operator),0,1) #ad dummy
TSBE$goodad<-ifelse(TSBE$gross_rating_point>0,1,0) #set dummy whether ad has significant GRP and thus may be effective
TSBE$goodad[is.na(TSBE$goodad)] <- 0
i=2
while(i<=nrow(TSBE)){
  if(TSBE$date_time[i]==TSBE$date_time[i-1]){
    TSBE$ad[i-1]<-TSBE$ad[i-1]+1
    TSBE$goodad[i-1]<-TSBE$goodad[i-1]+1
    if(TSBE$gross_rating_point[i]>TSBE$gross_rating_point[i-1]){
      TSBE$gross_rating_point[i-1]<-TSBE$gross_rating_point[i-1]+TSBE$gross_rating_point[i]
    }
    TSBE<-TSBE[-i,]
  }
  else{
    i=i+1
  }
}

rownames(TSBE)<-1:nrow(TSBE)

#fill missing minutes
mins<-as.data.frame(seq.POSIXt(as.POSIXct("2019-01-01 00:00:00"), as.POSIXct("2019-06-30 23:59:00"), by="min"))
colnames(mins)<-c("date_time")
TSBE<-merge(x=mins, y=TSBE, by="date_time", all=TRUE)
TSBE$Freq[is.na(TSBE$Freq)]<-0 #set empty frequencies to zero
TSBE$ad[is.na(TSBE$ad)]<-0 #set adds to zero for missing data
#TSBE<-subset(TSBE,select=c("date_time", "Freq", "ad"))
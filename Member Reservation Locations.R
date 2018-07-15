###########################################################################
# Exploratory analysis on where members reserve
# 9/15/16
###########################################################################

#unload all previously loaded packages
pkgs = names(sessionInfo()$otherPkgs) 
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
rm(pkgs)


#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\Member Reserving Locations\\")

#load required libraries
if(! "readr" %in% installed.packages()) {
  install.packages('readr') # read data mo' betta
}
require(readr)


if(! "dplyr" %in% installed.packages()) {
  install.packages('dplyr') # manipulate data mo' betta
}
require(dplyr)


if(! "tidyr" %in% installed.packages()) {
  install.packages('tidyr') # arrange data mo' betta
}
require(tidyr)

if(! "lubridate" %in% installed.packages()) {
  install.packages('lubridate') # work with dates
}
require(lubridate)

if(! "geosphere" %in% installed.packages()) {
  install.packages("geosphere", depend = TRUE)  # figure out distances between lats/longs
}
require(geosphere)

if(! "data.table" %in% installed.packages()) {
  install.packages("data.table", depend = TRUE)  
}
require(data.table)

if(! "knitr" %in% installed.packages()) {
  install.packages("knitr", depend = TRUE) # r markdown - writing reports and ish
}
require(knitr)

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

if(!exists("getMemberData", mode="function")) source("C:\\Users\\babbenante\\documents\\My Stuff\\code\\utils\\load data.r")

#get all members
members.all<-getMemberData("MembersWeekly.txt")
members.all<-members.all%>%
  distinct(MEMBER_ID,.keep_all=TRUE)%>%
setDT(members.all)
members.all[,TIME.TO.FIRST.DRIVE := difftime(FIRST_RES,FIRST_JOIN,units="days")]
setDF(members.all)

#get all reservations
reservations.all<-getReservationData("Reservation-Details-Association.txt")

#load member address lat/long
member.locations<-getMemberLocationData("Member_Locations.txt")

#filter for just unique boston reservation locations by member
res.boston.all<-reservations.all%>%
  mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  filter(ZIPFLEET=='boston_region' & RESERVATION_DATE>=as.Date("2016-05-01") & RESERVATION_DATE<=as.Date("2016-07-31"))

#filter for just unique boston reservation locations by member
res.boston<-reservations.all%>%
  mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  filter(ZIPFLEET=='boston_region' & RESERVATION_DATE>=as.Date("2016-05-01") & RESERVATION_DATE<=as.Date("2016-07-31"))%>%
  mutate(Res.Month=lubridate::month(RESERVATION_DATE,label=TRUE))%>%
#  group_by(MEMBER_ID,Res.Month)%>%
  arrange(MEMBER_ID,RESERVATION_DATE)%>%
  select(MEMBER_ID,LOCATION_ID,LOCATION_LATITUDE,LOCATION_LONGITUDE)%>%
  distinct(MEMBER_ID,LOCATION_LATITUDE,LOCATION_LONGITUDE,.keep_all=TRUE)%>%
  group_by(MEMBER_ID)%>%
  mutate(NextResLat=lag(LOCATION_LATITUDE)
         ,NextResLong=lag(LOCATION_LONGITUDE))

setDT(res.boston)

#calculate the location distance from next reservation
res.boston[ , dist.from.last.res := distGeo(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2), 
                                  matrix(c(NextResLong, NextResLat), ncol = 2))]

setDF(res.boston)

#get some summary info on distances between reservation locations
res.boston.distances<-res.boston%>%
  group_by(MEMBER_ID)%>%
  summarise(num.locations=n_distinct(LOCATION_ID)
            ,avg.distance=mean(dist.from.last.res,na.rm=TRUE)
            ,med.distance=median(dist.from.last.res,na.rm=TRUE))%>%
  group_by(num.locations)%>%
  summarise(mean.distance=mean(avg.distance)
            ,med.distance=mean(med.distance)
            ,quant.10.distance=quantile(avg.distance,probs=.1,na.rm=TRUE)
            ,quant.25.distance=quantile(avg.distance,probs=.25,na.rm=TRUE)
            ,quant.75.distance=quantile(avg.distance,probs=.75,na.rm=TRUE)
            ,quant.90.distance=quantile(avg.distance,probs=.9,na.rm=TRUE))%>%
  mutate(mean.distance.mile=mean.distance*0.000621371192 # multiple by 0.000621371192 to convert meters to miles
         ,med.distance.mile=med.distance*0.000621371192
         ,quant.10.distance.mile=quant.10.distance*0.000621371192
         ,quant.25.distance.mile=quant.25.distance*0.000621371192
         ,quant.75.distance.mile=quant.75.distance*0.000621371192
         ,quant.90.distance.mile=quant.90.distance*0.000621371192)

#get members who have more than two reservations
res.boston.multiple<-res.boston%>%
  group_by(MEMBER_ID)%>%
  summarise(count=n())%>%
  filter(count>2)%>%
  select(MEMBER_ID)

#get the reservations for those members
res.boston.data<-merge(res.boston.multiple,res.boston,all.x=TRUE,by="MEMBER_ID")

#calculate the center of the reservation locations
res.boston.centroid.distance<-res.boston.data%>%
  group_by(MEMBER_ID)%>%
  mutate(cent.long=centroid(makePoly(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2)))[1,1]
         ,cent.lat=centroid(makePoly(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2)))[1,2])

setDT(res.boston.centroid.distance)
#calculate the distance of each res from the centroid
res.boston.centroid.distance[ , dist.from.center := distGeo(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2), 
                                            matrix(c(cent.long, cent.lat), ncol = 2))]

#summarise multi-location users by number of trips and summary of distances
# res.boston.multiple.summary<-res.boston.centroid.distance%>%
#   group_by(MEMBER_ID)%>%
#   summarise(res=n()
#           ,dist.med=median(dist.from.center)
#           ,dist.avg=mean(dist.from.center))%>%
#   group_by(res)%>%
#   summarise(members=n()
#               ,dist.med.med=(median(dist.med)) #multiply by *0.000621371192 to convert to miles
#               ,dist.med.avg=(mean(dist.med))
#               ,dist.avg.med=(median(dist.avg))
#               ,dist.avg.avg=(mean(dist.avg)))

#just keep the centroids
res.boston.centroid.distance<-res.boston.centroid.distance%>%
  select(MEMBER_ID,cent.long,cent.lat)%>%
  distinct(MEMBER_ID,.keep_all=TRUE)
res.boston.multiple.data<-merge(res.boston.multiple,res.boston.centroid.distance,all.x=TRUE,by="MEMBER_ID")


#get members who had two reservations
res.boston.two<-res.boston%>%
  group_by(MEMBER_ID)%>%
  summarise(count=n())%>%
  filter(count==2)%>%
  select(MEMBER_ID)



res.boston.two.distance<-merge(res.boston.two,res.boston,all.x=TRUE,by="MEMBER_ID")
res.boston.two.distance<-res.boston.two.distance[complete.cases(res.boston.two.distance),]


setDT(res.boston.two.distance)
res.boston.two.distance<-res.boston.two.distance%>%
  group_by(MEMBER_ID)%>%
  mutate(cent.long=midPoint(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2),matrix(c(NextResLong,NextResLat), ncol = 2))[1,1]
         ,cent.lat=midPoint(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2),matrix(c(NextResLong,NextResLat), ncol = 2))[1,2])

setDF(res.boston.two.distance)

res.boston.two.distance<-res.boston.two.distance%>%
  select(MEMBER_ID,cent.long,cent.lat)%>%
  distinct(MEMBER_ID,.keep_all=TRUE)

res.boston.two.data<-merge(res.boston.two,res.boston.two.distance,all.x=TRUE,by="MEMBER_ID")


#summary data for members reserving in two locations
# res.boston.two.summary<-res.boston.two.data%>%
#   group_by(MEMBER_ID)%>%
#   summarise(res=n()
#             ,dist.med=median(dist.from.last.res,na.rm=TRUE)
#             ,dist.avg=mean(dist.from.last.res,na.rm=TRUE))%>%
#   group_by(res)%>%
#   summarise(members=n()
#             ,dist.med.med=(median(dist.med,na.rm=TRUE)*0.00062137119) #multiply by *0.000621371192 to convert to miles
#             ,dist.med.avg=(mean(dist.med,na.rm=TRUE)*0.00062137119)
#             ,dist.avg.med=(median(dist.avg,na.rm=TRUE)*0.00062137119)
#             ,dist.avg.avg=(mean(dist.avg,na.rm=TRUE)*0.00062137119))


#get members who only reserved in one location
res.boston.single<-res.boston%>%
  group_by(MEMBER_ID)%>%
  summarise(count=n())%>%
  filter(count==1)%>%
  select(MEMBER_ID)

res.boston.single.data<-merge(res.boston.single,res.boston,all.x=TRUE,by="MEMBER_ID")%>%
  mutate(cent.long=LOCATION_LONGITUDE
         ,cent.lat=LOCATION_LATITUDE)%>%
  select(MEMBER_ID,cent.long,cent.lat)

#now merge them all back together
res.boston.distance.all<-rbind(res.boston.multiple.data,res.boston.two.data)
res.boston.distance.all<-rbind(res.boston.distance.all,res.boston.single.data)

#and append the member locations

res.boston.distance.all<-merge(res.boston.distance.all,member.locations,all.x=TRUE,by="MEMBER_ID")
setDT(res.boston.distance.all)
res.boston.distance.all[ , dist.to.res := distGeo(matrix(c(cent.long, cent.lat), ncol = 2), 
                                         matrix(c(as.numeric(ADDRESS_LONG), as.numeric(ADDRESS_LAT)), ncol = 2))]
setDF(res.boston.distance.all)

#now summarise the number of reservations for each mem
res.boston.num.res<-res.boston.all%>%
  group_by(MEMBER_ID)%>%
  summarise(num.loc=n_distinct(LOCATION_ID)
            ,num.res=n())

#and append that to our summary
res.boston.distance.all<-merge(res.boston.distance.all,res.boston.num.res,all.x=TRUE,by="MEMBER_ID")

#get those who's reservations included their first
first.time.res<-reservations.all%>%
  mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  filter(ZIPFLEET=='boston_region' & RESERVATION_DATE>=as.Date("2016-05-01") & RESERVATION_DATE<=as.Date("2016-07-31"))%>%
  filter(OVERALL_RESERVATION_SEQNUMBER==1)%>%
  select(MEMBER_ID)%>%
  mutate(first.res.included=1)

#and append that to our summary
res.boston.distance.all<-merge(res.boston.distance.all,first.time.res,all.x=TRUE,by="MEMBER_ID")

write_delim(res.boston.distance.all,"c:\\users\\babbenante\\downloads\\blake.txt",delim="\t")

IQ.dist<-quantile(res.boston.all$mem.dist.to.res,probs=.75,na.rm=TRUE)-quantile(res.boston.all$mem.dist.to.res,probs=.25,na.rm=TRUE)
upper.outlier<-quantile(res.boston.all$mem.dist.to.res,probs=.75,na.rm=TRUE)+(IQ.dist*1.5)
lower.outlier<-quantile(res.boston.all$mem.dist.to.res,probs=.25,na.rm=TRUE)-(IQ.dist*1.5)

setDT(res.boston.all)
res.boston.all[ , mem.dist.to.res := distGeo(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2), 
                                         matrix(c(MEMBER_ADDR_LONGITUDE, MEMBER_ADDR_LATTITUDE), ncol = 2))]
setDF(res.boston.all)

res.boston.mem.distance.summary<-res.boston.all%>%
  group_by(MEMBER_ID)%>%
  summarise(num.location=n_distinct(LOCATION_ID)
            ,num.res=n()
            ,avg.dist=mean(mem.dist.to.res)
            ,med.dist=median(mem.dist.to.res)
            ,q.25.dist=quantile(mem.dist.to.res,probs=.25,na.rm=TRUE)
            ,q.75.dist=quantile(mem.dist.to.res,probs=.75,na.rm=TRUE)
            ,first.res.position=min(OVERALL_RESERVATION_SEQNUMBER))



first.time.res<-reservations.all%>%
  filter(OVERALL_RESERVATION_SEQNUMBER==1)
setDT(first.time.res)
first.time.res[ , dist.to.res := distGeo(matrix(c(LOCATION_LONGITUDE, LOCATION_LATITUDE), ncol = 2), 
                                            matrix(c(MEMBER_ADDR_LONGITUDE, MEMBER_ADDR_LATTITUDE), ncol = 2))]
setDF(first.time.res)
first.time.res<-first.time.res%>%
  select(MEMBER_ID,dist.to.res)%>%
  mutate(dist.to.res=dist.to.res*0.00062137119)

first.time.res<-merge(first.time.res,members.all,all.x=TRUE,by="MEMBER_ID")
first.time.res<-first.time.res[complete.cases(first.time.res),]
first.time.res.filtered<-first.time.res[which(first.time.res$dist.to.res<=80),]
my.lr<-lm(TTF~dist.to.res,first.time.res.filtered)
summary(my.lr)
summary(first.time.res.filtered)

d<-density(first.time.res.filtered$dist.to.res)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")
###########################################################################
# Compare locations of past 6 months to closest locations to member's saved
# addresses
# 12/18/16
###########################################################################

require(zipcarFunctions)

#unload all previously loaded packages
detachAllPackages()

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(read)
}
if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}
if (!require(purrr)) {
  install.packages('purrr') # consistent & safe list/vector munging
  require(purrr)
}
if (!require(tidyr)) {
  install.packages('tidyr') # consistent data.frame cleaning
  require(tidyr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # date manipulation
  require(lubridate)
}
if (!require(geosphere)) {
  install.packages('geosphere') # date manipulation
  require(geosphere)
}
if (!require(leaflet)) {
  install.packages('leaflet') # date manipulation
  require(leaflet)
}
if (!require(ggplot2)) {
  install.packages('ggplot2') # date manipulation
  require(ggplot2)
}
if (!require(devtools)) {
  install.packages('devtools') # date manipulation
  require(devtools)
}
if (!require(here)) {
  devtools::install_github("krlmlr/here") #relative working directory
  require(here)
}
#set working directory
setwd(here())

#if(!exists("getMemberData", mode="function")) source("../utils/loaddata.r")
#if(!exists("load_themes")) source("../utils/themes.r")
#if(!exists("diagPlot", mode="function")) source("../utils/charts.r")


#if loading the calculated rfm's, we don't really need to load the members.
#get member data
# members.all<-getMemberData("MembersWeekly-Current.txt")
# members.all<-members.all%>%
#   arrange(MEMBER_ID,desc(SNAPSHOT_DATE))%>%
#   distinct(MEMBER_ID,.keep_all=TRUE)%>%
#   setDT(members.all)
# members.all[,TIME.TO.FIRST.DRIVE := difftime(FIRST_RES,FIRST_JOIN,units="days")]
# members.all$FIRST_JOIN_COHORT<-paste(toupper(format(members.all[,FIRST_JOIN],"%Y-%b")))
# setDF(members.all)
# curr_date=as.Date("2017-01-30")
# 
# member.weekly<-members.all%>%
#   dplyr::filter(SNAPSHOT_DATE==curr_date)%>%
#   dplyr::mutate(NewMember=ifelse(curr_date-FIRST_JOIN<=14,1,0))%>%
#   dplyr::select(MEMBER_ID,FIRST_JOIN_COHORT,NewMember,ZIPFLEET,BUSINESS_SEGMENT,RATEPLAN)
# 
# rm(members.all)

#load res data
reservations.all<-getReservationData("Reservation-Details-Current.txt")


members_rfm <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/reports/rfm/rfmscores20170403.txt"
                          ," ", escape_double = FALSE
                          , col_types = cols(MEMBER_ID = col_character())
                          ,trim_ws = TRUE)



#we're only going to focus on NA members reserving in NA for now
reservations.all<-reservations.all%>%
  filter(LOCATION_ZIPFLEET %in% c('dc_region','ny_region','country_us_agencies','philadelphia_region','baltimore_region','pittsburgh_region'
                                  ,'vancouver_region','providence_region','sanfrancisco_region','chicago_region','toronto_region','sacramento_region'
                                  ,'seattle_region','dallas_region','atlanta_region','boston_region','portland_region','los_angeles_region'
                                  ,'san_diego_region','miami_region','houston_region','milwaukee_region','austin_region'
                                  ,'detroit_region','minneapolis_region','denver_region','country_us_airports')) %>% 
  filter(as.Date(RESERVATION_DATE) <= as.Date("2017-04-10"))


#load location address lat/long
member.locations<-getMemberClosestPodLocationData("closest_member_pod_locations.txt")



blake.temp<-reservation.current %>% 
  mutate(my.per=round(period_value,2)) %>% 
  group_by(my.per) %>% 
  summarise(res=n())

#set precision of Lat/longs here
dist.precision<-3
member.locations$ADDRESS_LONG<-round(member.locations$ADDRESS_LONG,dist.precision)
member.locations$ADDRESS_LAT<-round(member.locations$ADDRESS_LAT,dist.precision)
member.locations$pod_lat<-round(member.locations$pod_lat,dist.precision)
member.locations$pod_long<-round(member.locations$pod_long,dist.precision)
reservations.all$LOCATION_LATITUDE<-round(reservations.all$LOCATION_LATITUDE,dist.precision)
reservations.all$LOCATION_LONGITUDE<-round(reservations.all$LOCATION_LONGITUDE,dist.precision)
reservations.all$MEMBER_LATITUDE<-round(reservations.all$MEMBER_LATITUDE,dist.precision)
reservations.all$MEMBER_LONGITUDE<-round(reservations.all$MEMBER_LONGITUDE,dist.precision)

#lets just look at current members
mem.rfm.compact<-members_rfm%>%
  select(MEMBER_ID,Profile)
member.locations<-merge(mem.rfm.compact,member.locations,by='MEMBER_ID')



member.locations.user.saved<-member.locations%>%
  filter(!ADDRESS_TYPE %in% c('cur_loc','old_curr_loc'))%>%
#  filter(ADDRESS_TYPE %in% c('home','work','other'))%>%
  select(MEMBER_ID,LOCATION_ID,ADDRESS_TYPE,PREFERRED_P,is_Outlier,ADDRESS_LONG,ADDRESS_LAT,ZIPFLEET)%>%
  rename(Preferred_address=PREFERRED_P
         ,MEMBER_LOCATION_ID=LOCATION_ID
         ,MEMBER_LATITUDE=ADDRESS_LAT
         ,MEMBER_LONGITUDE=ADDRESS_LONG)

#length(unique(member.locations.user.saved$MEMBER_ID))

member.locations.summary<-member.locations%>%
  group_by(ADDRESS_TYPE)%>%
  summarise(members=n_distinct(MEMBER_ID))
member.locations.summary$tot_mem=length(members_rfm$MEMBER_ID)
member.locations.summary$mem_share=member.locations.summary$members/member.locations.summary$tot_mem

member.num.locations.summary<-member.locations.user.saved%>%
  group_by(MEMBER_ID)%>%
  summarise(locations=n_distinct(ADDRESS_TYPE))%>%
  group_by(locations)%>%
  summarise(members=n())

#sum(member.num.locations.summary$members)

#add a row for members with no locations.  
member.num.locations.summary<-rbind(member.num.locations.summary
                                    , c(0,length(members_rfm$MEMBER_ID)-sum(member.num.locations.summary$members)))

ggplot(member.locations.summary, aes(x=ADDRESS_TYPE,y=mem_share))+
geom_bar(stat="identity",fill="#51a601", alpha=0.75) +
scale_fill_brewer(palette="Greens")+
labs(title="Share of Member share having address type ", x="Address Type", y="Member Share") +
fte_theme()+
scale_y_continuous(labels = scales::percent)


ggplot(member.num.locations.summary, aes(x=locations,y=members))+
  geom_bar(stat="identity",fill="#51a601", alpha=0.75) +
  scale_colour_brewer(palette="Greens")+
  labs(title="Number of members by number of user entered addresses ", x="Number of addresses", y="Members") +
  fte_theme()+
  scale_y_continuous(labels = scales::comma)


# member.locations.inferred<-member.locations%>%
#   filter(ADDRESS_TYPE %in% c('cur_loc','old_curr_loc'))%>%
#   select(MEMBER_ID,LOCATION_ID,ADDRESS_TYPE,PREFERRED_P,is_Outlier,ADDRESS_LONG,ADDRESS_LAT)%>%
#   rename(Preferred_address=PREFERRED_P
#          ,MEMBER_LOCATION_ID=LOCATION_ID
#          ,MEMBER_LATITUDE=ADDRESS_LAT
#          ,MEMBER_LONGITUDE=ADDRESS_LONG)
  

member.location.reservations<-left_join(reservations.all,member.locations,all.x=TRUE,by=c("MEMBER_ID","LOCATION_ID"))

mem.num.loc.summary<-reservations.all%>%
  group_by(MEMBER_ID,LOCATION_ID)%>%
  summarise(reservations=n()
            ,locations=n_distinct(LOCATION_ID))%>%
  group_by(MEMBER_ID)%>%
  summarise(max_res=max(reservations)
            ,total_res=sum(reservations)
            ,total_locations=sum(locations))%>%
  mutate(res_per_loc=total_res/total_locations
         ,uniformity=max_res/total_res)

count.mem.by.loc.used<-mem.num.loc.summary%>%
  group_by(total_locations)%>%
  summarise(members=n())


ggplot(count.mem.by.loc.used, aes(x=total_locations,y=members))+
  geom_bar(stat="identity",fill="#51a601", alpha=0.75) +
  scale_colour_brewer(palette="Greens")+
  labs(title="Number of members by number locations used ", x="Number of locations", y="Members") +
  fte_theme()+
  scale_y_continuous(labels = scales::comma)

#  group_by(locations)%>%
#  summarise(member=n())

#location.summary<-member.locations%>%
#  group_by(MEMBER_ID,LOCATION_ID)%>%
#  summarise(addr_count=n())%>%
#  filter(addr_count>1)

# member.location.summary<-member.locations.user.saved%>%
#   group_by(MEMBER_ID)%>%
#   summarise(NumLocations=n_distinct(ADDRESS_TYPE)
#             ,OtherLocation=sum(ADDRESS_TYPE=="other")
#             ,HomeLocation=sum(ADDRESS_TYPE=="home")
#             ,WorkLocation=sum(ADDRESS_TYPE=="work"))%>%
#   mutate(OtherLocation=ifelse(OtherLocation>0,1,0)
#          ,HomeLocation=ifelse(HomeLocation>0,1,0)
#          ,WorkLocation=ifelse(WorkLocation>0,1,0))
# 
# member.location.summary.2<-member.locations.inferred%>%
#   group_by(MEMBER_ID)%>%
#   summarise(NumAutoLocations=n_distinct(LOCATION_ID))
# 
# member.weekly<-merge(member.weekly,member.location.summary,all.x=TRUE,by=c("MEMBER_ID"))
# member.weekly<-merge(member.weekly,member.location.summary.2,all.x=TRUE,by=c("MEMBER_ID"))

#member.weekly$NumLocations[is.na(member.weekly$NumLocations)]<-0
#member.weekly$NumAutoLocations[is.na(member.weekly$NumAutoLocations)]<-0

# res.summary<-reservations.all%>%
#   group_by(MEMBER_ID)%>%
#   summarise(Reservations=n()
#             ,Revenue=sum(UP_REVENUE))
# 
# member.weekly<-merge(member.weekly,res.summary,all.x=TRUE,by=c("MEMBER_ID"))
# member.weekly[is.na(member.weekly)] <- 0
# member.weekly$TotalLocations<-member.weekly$NumLocations+member.weekly$NumAutoLocations
# member.weekly$DidReserve<-ifelse(member.weekly$Reservations>0,1,0)
# member.weekly$OtherLocation<-as.factor(member.weekly$OtherLocation)
# member.weekly$HomeLocation<-as.factor(member.weekly$HomeLocation)
# member.weekly$WorkLocation<-as.factor(member.weekly$WorkLocation)
# num.loc.lm<-lm(Reservations~NumLocations+NumAutoLocations+HomeLocation+WorkLocation+OtherLocation,data=member.weekly)
# summary(num.loc.lm)
# require(car)
#outlierTest(num.loc.lm) # Bonferonni p-value for most extreme obs
#qqPlot(num.loc.lm, main="QQ Plot") #qq plot for studentized resid 
#leveragePlots(num.loc.lm) # leverage plots


# num.loc.summary<-member.weekly%>%
#   mutate(DidReserve=ifelse(Reservations>0,1,0)
#          ,NumUserLocations=NumLocations-NumAutoLocations)%>%
#   group_by(NumUserLocations,DidReserve)%>%
#   summarise(members=n()
#             ,Res=mean(Reservations)
#             ,Rev=mean(Revenue)
#             ,locations.mean=mean(TotalLocations)
#             ,locations.med=median(TotalLocations)
#             ,locations.member=mean(NumLocations)
#             ,locations.member.med=median(NumLocations)
#             ,locations.auto=mean(NumAutoLocations))


# other.loc.summary<-member.weekly%>%
#   mutate(DidReserve=ifelse(Reservations>0,1,0))%>%
#   group_by(OtherLocation,DidReserve)%>%
#   summarise(members=n()
#             ,Res=mean(Reservations)
#             ,Rev=mean(Revenue)
#             ,locations.mean=mean(TotalLocations)
#             ,locations.med=median(TotalLocations)
#             ,locations.member=mean(NumLocations)
#             ,locations.member.med=median(NumLocations)
#             ,locations.auto=mean(NumAutoLocations))
# 
# work.loc.summary<-member.weekly%>%
#   mutate(DidReserve=ifelse(Reservations>0,1,0))%>%
#   group_by(WorkLocation,DidReserve)%>%
#   summarise(members=n()
#             ,Res=mean(Reservations)
#             ,Rev=mean(Revenue)
#             ,locations.mean=mean(TotalLocations)
#             ,locations.med=median(TotalLocations)
#             ,locations.member=mean(NumLocations)
#             ,locations.member.med=median(NumLocations)
#             ,locations.auto=mean(NumAutoLocations))



res.compact<-reservations.all%>%
  select(MEMBER_ID,RESERVATION_ID,RESERVATION_DATE,LOCATION_ID,LOCATION,LOCATION_LATITUDE,LOCATION_LONGITUDE,LOCATION_ZIPFLEET)


mem.addr.res.full<-left_join(res.compact,member.locations.user.saved,by="MEMBER_ID")
mem.addr.res.full$dist.to.res = distGeo(matrix(c(mem.addr.res.full$MEMBER_LONGITUDE,mem.addr.res.full$MEMBER_LATITUDE), ncol = 2),
                                           matrix(c(mem.addr.res.full$LOCATION_LONGITUDE, mem.addr.res.full$LOCATION_LATITUDE), ncol = 2))*0.000621371192

zipfleet.distance.sum<-mem.addr.res.full%>%
  group_by(MEMBER_ID,RESERVATION_ID,LOCATION_ZIPFLEET)%>%
  summarise(MemDistance=min(dist.to.res))%>%
  group_by(LOCATION_ZIPFLEET)%>%
  summarise(my.deciles.10=quantile(MemDistance,probs=.1,na.rm=TRUE)
            ,my.deciles.20=quantile(MemDistance,probs=.2,na.rm=TRUE)
            ,my.deciles.30=quantile(MemDistance,probs=.3,na.rm=TRUE)
            ,my.deciles.40=quantile(MemDistance,probs=.4,na.rm=TRUE)
            ,my.deciles.50=quantile(MemDistance,probs=.5,na.rm=TRUE)
            ,my.deciles.60=quantile(MemDistance,probs=.6,na.rm=TRUE)
            ,my.deciles.70=quantile(MemDistance,probs=.7,na.rm=TRUE)
            ,my.deciles.80=quantile(MemDistance,probs=.8,na.rm=TRUE)
            ,my.deciles.90=quantile(MemDistance,probs=.9,na.rm=TRUE))


zipfleet.distance.sum.joan<-mem.addr.res.full%>%
  group_by(MEMBER_ID,RESERVATION_ID,LOCATION_ZIPFLEET)%>%
  summarise(MemDistance=min(dist.to.res))%>%
  ungroup(.) %>% 
  sample_n(25000) %>% 
#  group_by(LOCATION_ZIPFLEET)%>%
  summarise(my.deciles.10=quantile(MemDistance,probs=.1,na.rm=TRUE)
            ,my.deciles.20=quantile(MemDistance,probs=.2,na.rm=TRUE)
            ,my.deciles.25=quantile(MemDistance,probs=.25,na.rm=TRUE)            
            ,my.deciles.30=quantile(MemDistance,probs=.3,na.rm=TRUE)
            ,my.deciles.40=quantile(MemDistance,probs=.4,na.rm=TRUE)
            ,my.deciles.45=quantile(MemDistance,probs=.45,na.rm=TRUE)            
            ,my.deciles.50=quantile(MemDistance,probs=.5,na.rm=TRUE)
            ,my.deciles.55=quantile(MemDistance,probs=.55,na.rm=TRUE)            
            ,my.deciles.60=quantile(MemDistance,probs=.6,na.rm=TRUE)
            ,my.deciles.70=quantile(MemDistance,probs=.7,na.rm=TRUE)
            ,my.deciles.75=quantile(MemDistance,probs=.75,na.rm=TRUE)            
            ,my.deciles.80=quantile(MemDistance,probs=.8,na.rm=TRUE)
            ,my.deciles.90=quantile(MemDistance,probs=.9,na.rm=TRUE)
            ,my.deciles.95=quantile(MemDistance,probs=.95,na.rm=TRUE)
            ,my.deciles.95=quantile(MemDistance,probs=.99,na.rm=TRUE))


#get the shortest distance for each reservation
mem.shortest.distance<-mem.addr.res.full %>% 
  #group_by(MEMBER_ID,RESERVATION_ID,ZIPFLEET,LOCATION_ID)%>%
  group_by(MEMBER_ID,RESERVATION_ID,ZIPFLEET,LOCATION_ID,RESERVATION_DATE,LOCATION_ZIPFLEET)%>%
  summarise(MemDistance=min(dist.to.res,na.rm=TRUE))

#get the shortest distance for each member/location pair
mem.shortest.distance.location<-mem.addr.res.full%>%
  group_by(MEMBER_ID,RESERVATION_ID,LOCATION_ZIPFLEET,LOCATION_ID,RESERVATION_DATE)%>%
  summarise(MemDistance=min(dist.to.res,na.rm=TRUE))%>%
  group_by(MEMBER_ID,LOCATION_ID,LOCATION_ZIPFLEET)%>%
  summarise(MemDistance=mean(MemDistance,na.rm=TRUE)) 

#append the deciles for each zipfleet to the reservation records
mem.shortest.distance.location<-left_join(mem.shortest.distance.location,zipfleet.distance.sum,all.x=TRUE,by='LOCATION_ZIPFLEET')

#if the member didnt have any saved locations, they will be na for shortest distance
#use the median for their zipfleet 
mem.dist.sum<-mem.shortest.distance.location%>%
  ungroup(.)%>%
  mutate(MemDistance=ifelse(is.na(MemDistance),my.deciles.50,MemDistance))%>%
  mutate(ShortDistance=ifelse(MemDistance<=my.deciles.50,1,0))%>%
  group_by(MEMBER_ID)%>%
  summarise(close.loc=sum(ShortDistance)
            ,num.loc=n()
            ,proximity=close.loc/num.loc)

mem.dist.sum<-left_join(mem.dist.sum,mem.num.loc.summary,all.x=TRUE,by='MEMBER_ID')
#summary(mem.dist.sum)


members_rfm<-left_join(members_rfm,mem.dist.sum,all.x=TRUE,by='MEMBER_ID')

members_rfm.sample<-members_rfm%>%
  sample_n(25000)
members_rfm.sample[which(is.na(members_rfm.sample$uniformity)),]$uniformity<-1
members_rfm.sample[which(is.na(members_rfm.sample$proximity)),]$proximity<-1
members_rfm.sample<-members_rfm.sample[!is.na(members_rfm.sample$total_res),]

set.seed(19)
rfmCluster <- kmeans(members_rfm.sample[, c(15,20)], 4, nstart = 20)
members_rfm.sample$cluster<-rfmCluster$cluster

  ggplot(members_rfm.sample, aes(x = proximity, y = uniformity))+ 
##  geom_point(aes(color=cluster))+
  scale_colour_brewer(palette="Greens")+
#  scale_colour_hue("clarity")+
  geom_jitter(width = 0.1, height = 0.1,aes(color=factor(Profile, levels = c("Low Activity","Vulnerable","Standard-","Standard+","Strong","Power User"))))+
#  geom_jitter(width = 0.1, height = 0.1,aes(color=as.factor(cluster)))+
  labs(title="Segment scatter by proximity and uniformity ", x="Proximity", y="uniformity") +
  fte_theme()+
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
  
quantile(members_rfm$proximity,probs=seq(0,1,.1),na.rm=TRUE)
quantile(members_rfm$uniformity,probs=seq(0,1,.1),na.rm=TRUE)
members_rfm[which(is.na(members_rfm$proximity)),]$proximity<-1
members_rfm[which(is.na(members_rfm$uniformity)),]$uniformity<-1
#look at distribution of proximity w/o a res - 0.5 is the median
#quantile(members.rfm[which(!is.na(members.rfm$reservations.y)),]$close.res.share,probs=seq(0,1,.1))
members_rfm$proximity1<-ifelse(members_rfm$proximity<=.5,0,1)
#look at distribution of res per loc excluding people w/o a res - 1.5 is the median
#quantile(members.rfm[which(!is.na(members.rfm$reservations.y)),]$res_per_loc,probs=seq(0,1,.1))
members_rfm$uniformity1<-ifelse(members_rfm$uniformity<=.66,0,1)
members_rfm$LocationSegment<-ifelse(is.na(members_rfm$total_res),5
                                    ,ifelse(members_rfm$proximity1==0 & members_rfm$uniformity1==0,'Wildcard'
                                           ,ifelse(members_rfm$proximity1==1 & members_rfm$uniformity1==1,'Homebody'
                                                   ,ifelse(members_rfm$proximity1==0 & members_rfm$uniformity1==1,'Traveler','Opportunist'))))

write_delim(members_rfm,"C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/reports/rfm/rfmscores_with_location20170403.txt",delim="\t")


blake.temp<-members_rfm%>%
  group_by(LocationSegment)%>%
  summarise(members=n()
            ,active.members=sum(Reservations>0)
            ,reservations=sum(Reservations)
            ,revenue=sum(Revenue))




#members_rfm[is.na(members_rfm)]<-0
ggplot(members_rfm, aes(total_locations)) +
  geom_density(adjust=5,fill="#51a601", alpha=0.75) +
  fte_theme()
  


#now lets look at some locations
bos.loc.summary<-mem.shortest.distance%>%
  ungroup(.)%>%
  mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  arrange(LOCATION_ZIPFLEET,RESERVATION_DATE)%>%
  filter(LOCATION_ZIPFLEET=='boston_region' & RESERVATION_DATE>=as.Date('2017-03-06') & RESERVATION_DATE<=as.Date('2017-04-02'))%>%
  group_by(LOCATION_ID)%>%
  summarise(res=n_distinct(RESERVATION_ID)
            ,members=n_distinct(MEMBER_ID)
            ,min.dist=min(MemDistance,na.rm=TRUE)
            ,max.dist=max(MemDistance,na.rm=TRUE)
            ,mean.dist=mean(MemDistance,na.rm=TRUE)
            ,med.dist=median(MemDistance,na.rm=TRUE))

bos.loc.summary.top.mem<-mem.addr.res.full%>%
  mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  arrange(LOCATION_ZIPFLEET,RESERVATION_DATE)%>%
  filter(LOCATION_ZIPFLEET=='boston_region' & RESERVATION_DATE>=as.Date('2017-03-06') & RESERVATION_DATE<=as.Date('2017-04-02'))%>%
  group_by(LOCATION_ID,MEMBER_ID)%>%
  summarise(mem.res=n_distinct(RESERVATION_ID))%>%
  group_by(LOCATION_ID)%>%
  summarise(top.mem=max(mem.res))

bos.loc.summary<-merge(bos.loc.summary,bos.loc.summary.top.mem,all.x=TRUE,by='LOCATION_ID')
bos.loc.summary$leverage<-bos.loc.summary$top.mem/bos.loc.summary$res

location_utlization <- read_delim("C:/Users/babbenante/Downloads/location_utlization.txt"
                                  ,"\t"
                                  ,escape_double = FALSE
                                  ,col_types = cols(DAY = col_date(format = "%m/%d/%Y")
                                                    , LOCATION_ID = col_character())
                                  ,trim_ws = TRUE)




location_utlization_summary<-location_utlization %>% 
  mutate(weekend=ifelse(wday(DAY) %in% c(1,7),1,0)) %>% 
  group_by(LOCATION_ID,weekend) %>% 
  summarise(avg_daily_util=sum(HOURS_USED)/sum(CAPACITY)) %>% 
  spread(weekend,avg_daily_util)

names(location_utlization_summary)[2]<-'weekday_util'
names(location_utlization_summary)[3]<-'weekend_util'

bos.loc.summary<-left_join(bos.loc.summary,location_utlization_summary)

bos.loc.specific<-mem.shortest.distance%>%
  ungroup(.)%>%
    mutate(RESERVATION_DATE=as.Date(RESERVATION_DATE))%>%
  #location=357817679
  filter(LOCATION_ID %in% c(47812489) & RESERVATION_DATE>=as.Date('2017-03-06') & RESERVATION_DATE<=as.Date('2017-04-02'))

mem.rfm.compact<-members_rfm%>%
  select(MEMBER_ID,Profile,LocationSegment)

bos.loc.specific<-merge(bos.loc.specific,mem.rfm.compact,all.x=TRUE,by='MEMBER_ID')

bos.loc.specific<-bos.loc.specific[complete.cases(bos.loc.specific),]

bos.loc.specific.summary<-bos.loc.specific%>%
  group_by(Profile)%>%
  summarise(members=n_distinct(MEMBER_ID)
            ,reservations=n()
            ,dist.mean=mean(MemDistance,na.rm=TRUE)
            ,my.deciles.10=quantile(MemDistance,probs=.1,na.rm=TRUE)
            ,my.deciles.20=quantile(MemDistance,probs=.2,na.rm=TRUE)
            ,my.deciles.30=quantile(MemDistance,probs=.3,na.rm=TRUE)
            ,my.deciles.40=quantile(MemDistance,probs=.4,na.rm=TRUE)
            ,my.deciles.50=quantile(MemDistance,probs=.5,na.rm=TRUE)
            ,my.deciles.60=quantile(MemDistance,probs=.6,na.rm=TRUE)
            ,my.deciles.70=quantile(MemDistance,probs=.7,na.rm=TRUE)
            ,my.deciles.80=quantile(MemDistance,probs=.8,na.rm=TRUE)
            ,my.deciles.90=quantile(MemDistance,probs=.9,na.rm=TRUE))

mem.boston<-member.locations.user.saved%>%
  filter(ZIPFLEET=='boston_region')
  
mem.boston$dist.to.location = distGeo(matrix(c(mem.boston$MEMBER_LONGITUDE,mem.boston$MEMBER_LATITUDE), ncol = 2),
                                        matrix(c(-71.0640061005943, 42.3517478004326), ncol = 2))*0.000621371192



mem.boston<-merge(mem.boston,mem.rfm.compact,all.x=TRUE,by='MEMBER_ID')  



mem.boston.best<-mem.boston%>%
#  filter(Profile %in% c('Power User','Strong','Standard+') & LocationSegment=='Homebody' &dist.to.location<=0.55)%>%
  filter(LocationSegment!=5 & dist.to.location<=1.25)%>%
  select(MEMBER_ID,MEMBER_LATITUDE,MEMBER_LONGITUDE,LocationSegment,Profile,dist.to.location,ADDRESS_TYPE)%>%
  unique(.)


zipIcon <- makeIcon(
  "zipcar_logo_3.png",20,20
)

leaflet(data = mem.boston.best) %>% addTiles() %>%
  addMarkers(~MEMBER_LONGITUDE, ~MEMBER_LATITUDE
             ,icon=zipIcon
             ,popup = paste0("<strong>","MEMBER ID:",mem.boston.best$MEMBER_ID
                             ,"<p>Addr: ",mem.boston.best$ADDRESS_TYPE
                             ,"<p>Usage: ",mem.boston.best$Profile
                             ,"<p>Location: ",mem.boston.best$LocationSegment)) %>%
  addCircles(lng = -71.0640061005943, lat = 42.3517478004326, weight = 1,
             radius = 1609 * 1.1)

  
#quantile(mem.dist.sum$MemDistance,probs=seq(0,1,.1),na.rm=TRUE)
dist.sum<-gather(mem.dist.sum, decile, dist.in.miles, my.deciles.10:my.deciles.90, factor_key=TRUE)

write_delim(dist.sum,"c:/users/babbenante/downloads/blake.txt",delim ="\t")

#### anything south of here might be a shit show - need to evaluate



mem.dist.sum.na <- mem.dist.sum[rowSums(is.na(mem.dist.sum)) > 0,]

mem.addr.count<-member.locations.user.saved%>%
  group_by(MEMBER_ID)%>%
  summarise(addr_count=n_distinct(ADDRESS_TYPE))%>%
  merge(.,res.sum,all.x=TRUE,by='MEMBER_ID')%>%
  mutate(DidReserve=ifelse(res>0,1,0))%>%
  group_by(addr_count,DidReserve)%>%
  summarise(member=n())

mem.cur_loc<-member.locations%>%
  filter(ADDRESS_TYPE=="cur_loc")%>%
  summarise(members=n_distinct(MEMBER_ID))

mem.num.loc<-reservations.all%>%
  group_by(MEMBER_ID)%>%
  summarise(Num_res=n()
            ,Num_loc=n_distinct(LOCATION_ID))%>%
  group_by(Num_res,Num_loc)%>%
  summarise(members=n())
  
res.sum<-reservations.all%>%
  group_by(MEMBER_ID)%>%
  summarise(res=n())


mem.addr.count<-merge(mem.addr.count,res.sum,all.x=TRUE,by='MEMBER_ID')

reservations.all$mem.dist.to.res = distGeo(matrix(c(reservations.all$MEMBER_LONGITUDE,reservations.all$MEMBER_LATITUDE), ncol = 2),
                                                matrix(c(reservations.all$LOCATION_LONGITUDE, reservations.all$LOCATION_LATITUDE), ncol = 2))*0.000621371192

IQ.dist<-quantile(reservations.all$mem.dist.to.res,probs=.75,na.rm=TRUE)-quantile(reservations.all$mem.dist.to.res,probs=.25,na.rm=TRUE)
upper.outlier<-quantile(reservations.all$mem.dist.to.res,probs=.75,na.rm=TRUE)+(IQ.dist*1.5)
lower.outlier<-quantile(reservations.all$mem.dist.to.res,probs=.25,na.rm=TRUE)-(IQ.dist*1.5)

reservations.all$Outlier <- ifelse(!is.na(reservations.all$mem.dist.to.res) & reservations.all$mem.dist.to.res > upper.outlier, 1, 0)
reservations.all[is.na(reservations.all$mem.dist.to.res),]$Outlier<-2

res.loc.summary<-reservations.all%>%
  filter(Outlier==0)%>%
  group_by(MEMBER_ID)%>%
  summarise(NumRes=n()
            ,NumLoc=n_distinct(LOCATION_ID)
            ,ResPerLoc=NumRes/NumLoc
            ,NumZone=n_distinct(ZONE)
            ,AvgDist=mean(mem.dist.to.res,na.rm=TRUE)
            ,MedDist=median(mem.dist.to.res,na.rm=TRUE))

member.rfm.compact<-member.rfm%>%
  select(MEMBER_ID,Profile)

member.rfm.compact<-merge(member.rfm.compact,res.loc.summary,all.x=TRUE,by="MEMBER_ID")
member.rfm.compact$group<-NULL
member.rfm.compact[is.na(member.rfm.compact)] <- 0

member.rfm.compact%>%
  mutate(DidReserve=ifelse(NumRes>0,1,0))%>%
  group_by(Profile,DidReserve)%>%
  summarise(members=n()
            ,res=mean(NumRes)
            ,loc=mean(NumLoc)
            ,dist=mean(AvgDist))

l1<-member.rfm.compact%>%
  filter(Profile=='Power User')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Power User) ", x="Number of Locations", y="Density") 
  
l2<-member.rfm.compact%>%
  filter(Profile=='Strong')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Strong) ", x="Number of Locations", y="Density") 

l3<-member.rfm.compact%>%
  filter(Profile=='Standard+')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Standard+) ", x="Number of Locations", y="Density") 

l4<-member.rfm.compact%>%
  filter(Profile=='Standard-')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Standard-) ", x="Number of Locations", y="Density") 

l5<-member.rfm.compact%>%
  filter(Profile=='Vulnerable')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Vulnerable) ", x="Number of Locations", y="Density") 

l6<-member.rfm.compact%>%
  filter(Profile=='Low Activity')%>%
  ggplot(., aes(NumLoc)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Location (Low Activity) ", x="Number of Locations", y="Density") 

grid.arrange(l1,l2,l3,l4,l5,l6,ncol=3)


r1<-member.rfm.compact%>%
  filter(Profile=='Power User')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Power User) ", x="Number of Reservations", y="Density") 

r2<-member.rfm.compact%>%
  filter(Profile=='Strong')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Strong) ", x="Number of Reservations", y="Density") 

r3<-member.rfm.compact%>%
  filter(Profile=='Standard+')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Standard+) ", x="Number of Reservations", y="Density") 

r4<-member.rfm.compact%>%
  filter(Profile=='Standard-')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Standard-) ", x="Number of Reservations", y="Density") 

r5<-member.rfm.compact%>%
  filter(Profile=='Vulnerable')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Vulnerable) ", x="Number of Reservations", y="Density") 

r6<-member.rfm.compact%>%
  filter(Profile=='Low Activity')%>%
  ggplot(., aes(NumRes)) +
  geom_density(adjust = 5) +
  fte_theme() +
  labs(title="Kernel Density of Reservation (Low Activity) ", x="Number of Reservations", y="Density") 

grid.arrange(r1,r2,r3,r4,r5,r6,ncol=3)



###########################################################################
# Exploratory analysis on where members reserve
# 9/15/16
###########################################################################

#unload all previously loaded packages
if(!exists("detachAllPackages", mode="function")) source("C:\\Users\\babbenante\\OneDrive - Avis Budget Group\\My Stuff\\code\\utils\\utils.r")
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
if (!require(data.table)) {
  install.packages('data.table') # faster fread() and better weekdays()
  require(data.table)
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
if (!require(geohash)) {
  install.packages('geohash') # date manipulation
  require(geohash)
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


if(!exists("getMemberData", mode="function")) source("../utils/loaddata.r")
if(!exists("load_themes")) source("../utils/themes.r")
if(!exists("diagPlot", mode="function")) source("../utils/charts.r")



#load member address lat/long
member.locations<-getMemberLocationData("Member_Locations.txt")

#load location address lat/long
pod.locations<-getPodLocationData("Zip_Locations.txt")

setDF(member.locations)
setDF(pod.locations)

#get geohashes for member and pod locations
member.locations$memberhash<-gh_encode(member.locations$ADDRESS_LAT,member.locations$ADDRESS_LONG,5)
pod.locations$geohash<-gh_encode(pod.locations$LATITUDE,pod.locations$LONGITUDE,5)
colnames(member.locations)[3]<-"MEMBER_LAT"
colnames(member.locations)[4]<-"MEMBER_LONG"
colnames(pod.locations)[5]<-"POD_LAT"
colnames(pod.locations)[6]<-"POD_LONG"



#my.x<-member.locations[1:25,]
#my.x<-member.locations
my.x<-member.locations%>%
  filter(MEMBER_REGION=='atlanta_region')
my.x$north<-north(my.x$memberhash)
my.x$northeast<-northeast(my.x$memberhash)
my.x$east<-east(my.x$memberhash)
my.x$southeast<-southeast(my.x$memberhash)
my.x$south<-south(my.x$memberhash)
my.x$southwest<-southwest(my.x$memberhash)
my.x$west<-west(my.x$memberhash)
my.x$northwest<-northwest(my.x$memberhash)

member.long<-gather(my.x,neighbor_direction,geohash,memberhash,north,northeast,east
                  ,southeast,south,southwest,west,northwest)

mem.loc.match<-merge(member.long,pod.locations,all.x=TRUE,by="geohash")

setDT(mem.loc.match)

#calculate the location distance from next reservation
mem.loc.match[ , dist.to.pod := distGeo(matrix(c(MEMBER_LONG,MEMBER_LAT), ncol = 2), 
                                            matrix(c(POD_LONG, POD_LAT), ncol = 2))]

setDF(mem.loc.match)

#convert to miles
mem.loc.match$dist.to.pod=mem.loc.match$dist.to.pod*0.000621371192

mem.loc.summary<-mem.loc.match%>%
  group_by(MEMBER_ID)%>%
  summarise(loc.nearby=sum(!is.na(dist.to.pod))
            ,loc.half.mile=sum(dist.to.pod<=.5,na.rm=TRUE)
            ,closest.dist=min(dist.to.pod,na.rm=TRUE)
            ,avg.dist=mean(dist.to.pod,na.rm=TRUE))


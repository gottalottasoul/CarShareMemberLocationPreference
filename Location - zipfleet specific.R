
#unload all previously loaded packages
pkgs = names(sessionInfo()$otherPkgs) 
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
rm(pkgs)


#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\Member Nearest Locations\\")

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

if(! "geohash" %in% installed.packages()) {
  install.packages("geohash", depend = TRUE) # get hashes for lat/long
}
require(geohash)

if(! "zoo" %in% installed.packages()) {
  install.packages("zoo", depend = TRUE) # get hashes for lat/long
}
require(zoo)

if(! "ggplot2" %in% installed.packages()) {
  install.packages("ggplot2", depend = TRUE) # get hashes for lat/long
}
require(ggplot2)

if(! "RColorBrewer" %in% installed.packages()) {
  install.packages("RColorBrewer", depend = TRUE) # get hashes for lat/long
}
require(RColorBrewer)

if(! "scales" %in% installed.packages()) {
  install.packages("scales", depend = TRUE) # get hashes for lat/long
}
require(scales)



## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

##############user defined functions##############

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
#    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.major.x = element_blank() ,
  # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black" )) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25,hjust = 0.5)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


############end user defined functions############


if(!exists("getMemberData", mode="function")) source("C:\\Users\\babbenante\\documents\\My Stuff\\code\\utils\\load data.r")



member.reservations<-getReservationData("Reservation-Details-six-month.txt")
setDF(member.reservations)

member.reservations<-member.reservations[complete.cases(member.reservations),]

member.reservations.zipfleet<-member.reservations%>%
#  filter(MEMBER_ZIPFLEET=='dallas_region' & CANCELLED_RES==0)
  filter(CANCELLED_RES==0)

zipfleet.summary<-member.reservations.zipfleet%>%
  group_by(MEMBER_ID,LOCATION_ZIPFLEET)%>%
  summarise(RESERVATIONS=n())%>%
  group_by(MEMBER_ID)%>%
  summarise(RESERVATIONS=sum(RESERVATIONS)
            ,locations=n_distinct(LOCATION_ZIPFLEET)
#            ,ZF_RES=sum(RESERVATIONS[LOCATION_ZIPFLEET=='dallas_region']))
            ,ZF_RES=sum(RESERVATIONS[LOCATION_ZIPFLEET==MEMBER_ZIPFLEET]))

zipfleet.mem.zipfleet.res<-member.reservations%>%
#  filter(MEMBER_ZIPFLEET=='dallas_region' & CANCELLED_RES==0 & LOCATION_ZIPFLEET=='dallas_region')
  filter(MEMBER_ZIPFLEET==LOCATION_ZIPFLEET & CANCELLED_RES==0)

zipfleet.mem.zipfleet.res$dist.to.pod = distGeo(matrix(c(zipfleet.mem.zipfleet.res$MEMBER_ADDR_LONGITUDE,zipfleet.mem.zipfleet.res$MEMBER_ADDR_LATTITUDE), ncol = 2),
                                              matrix(c(zipfleet.mem.zipfleet.res$LOCATION_LONGITUDE, zipfleet.mem.zipfleet.res$LOCATION_LATITUDE), ncol = 2))*0.000621371192

IntraQuantRange<-quantile(zipfleet.mem.zipfleet.res$dist.to.pod,probs=.75,na.rm=TRUE)-quantile(zipfleet.mem.zipfleet.res$dist.to.pod,probs=.25,na.rm=TRUE)
UpperRange<-(IntraQuantRange*1.5)+quantile(zipfleet.mem.zipfleet.res$dist.to.pod,probs=.75,na.rm=TRUE)
LowerRange<-quantile(zipfleet.mem.zipfleet.res$dist.to.pod,probs=.25,na.rm=TRUE)-(IntraQuantRange*1.5)

zipfleet.outliers<-zipfleet.mem.zipfleet.res%>%
  mutate(isOutlier=ifelse(dist.to.pod>UpperRange,1,0))%>%
  group_by(isOutlier)%>%
  summarise(count=n()
            ,members=n_distinct(MEMBER_ID))

zipfleet.mem.summary<-zipfleet.mem.zipfleet.res%>%
  mutate(isOutlier=ifelse(dist.to.pod>UpperRange,1,0))%>%
  filter(isOutlier==0)%>%
  group_by(MEMBER_ID,MEMBER_ZIPFLEET)%>%
  summarise(res=n()
            ,locations=n_distinct(LOCATION_ID)
            ,closest.res=min(dist.to.pod)
            ,avg.res=mean(dist.to.pod)
            ,med.res=median(dist.to.pod)
            ,farthest.res=max(dist.to.pod))
write_delim(zipfleet.mem.summary,"c:/users/babbenante/downloads/blake.txt",delim="\t")
member.pod.dist.density<-density(zipfleet.mem.summary$avg.res)
plot(member.pod.dist.density, main="Kernel Density of Distance to Pod ")
polygon(member.pod.dist.density, col="red", border="blue")
hist(zipfleet.mem.summary$locations,breaks = 20)
#Get AUC for arbitrary point* in density plot
#in the below example arbitrary point = 0.5
xt <- diff(member.pod.dist.density$x[member.pod.dist.density$x<1])
yt <- rollmean(member.pod.dist.density$y[member.pod.dist.density$x<1],2)
# This gives you the area under curve (AUC)
sum(xt*yt)


ggplot(zipfleet.mem.summary, aes(locations)) +
  geom_histogram(binwidth=1,fill="#51a601", alpha=0.75) +
  fte_theme() +
  labs(title="Distribution of Member Reserving Locations", x="# of Locations Used", y="Count of Members") +
  scale_x_continuous(breaks=seq(0,50, by=5)) +
  scale_y_continuous(labels=comma) + 
  geom_hline(yintercept=0, size=0.4, color="black")

ggplot(zipfleet.mem.summary, aes(avg.res)) +
  geom_density(fill="#51a601", alpha=0.75) +
#  geom_density(binwidth=1,fill="#51a601", alpha=0.75) +
  fte_theme() +
  labs(title="Kernel Density of Avg Location Distance", x="Pod Distance (mi)", y="Density") +
  scale_x_continuous(breaks=seq(0,5, by=.25)) +
  scale_y_continuous(labels=comma) + 
  geom_hline(yintercept=0, size=0.4, color="black")
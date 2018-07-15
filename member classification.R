###########################################################################
# Zipcar Monthly Member Drive Predictor
# 3/23/16
###########################################################################

#unload all previously loaded packages
pkgs = names(sessionInfo()$otherPkgs) 
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
rm(pkgs)


#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\Member Classification\\")

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
if (!require(ggplot2)) {
  install.packages('ggplot2') # base plots are for Coursera professors
  require(ggplot2)
}
if (!require(scales)) {
  install.packages('scales') # pairs nicely with ggplot2 for plot label formatting
  require(scales)
}
if (!require(gridExtra)) {
  install.packages('gridExtra') # a helper for arranging individual ggplot objects
  require(gridExtra)
}
if (!require(ggthemes)) {
  install.packages('ggthemes') # has a clean theme for ggplot2
  require(ggthemes)
}
if (!require(viridis)) {
  install.packages('viridis') # best. color. palette. evar.
  require(viridis)
}
if (!require(DT)) {
  install.packages('DT') # prettier data.frame output
  require(DT)
}
if (!require(caret)) {
  install.packages('caret') 
  require(caret)
}
if (!require(car)) {
  install.packages('car') 
  require(car)
}
if (!require(ROCR)) {
  install.packages('ROCR') 
  require(ROCR)
}
if (!require(Matrix)) {
  install.packages('Matrix')
  require(Matrix)
}


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()



# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}


###########Start Data Wrangling###############################################################################
####This should be one time - or at least infrequent - get all our historical data in a working format

reservations.all=read_delim("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Reservations\\Reservation-Details-June-Predictions.txt"
                            ,"\t"
                            ,col_types = cols(col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_datetime("%m/%d/%Y %H:%M")
                                              ,col_character()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()                                              
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_character()                                              
                                              ,col_integer()
                                              ,col_integer()))
reservations.all<-data.table(reservations.all)
reservations.cancels<-subset(reservations.all,CANCELLED_RES==1)
reservations.all<-subset(reservations.all,CANCELLED_RES==0)


#do we really need to factor member id?  not going to use it for anything
#reservations.all$MEMBER_ID<-as.factor(reservations.all$MEMBER_ID)
reservations.all$ZIPFLEET<-as.factor(reservations.all$ZIPFLEET)
reservations.all$SEGMENT_CLASS<-as.factor(reservations.all$SEGMENT_CLASS)
reservations.all$RENTAL_TIME_UTIL<-reservations.all$TOTAL_DRIVING_TIME/reservations.all$TOTAL_HOURS
reservations.all$DAY_OF_WEEK<-wday(reservations.all$RESERVATION_DATE)

email.activity.all<-read_delim("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Email Activity\\Member Email Activity-june-predictions.txt","\t"
                               ,col_types = cols(col_character()
                                                 ,col_date("%m/%d/%Y")
                                                 ,col_character()))

members.all=read_delim("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Members\\MembersMonthly - June predictions.txt"
                       ,"\t"
                       ,col_types = cols(col_character()
                                         ,col_character()
                                         ,col_character()
                                         ,col_character()
                                         ,col_date("%m/%d/%Y")
                                         ,col_date("%m/%d/%Y")
                                         ,col_character()))
members.all<-data.table(members.all)
#members.all$MEMBER_ID<-as.factor(members.all$MEMBER_ID)
members.all$ZIPFLEET<-as.factor(members.all$ZIPFLEET)
members.all$BUSINESS_SEGMENT<-as.factor(members.all$BUSINESS_SEGMENT)
members.all$RATEPLAN<-as.factor(members.all$RATEPLAN)
members.all$SNAPSHOT_YM<-as.factor(members.all$SNAPSHOT_YM)
#members.all$FIRST_JOIN<-as.POSIXct(strptime(members.all$FIRST_JOIN,"%m/%d/%Y"))
#members.all$FIRST_RES<-as.POSIXct(strptime(members.all$FIRST_RES,"%m/%d/%Y %H:%M:%S %p"))
members.all$FIRST_JOIN_COHORT<-paste(format(members.all[,FIRST_JOIN],"%Y-%m"))
members.all$FIRST_JOIN_COHORT<-as.factor(members.all$FIRST_JOIN_COHORT)
members.all[,TIME.TO.FIRST.DRIVE := difftime(FIRST_RES,FIRST_JOIN,units="days")]


#Change the 'current month' here based on the snapshot date to get correctly member-aged customers
#members.all[,MONTHS.TO.CURRENT := ceiling(as.numeric(difftime(as.Date('2015-06-01'),FIRST_JOIN,units="weeks"))/52*12)] 

member.monthly<-members.all%>%
  dcast(MEMBER_ID+FIRST_JOIN+FIRST_RES+FIRST_JOIN_COHORT+TIME.TO.FIRST.DRIVE+MONTHS.TO.CURRENT~SNAPSHOT_YM, value.var=c("ZIPFLEET","BUSINESS_SEGMENT","RATEPLAN"))

############Start here for repeating steps in building data frame ################
#next month to run for is april
snapshot_str="2016-MAY"
snapshot_mth="MAY"
prior_floor="2015-11-01"
prior_ceiling="2016-05-31"
curr_floor="2016-06-01"
curr_ceiling="2016-06-30"


member.monthly[,MONTHS.TO.CURRENT := MONTHS.TO.CURRENT+1]

#########################################update column names for applicable month here as well
month.ltv<-member.monthly[, c('MEMBER_ID',paste0('ZIPFLEET_',snapshot_str)
                              ,paste0('BUSINESS_SEGMENT_',snapshot_str)
                              ,paste0('RATEPLAN_',snapshot_str)
                              ,'MONTHS.TO.CURRENT')
                          , with=FALSE]
month.ltv<-na.omit(month.ltv)
names(month.ltv)[2]<-"ZIPFLEET"
names(month.ltv)[3]<-"BUSINESS.SEGMENT"
names(month.ltv)[4]<-"RATEPLAN"


prev.6.month.res<-reservations.all%>%
#################################################################regex should match for the previous 6 months  
  filter(RESERVATION_DATE>=as.POSIXct(prior_floor),RESERVATION_DATE<=as.POSIXct(prior_ceiling))%>%
  group_by(MEMBER_ID,DAY_OF_WEEK)%>%
  summarise(Reservations=n()
            ,WEB_RES=sum(WEB_RES)
            ,APP_RES=sum(APP_RES)
            ,HOURLY_RES=sum(HOURLY_RES)
            ,DAILY_RES=sum(DAILY_RES)
            ,OVERNIGHT_RES=sum(OVERNIGHT_RES)
            ,UP_REVENUE=sum(UP_REVENUE)
            ,PENALTY_CHARGE=sum(PENALTY_CHARGE)
            ,TOTAL_HOURS=sum(TOTAL_HOURS)
            ,TOTAL_DRIVING_TIME=sum(TOTAL_DRIVING_TIME)
            ,TOTAL_DISTANCE=sum(TOTAL_DISTANCE)
            ,RESERVATION_DATE=max(RESERVATION_DATE))%>%
  mutate(MaxRes=max(Reservations))%>%
  group_by(MEMBER_ID)%>%
  summarise(Reservations=sum(Reservations)
            ,Web_Reservations=sum(WEB_RES)
            ,App_Reservations=sum(APP_RES)
            ,Hourly_Reservations=sum(HOURLY_RES)
            ,Daily_Reservations=sum(DAILY_RES)
            ,Overnight_Reservations=sum(OVERNIGHT_RES)
            ,Web_Res_Share=round(sum(WEB_RES)/sum(Reservations),2)
            ,App_Res_Share=round(sum(APP_RES)/sum(Reservations),2)
            ,Hourly_Res_Share=round(sum(HOURLY_RES)/sum(Reservations),2)
            ,Daily_Res_Share=round(sum(DAILY_RES)/sum(Reservations),2)
            ,Overnight_Res_Share=round(sum(OVERNIGHT_RES)/sum(Reservations),2)
            ,Revenue=sum(UP_REVENUE)
            ,Rented_Hours=sum(TOTAL_HOURS)
            ,Used_Hours=sum(TOTAL_DRIVING_TIME)
            ,Distance_Driven=sum(TOTAL_DISTANCE)
            ,RevPerRes=round(sum(UP_REVENUE)/sum(Reservations),2)
            ,Penalty_Charge=sum(PENALTY_CHARGE)
            ,Utilization=round(sum(TOTAL_DRIVING_TIME)/sum(TOTAL_HOURS),2)
            ,WeekDayDist=n_distinct(DAY_OF_WEEK)
            ,MaxRes=mean(MaxRes)
            ,Last_Rent=max(RESERVATION_DATE))%>%
  mutate(Recency=as.duration(interval(Last_Rent,as.Date(curr_floor))))%>%
  mutate(Last_Rent= ifelse(Recency<dweeks(4), 1,
                      ifelse(Recency<dweeks(8), 2,
                      ifelse(Recency<dweeks(12), 3,
                      ifelse(Recency<dweeks(16), 4,
                      ifelse(Recency<dweeks(20), 5,
                      ifelse(Recency<dweeks(24), 6,0)))))))%>%
  mutate(Recency=NULL)


prev.6.month.email<-email.activity.all%>%
  #################################################################regex should match for the previous 6 months  
  filter(EVENT_DATE>=as.Date(prior_floor),EVENT_DATE<=as.Date(prior_ceiling))%>%
  group_by(MEMBER_ID)%>%
  summarise(opens=sum(EVENT_ACTION=='open')
            ,clicks=sum(EVENT_ACTION=='click'))

prev.6.month.reservation.cancels<-reservations.cancels%>%
  #################################################################regex should match for the previous 6 months  
  filter(RESERVATION_DATE>=as.POSIXct(prior_floor),RESERVATION_DATE<=as.POSIXct(prior_ceiling))%>%
  group_by(MEMBER_ID)%>%
  summarise(cancellations=n())

  
current.month.res<-reservations.all%>%
########################################now we want the *next* month - ie, the one we're predicting
  filter(RESERVATION_DATE>=as.Date(curr_floor),RESERVATION_DATE<=as.Date(curr_ceiling))%>%
  group_by(MEMBER_ID)%>%
  summarise(CurrentReservations=n())%>%
  mutate(DidReserve=1)

train.data<-merge(month.ltv,prev.6.month.res,all.x=TRUE,by="MEMBER_ID")
train.data<-merge(train.data,prev.6.month.reservation.cancels,all.x=TRUE,by="MEMBER_ID")
train.data<-merge(train.data,prev.6.month.email,all.x=TRUE,by="MEMBER_ID")
train.data<-merge(train.data,current.month.res,all.x = TRUE,by="MEMBER_ID")
train.data[is.na(train.data)]<-0
train.data$DidReserve<-as.factor(train.data$DidReserve)
train.data$Last_Rent<-as.factor(train.data$Last_Rent)
###############################add two new columns - one as a label for the period we're predicting
#################################and one for month so we can (potentially) use that as a predictor
train.data$CurrentPeriod<-snapshot_str
train.data$CurrentMonth<-snapshot_mth

#train.data.master<-train.data
gc()
train.data.master<-rbind(train.data.master,train.data)
rm(train.data)
gc()

################################################################################
############End here for repeating steps in building data frame ################
################################################################################


train.data.master$CurrentPeriod<-as.factor(train.data.master$CurrentPeriod)
train.data.master$CurrentMonth<-factor(train.data.master$CurrentMonth,levels=c("JUL","JAN","FEB","MAR","APR","MAY","JUN","AUG","SEP","OCT","NOV","DEC"))
#set july as reference month
train.data.master$CurrentMonth<-as.character(train.data.master$CurrentMonth)
train.data.master$CurrentMonth<-"MAY"
#set consumer as reference segment
train.data.master$BUSINESS.SEGMENT<-relevel(train.data.master$BUSINESS.SEGMENT,ref=3)
#Set philly as reference zipfleet
train.data.master$ZIPFLEET<-relevel(train.data.master$ZIPFLEET,ref=23)
save(train.data.master, file="C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Classification\\Member6MonthPerfSnapshot.RData")
load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Classification\\Member6MonthPerfSnapshot.RData")

###########End Data Wrangling###############################################################################


###########Start Modeling/Discovery#########################################################################

#test.feb<-subset(train.data.master, CurrentPeriod=='2016-FEB')
#test.mar<-subset(train.data.master, CurrentPeriod=='2016-MAR')

train.data.master<-subset(train.data.master, CurrentPeriod !='2016-FEB' & CurrentPeriod != '2016-MAR')


##our data set is huge - let's not use it all, start with 25%
#smp_size <- floor(0.25 * nrow(train.data.master))

## set the seed to make your partition reproductible
#set.seed(910)
#train_ind <- sample(seq_len(nrow(train.data.master)), size = smp_size)

#train <- train.data.master[train_ind, ]
#test <- train.data.master[-train_ind, ]

##now split our 25% 25/75 test and train
#smp_size <- floor(0.75 * nrow(train))

## set the seed to make your partition reproductible
#set.seed(131)
#train_ind <- sample(seq_len(nrow(train)), size = smp_size)
#train.smp <- train[train_ind, ]
#test.smp <- train[-train_ind, ]

#save(test.feb,test.mar,train.smp,test.smp, file="C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Classification\\Member6MonthPerfSnapshot-subset.RData")
#load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Classification\\Member6MonthPerfSnapshot.RData")
load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Classification\\Member6MonthPerfSnapshot-subset.RData")



##naive model for variable selection

base<-glm(DidReserve~Reservations,family=binomial,data=test.smp)
naive.model.min=formula(glm(DidReserve~Reservations,family=binomial,data=test.smp))
naive.model.max=formula(glm(DidReserve~Reservations+Web_Reservations+App_Reservations
                      +Hourly_Reservations+Daily_Reservations+Overnight_Reservations
                      +Web_Res_Share+App_Res_Share+Hourly_Res_Share+Daily_Res_Share
                      +Overnight_Res_Share+Revenue+Rented_Hours+Used_Hours+Distance_Driven
                      +RevPerRes+Penalty_Charge+Utilization+WeekDayDist+MaxRes+MONTHS.TO.CURRENT
                      +cancellations+opens+clicks+Last_Rent+ZIPFLEET+BUSINESS.SEGMENT+RATEPLAN+CurrentMonth
                      ,family=binomial,data=test.smp))
naive.model.varselection<-step(base,direction="forward",scope=list(lower=naive.model.min, upper=naive.model.max),trace=1)




member.likelihood.lm<-glm(DidReserve ~ Reservations + Last_Rent + WeekDayDist + 
                            MaxRes + MONTHS.TO.CURRENT + RATEPLAN + CurrentMonth + ZIPFLEET + 
                            Penalty_Charge + BUSINESS.SEGMENT + RevPerRes + cancellations + 
                            Revenue + Web_Reservations + opens + clicks + App_Res_Share + 
                            Rented_Hours + Hourly_Reservations + Daily_Reservations + 
                            Overnight_Reservations + Web_Res_Share + App_Reservations + 
                            Distance_Driven + Overnight_Res_Share + Hourly_Res_Share
                     ,family=binomial,data=train.smp)
vif(member.likelihood.lm)
member.likelihood.lm.prob<-predict(member.likelihood.lm,train.data,type="response",se.fit=FALSE)
#apr.pred<-test.apr[,MEMBER_ID]
fitted.results.cat <- ifelse(member.likelihood.lm.prob >= 0.5,1,0)
#write.csv(fitted.results.cat, "C:\\Users\\Babbenante\\downloads\\blake.csv")
confusionMatrix(fitted.results.cat,test.mar$DidReserve,positive="1")


member.likelihood.rf<-train(DidReserve ~ Reservations + Last_Rent + WeekDayDist + 
                  MaxRes + MONTHS.TO.CURRENT + RATEPLAN + CurrentMonth + ZIPFLEET + 
                  Penalty_Charge + BUSINESS.SEGMENT + RevPerRes + cancellations + 
                  Revenue + Web_Reservations + opens + clicks + App_Res_Share + 
                  Rented_Hours + Hourly_Reservations + Daily_Reservations + 
                  Overnight_Reservations + Web_Res_Share + App_Reservations + 
                  Distance_Driven + Overnight_Res_Share + Hourly_Res_Share
                ,data=test.feb
                ,method="rf"
                ,trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)


member.likelihood.rf = randomForest(DidReserve ~ Reservations + Last_Rent + WeekDayDist + 
                                      MaxRes + MONTHS.TO.CURRENT + RATEPLAN + CurrentMonth + ZIPFLEET + 
                                      Penalty_Charge + BUSINESS.SEGMENT + RevPerRes + cancellations + 
                                      Revenue + Web_Reservations + opens + clicks + App_Res_Share + 
                                      Rented_Hours + Hourly_Reservations + Daily_Reservations + 
                                      Overnight_Reservations + Web_Res_Share + App_Reservations + 
                                      Distance_Driven + Overnight_Res_Share + Hourly_Res_Share
                                    ,data=train.smp
                                    ,ntree=100
                                    ,nodesize=50)
member.likelihood.rf.pred = predict(member.likelihood.rf, newdata=test.smp, type="prob") 
fitted.results.cat <- ifelse(member.likelihood.rf.pred[,2] > 0.5,1,0)
confusionMatrix(test.smp$DidReserve, fitted.results.cat, positive="1")




member.likelihood.rocr<-prediction(member.likelihood.lm.prob,test.smp$DidReserve)
perf<-performance(member.likelihood.rocr,"tpr","fpr")
plot(perf)
auc.tmp <- performance(member.likelihood.rocr,"auc") 
auc <- as.numeric(auc.tmp@y.values)

cuts <- bins(my.mod.prob, target.bins = 5, minpts = 2000)
cuts$breaks <- bins.getvals(cuts)
cuts$binct
my.mod.prod.rd<-round(my.mod.prob,digits=2)
hist(my.mod.prod.rd)


member.drives.lm<-lm(CurrentReservations~Reservations+Daily_Reservations
                     +Web_Res_Share+App_Res_Share+Daily_Res_Share
                     +Overnight_Res_Share+Rented_Hours+Distance_Driven
                     +RevPerRes+MONTHS.TO.CURRENT+Last_Rent+ZIPFLEET
                     +BUSINESS.SEGMENT+RATEPLAN+CurrentMonth
                     ,data=train.smp)
member.drives.lm.pred<-predict(member.drives.lm,my.feb.data,type="response")

my.results<-data.frame(test.mar[, c('MEMBER_ID','DidReserve','CurrentReservations'), with=FALSE])
my.results<-data.frame(train.data[, c('MEMBER_ID','ZIPFLEET'), with=FALSE])
my.results$Likelihood<-member.likelihood.lm.prob
my.results$Error<-my.results$PredictedRes-my.results$CurrentReservations
my.results$PredictedRes<-member.drives.lm.pred
rmse(my.results$Error)
mae(my.results$Error)

my.results$Likelihood<-my.mod.prob


write.csv(my.results, "C:\\Users\\Babbenante\\downloads\\blake.csv")  





member.drives.simple.lm<-lm(CurrentReservations~Reservations,data=test.feb)




sparse_matrix = sparse.model.matrix(DidReserve~Reservations+Daily_Reservations
                                    +Web_Res_Share+App_Res_Share+Daily_Res_Share
                                    +Overnight_Res_Share+Rented_Hours+Distance_Driven
                                    +RevPerRes+MONTHS.TO.CURRENT+Last_Rent+ZIPFLEET
                                    +BUSINESS.SEGMENT+RATEPLAN+CurrentMonth-1, data = train.smp)
output_vector = train.smp[,Y:=0][DidReserve == "1",Y:=1][,Y]
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 9,
               eta = 1, nthread = 2, nround = 10,objective = "binary:logistic")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
xgb.plot.importance(importance[1:10,])
xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = bst, n_first_tree = 2)
member.likelihood.rf.pred = predict(member.likelihood.rf, newdata=test.smp, type="prob") 
sparse_matrix.test = sparse.model.matrix(DidReserve~Reservations+Daily_Reservations
                                    +Web_Res_Share+App_Res_Share+Daily_Res_Share
                                    +Overnight_Res_Share+Rented_Hours+Distance_Driven
                                    +RevPerRes+MONTHS.TO.CURRENT+Last_Rent+ZIPFLEET
                                    +BUSINESS.SEGMENT+RATEPLAN+CurrentMonth-1, data = test.mar)
member.likelihood.xgboost.pred<-predict(bst,newdata=sparse_matrix.test)
fitted.results.cat <- ifelse(member.likelihood.xgboost.pred > 0.5,1,0)
confusionMatrix(test.mar$DidReserve, fitted.results.cat, positive="1")
write.csv(member.likelihood.xgboost.pred, "C:\\Users\\Babbenante\\downloads\\blake.csv")  



#######
test.apr<-merge(month.ltv,prev.6.month.res,all.x=TRUE,by="MEMBER_ID")
test.apr[is.na(test.apr)]<-0
test.apr$Last_Rent<-as.factor(test.apr$Last_Rent)
train.data$CurrentMonth<-"JUN"
train.data$CurrentMonth<-as.factor(train.data$CurrentMonth)
levels(train.data$CurrentMonth)<-c("JUN","JAN","FEB","MAR","APR","MAY","JUL","AUG","SEP","OCT","NOV","DEC")
train.data$CurrentMonth<-"JUN"




set.seed(747)
smp_size <- floor(0.01 * nrow(test.smp))
my_ind <- sample(seq_len(nrow(test.smp)), size = smp_size)
my.smp <- data.frame(test.smp[my_ind, ])

my.smp<-my.smp%>%filter(Reservations>0)%>%
  filter(Revenue<1000)

my.pca<-prcomp(my.smp[,6:23],center=TRUE,scale. = TRUE)
plot(my.pca)
summary(my.pca)

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(my.pca, obs.scale = 1, var.scale = 1, 
              groups = my.smp[,5], ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


require(ggplot2)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(my.pca$rotation, 
                       .names = row.names(my.pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")



train.data.master<-subset(train.data.master
                          ,CurrentPeriod %in% c('2015-JAN','2015-FEB','2015-MAR','2015-APR'
                                                ,'2015-MAY','2015-JUN','2015-JUL','2015-AUG'
                                                ,'2015-SEP','2015-OCT','2015-NOV','2015-DEC'
                                                ,'2016-JAN','2016-FEB','2016-MAR'))


levels(train.data$ZIPFLEET)<-c('philadelphia_region','atlanta_region','austin_region','baltimore_region'
,'boston_region','chicago_region','non_used_region','country_us_agencies'
,'country_us_airports','dallas_region','dc_region','denver_region'
,'detroit_region','non_used_region','non_used_region','non_used_region'
,'hawaii_region','houston_region','los_angeles_region','miami_region'
,'milwaukee_region','minneapolis_region','ny_region','pittsburgh_region'
,'portland_region','providence_region','sacramento_region','san_diego_region'
,'sanfrancisco_region','seattle_region','toronto_region','vancouver_region')


save(member.likelihood.rf,member.likelihood.lm, file="C:\\Users\\Babbenante\\documents\\my stuff\\code\\Member Classification\\Models\\MemberClassificationModels.RData")
load("C:\\Users\\Babbenante\\documents\\my stuff\\code\\Member Classification\\Models\\MemberClassificationModels.RData")
merged<-read.csv(file.choose(), stringsAsFactors = FALSE,na.strings = '') #Importing the csv file


merged$consent_id <- paste(merged$consent_id,merged$type)
merged <- merged[complete.cases(merged$log_deflated_project_value), ] #Omitting one missing case
merged$consent_date <- dmy(merged$consent_date)  #Converting into appropriate date format
merged$consent_quarter<-dmy(merged$consent_quarter)
merged$date_completed<-dmy(merged$date_completed)


summary(merged)
head(merged)
dim(merged)
library(lubridate)


str(merged$consent_date)
dim(merged)

str(merged)
merged$type<-as.factor(merged$type)
merged$ta<-as.factor(merged$ta)
merged$ta13<-as.factor(merged$ta13)
merged$region13<-as.factor(merged$region13)
merged$year_group_cancelled<-as.factor(merged$year_group_cancelled)
merged$TA_group_lag<-as.factor(merged$TA_group_lag)
merged$house<-as.factor(merged$house)
merged$consent_id<-as.factor(merged$consent_id)
merged$qbas_id<-as.factor(merged$qbas_id)
merged$meshblock13<-as.factor(merged$meshblock13)
merged$cancelled_train<-as.factor(merged$cancelled_train)
merged$completed<-as.factor(merged$completed)
merged$cancelled<-as.factor(merged$cancelled)
merged$ongoing<-as.factor(merged$ongoing)
merged$year_group_cancelled<-as.factor(merged$year_group_cancelled)
merged$month_group_lag<-as.factor(merged$month_group_lag)

merged$completion_lag<-as.numeric(merged$completion_lag)
merged$completion_lag_train<-as.numeric(merged$completion_lag_train)

merged$deflated_project_value<-as.numeric(merged$deflated_project_value)
merged$completed_value<-as.numeric(merged$completed_value)





head(merged)
summary(merged)
sapply(merged,class)
dim(merged)
sapply(merged,class)



##############Training and testing dataset for linear and logistic model
##For Linear
linear_training<-merged[complete.cases(merged[ , 26]),]
linear_testing<-merged[!complete.cases(merged[ , 26]),]
dim(linear_training)
dim(linear_testing)

#For Logistics
log_training<-merged[complete.cases(merged[ , 25]),]
log_testing<-merged[!complete.cases(merged[ , 25]),]
dim(log_training)
dim(log_testing)

#Releveling for linear model based on linear_training

linear_training$house<-relevel(linear_training$house, ref='1')
linear_training$TA_group_lag<-relevel(linear_training$TA_group_lag,ref='other')
linear_training$month_group_lag<-relevel(linear_training$month_group_lag, ref='oct99-mar03')
class(linear_training$completion_lag_train)

head(merged)

#Linear Regression Model
lineartrainingmodel<-lm(completion_lag_train~log_deflated_project_value+month_group_lag+house*log_deflated_project_value+TA_group_lag,data=linear_training)
summary(lineartrainingmodel)
head(linear_training)  
head(linear_testing)
linear_training$pred<-fitted(lineartrainingmodel)

#Predicted for linear_testing dataset
linear_testing$pred<-predict(lineartrainingmodel,linear_testing)


##Logistic Regression Model
log_training$house<-relevel(log_training$house, ref='1')
log_training$TA_group_lag<-relevel(log_training$TA_group_lag,ref='other')
log_training$month_group_lag<-relevel(log_training$month_group_lag, ref='oct99-mar03')
summary(log_training)
log_training$year_group_cancelled<-relevel(log_training$year_group_cancelled,ref='other')
logtrainingcancellationmodel<-glm(cancelled_train ~ year_group_cancelled+log_deflated_project_value+house+house*log_deflated_project_value,family="binomial",data=log_training)
summary(logtrainingcancellationmodel)
head(log_training)


#Predicting Probabilities
log_training$pred<-round(fitted(logtrainingcancellationmodel),2)
head(log_training)

log_testing$pred<-predict(logtrainingcancellationmodel,log_testing,type="response")
head(log_testing)
dim(log_testing)
dim(log_training)


#Merging training and testing data from linear and logistic regression
finaldatalinear<-rbind(linear_training,linear_testing)
dim(finaldatalinear)
head(finaldatalinear)

finaldatalogistic<-rbind(log_training,log_testing)
dim(finaldatalogistic)
head(finaldatalogistic)


install.packages("tidyr")
install.packages("dplyr")
library("tidyverse")
library(dplyr)

install.packages("magrittr")
library(magrittr)
library(data.table)
library(dplyr)
linear_merged<-finaldatalinear %>% mutate(lag=ifelse(is.na(completion_lag),pred,completion_lag))
head(linear_merged)
linear_merged$consent_quarter<- as.Date("linear_merged$consent_quarter",format= "%m/%d/%Y")
linear_merged$consent_quarter <- format(as.Date(linear_merged$consent_quarter, "%Y-%m"))

#xxxxxx
linear_merged$consent_quarter<-dmy(linear_merged$consent_quarter)
linear_merged$consent_date<-dmy(linear_merged$consent_date)
#xxxxxxxxx


summary(linear_merged)
head(linear_merged$lag)
head(log_training$consent_date)
head(linear_merged$consent_date)

head(linear_merged$consent_date,10)
head(linear_merged$lag)

summary(linear_merged)
dim(linear_merged)
#Data Manipulation 
#Make a new variable called quarter which will be sum of consent date and (lag/12)*365
#Defining a new variable to store the increment in date and once the date is converted to quarter, the variable will be added to the main dataset 

c<-days(as.integer((linear_merged$lag)/12*365))

linear_merged$temp<-as.POSIXlt(linear_merged$consent_date)+c  #Create a temporary variable to add the consent date to the lag, which will be converted into quarter
dim(linear_merged)

head(linear_merged)
library(zoo)
linear_merged$quarter<- as.yearqtr(linear_merged$temp, format = "%Y-%m-%d")

head(linear_merged$quarter)
head(linear_merged)
dim(linear_merged)

#Checking Logistic dataset so as to make it ready to merge it with the main dataset
head(finaldatalogistic)

#Now that all the derived variables are data-ready, we need to extract these variables and them to the main dataset, common variable =Consent_id, skip the sksk variable

derived_linear<-linear_merged[,c(1,27,28,30)]
head(derived_linear)

derived_logistic<-finaldatalogistic[,c(1,27)]
summary(derived_logistic)
head(derived_logistic)
summary(derived_logistic)
leftjoin<-merge(derived_linear,derived_logistic,by=c("consent_id"),all.x=T)
head(leftjoin) #predx: represents lag in months; pred.y represents cancellation probabibility 
summary(leftjoin)

#Since there was a common name for both the pred variables, renaming it to pred_lag and predprob_cancelled
library(reshape)
leftjoin <- rename(leftjoin, c(pred.x="pred_lag"))
leftjoin <- rename(leftjoin, c(pred.y="predprob_cancelled"))
head(leftjoin)


leftjoin$quarter<-as.yearqtr(leftjoin$quarter)
head(leftjoin)

#CONVERTING DATE FOR THE Main dataset
merged$consent_date <- dmy(merged$consent_date)
merged$consent_quarter<-dmy(merged$consent_quarter)
merged$date_completed<-dmy(merged$date_completed)
summary(merged)

#Merging the main dataframe with leftjoin based on consent_id
dim(leftjoin)
dim(merged)
summary(merged)
new_merged<-merge(merged,leftjoin,by=c("consent_id"),all.x=T)
dim(new_merged)
head(new_merged)


length(unique(new_merged$consent_id))
dim(new_merged)
length(unique(new_merged$consent_id)) == nrow(new_merged)


n_occur <- data.frame(table(merged$consent_id))
n_occur[n_occur$Freq > 1,]
new_merged[new_merged$consnet_id %in% n_occur$Var1[n_occur$Freq > 1],]

dim(new_merged[duplicated(new_merged$consnet_id),])[1]
class(new_merged$consent_quarter)

#Adding a new variable completeddate
d <- as.Date(new_merged$consent_date)
new_merged$lag<-round(new_merged$lag)
month(d) <- month(d) + new_merged$lag
day(d) <- days_in_month(d)
d
summary(linear_merged)
summary(derived_logistic)
new_merged$completeddate<-d

o3<-filter(new_merged, new_completed>1)
dim(o3)
head(o3)
o3
dim(new_merged)
new_merged<-new_merged[c(-32,-33)]
new_merged$oneminuspredprob<-1-(new_merged$predprob_cancelled)
summary(new_merged)


#Completed
new_merged$new_completed<-ifelse(!is.na(new_merged$completed), new_merged$completed, 
                              ifelse(new_merged$completeddate <=ymd("2016-10-01") , (new_merged$predprob_cancelled),0))


new_merged$new_completed<-ifelse(!is.na(new_merged$completed), new_merged$completed, 
                                 ifelse( new_merged$consent_quarter  > as.Date('1990-01-01') & new_merged$consent_quarter <= as.Date('2016-10-01')  , (1-new_merged$predprob_cancelled),0))
head(new_merged$new_completed)

#Ongoing
new_merged$new_ongoing<-ifelse(!is.na(new_merged$ongoing), new_merged$ongoing, 
                               ifelse(new_merged$quarter <=as.Date("2016-10-01") , 0,(1-new_merged$predprob_cancelled)))

tail(new_merged$new_ongoing,100)


#Cancelled
new_merged$new_cancelled<-ifelse(!is.na(new_merged$cancelled),new_merged$cancelled,new_merged$predprob_cancelled)
dim(new_merged)
head(new_merged)

#Derive a new variable called completion_probabibilty
library(dplyr)
new_merged <- new_merged %>% mutate(completion_probability = new_completed * dwellings_consented)
View(new_merged)
summary(new_merged)

#Adding a new column to determine the quarter #Note the format between the variable quarter and consent_quarter_new. Different ways to represent
new_merged$consent_quarter_new<-as.Date(new_merged$consent_date)
new_merged$consent_quarter_new<-quarter(new_merged$consent_quarter_new,with_year = T)
head(new_merged)
#Adding a new column named completed by adding lag to consentquarternew 
new_merged$completeddate<-(new_merged$consent_date+new_merged$lag)
View(new_merged)
head(new_merged)
new_merged<-select(new_merged,-completedmonth)

class(new_merged$compl)
###
new_merged$completed_quarter_new<-as.Date(new_merged$completeddate)
new_merged$completed_quarter_new<-quarter(new_merged$completed_quarter_new,with_year = T)
aggregate(completion_probability~completed_quarter_new,FUN=sum,data=new_merged)
###


summary(new_merged)
the.best <-  < baskets.of.Granny




#Rounding lag column
new_merged$lag<-round(new_merged$lag)
class(new_merged$lag)

#Convert the consent_date to appropriate format and add it to lag 
require(lubridate)
month(d) <- month(new_merged$consent_date)+new_merged$lag
day(d) <- days_in_month(d)
d  

#Adding a new column completed date derived 
new_merged$date_completed_derived <- format( as.yearqtr(d))

#Summarising based on text
y<-new_merged$date_completed_derived
a<-cbind(freq=table(y))
a

dim(new_merged)
unique(new_merged$dwellings_completed_new)


aggregate(dwellings_consented~consent_quarter_new,FUN=sum,data=new_merged)
head(new_merged)
summary(new_merged)

max(new_merged$dwellings_completed_new)



min(new_merged$verynew)
max(merged$dwellings_consented)
###
head(new_merged)
aggregate(dwellings_consented~consent_id,FUN=sum,new_merged)

aggregate(completion_probability~consent_quarter_new,FUN=sum,data=new_merged)



#####
new_merged$consentedmonth<-as.POSIXct(new_merged$consent_date)
new_merged$consentedmonthnew<-format(new_merged$consentedmonth,"%Y-%m")


t<-aggregate(dwellings_consented~consentedmonthnew,FUN=sum,new_merged)
t
mutate(t,lead(dwellings_consented),2)
i
new_merged[duplicated(new_merged), ]
dim(unique(new_merged))

o2<-filter(t, consent_id>1)
o2
dim(o2)
head(new_merged)

new_merged$dwellings_completed_new<- (new_merged$dwellings_consented*new_merged$new_completed)

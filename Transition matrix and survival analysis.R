survival_data<-merged[complete.cases(merged[ , 24]),]
survival_testing<-merged[!complete.cases(merged[ , 24]),]
survival_testing_new<-filter(survival_testing, consent_year > 2000 & consent_year < 2002)
summary(survival_testing)
dim(survival_testing)
#replace duplicate id's by merging two columns
survival_data$consent_id <- paste(survival_data$consent_id,"_",survival_data$type)
#Take off the negative values from the completion lag column
survival_data<-subset(survival_data, survival_data[ , 24] > 0)  
#New variable based on quarter
summary(survival_data)
dim(survival_data)

#Converting the data into appropriate quarter format. 
survival_data$consent_quarter_new<-as.Date(survival_data$consent_date)
survival_data$consent_quarter_new<-quarter(survival_data$consent_quarter_new,with_year = T)
head(survival_data$consent_quarter_new,100)
class(survival_data$consent_quarter_new)


#break down the data based on grouping 3 year period
library(dplyr)
p<-filter(survival_data, consent_year > 1999)
dim(p)

p1<-filter(survival_data, consent_quarter_new == 2000.1 & consent_quarter_new==2000.3)
head(p1)
a<-subset( survival_data,consent_quarter_new< 2000.1 & consent_quarter_new > 2000.3)
tail(survival_data$consent_quarter_new,100)
p1<-subset( survival_data,consent_quarter_new>= 2000.1 & consent_quarter_new<=2002.4)
summary(p1)
(p1$consent_quarter_new)

#Create a survival object and plot the trajectory
survival.obj1<-Surv(p1$completion_lag,p1$completed)
survdist1<-survfit((survival.obj1)~1)
plot(survdist1,col=c("red","blue","green","yellow"), xlim = c(0,30))
abline(h=0.9,v=20)



#Repeat the above process for different timelines p2,p3,p4,p5
p2<-subset( survival_data,consent_quarter_new>= 2003.1 & consent_quarter_new<=2005.4)
summary(p2)
survival.obj2<-Surv(p2$completion_lag,p2$completed)
survdist2<-survfit((survival.obj2)~1)
plot(survdist2,col=c("red","blue","green","yellow"), xlim = c(0,30))


p3<-subset( survival_data,consent_quarter_new>= 2006.1 & consent_quarter_new<=2008.4)
summary(p3)
survival.obj3<-Surv(p3$completion_lag,p3$completed)
survdist3<-survfit((survival.obj3)~1)
plot(survdist3,col=c("red","blue","green","yellow"), xlim = c(0,30))



p4<-subset( survival_data,consent_quarter_new>= 2009.1 & consent_quarter_new<=2011.4)
summary(p4)
survival.obj4<-Surv(p4$completion_lag,p4$completed)
survdist4<-survfit((survival.obj4)~1)
plot(survdist4,col=c("red","blue","green","yellow"), xlim = c(0,30))




p5<-subset( survival_data,consent_quarter_new>= 2012.1 & consent_quarter_new<=2014.4)
summary(p5)

survival.obj5<-Surv(p5$completion_lag,p5$completed)
survdist5<-survfit((survival.obj5)~1)
plot(survdist5,col=c("red","blue","green","yellow"), xlim = c(0,30),,ylab="completion probability",xlab="lag in months")
library(tidyr)
survival_data<-tbl_df(survival_data)

#EXPERIMENTAL
#Log rank test. Tried running the Log rank test on the survival_data, however log rank test 
#expects censored events as well and our dataset has only completed dataset. The log rank test can be implemented
#on the dataset with censored/cancelled dwellings
survdiff((survival.obj)~house,data=survival_data)
summary(survival_data)
survival_data[survival_data$completion_lag_train >= 0]

#Setting up base variable
survival_data$year_group_cancelled<-relevel(survival_data$year_group_cancelled, ref='other')
survival_data$TA_group_lag<-relevel(survival_data$TA_group_lag, ref='other')


#EXPERIMENTAL
##Cox model #Cox model usually displays the survival probability across time period. 
#It was not one of the research question to be solved, currently, however for future scope, we can estimate
#probability of completion for various time lengths

install.packages("pec")
library(pec)
cox1<-coxph(Surv(completion_lag,completed)+dwellings_consented+log_deflated_project_value+month_group_lag+date_completed+year_group_cancelled,data=survival_data)
summary(cox1)
survival_testing$new<-predict(cox1,newdata=survival_testing,type=c("expected",))
survival_testing$new1<-1-survival_testing$new
head(survival_testing)
plot(survfit(cox1))
dim(survival_testing$new1)
summary(survival_testing)

#Prediction
survival_testing_new$prob<-predictSurvProb(timemodel,survival_testing_new,times=10)
head(survival_testing_new)

dim(survival_testing_new)
head(survival_testing_new)

dim(survival_data)
head(survival_data)
plot(km_fit)

km_trt_fit <- survfit(Surv(completion_lag, completed) ~ house, data=survival_data)
plot(km_trt_fit)
plot(survdist)





## Load survival
library(survival)

## Load foreign package for Stata data
library(foreign)

obj<-with(survival_data,Surv(completion_lag,completed==1))
res.km<-survfit(formula=obj~type,data=survival_data)
plot(res.km,col=c("red","blue","green","yellow"))


#CoxPh model 
res.cox <- coxph(formula = obj ~ type, data = survival_data)
summary(res.cox)

#Exponential Model
res.exp <- survreg(formula = obj ~ type, data = survival_data, dist = "exponential")
summary(res.exp)


y<-survival_data$consent_quarter_new
cbind(freq=table(y),percentage=prop.table(table(y))*100)


#Prediction based on probability matrix
#Probability transition matrix was created to understand the completion probability across time period
head(survival_data)
P<-filter(survival_data, consent_quarter_new > 2008.4 & date_completed > 2008-12-12)
head(P)
P$newconsent <- as.yearqtr(P$consent_date, format = "%Y-%m-%d")
P$newconsent<-format(P$newconsent, format = "%y/0%q")
head(P)
P.consenttable<-table(P$newconsent) 


P$newcompleted <- as.yearqtr(P$date_completed, format = "%Y-%m-%d")
P$newcompleted<-format(P$newcompleted, format = "%y/0%q")
head(P)
P.completedtable<-table(P$newcompleted) 

#This gives us contingency table based on consented and completed
t <- with(a, table(P$newconsent, P$newcompleted))
t <- prop.table(t, margin = 0)
t

#Where margin =1, gives us the probability. margin = 0 helps us in deriving the distribution of completed consents
t1 <- with(a, table(P$newconsent, P$newcompleted))
t1 <- prop.table(t, margin = 1)
options(max.print=999999)
t1

#Copy the above mentioned probabilities in excel and then multiply the dwellings consented with the probabilities.

summary(survival_testing)
dim(survival_testing)
Q<-filter(survival_testing, consent_year > 2008)
head(Q)
dim(Q)

#Changing the testing set to appropriate format and then filter
Q$newconsent <- as.yearqtr(Q$consent_date, format = "%Y-%m-%d")
Q$newconsent<-format(Q$newconsent, format = "%y/0%q")
head(Q)

###
o<-table(Q$newconsent) 
##
o

dat<-matrix(seq(as.Date(Q$newconsent)))
head(dat)
dim(dat)
dat<-data.frame(Date=Q$newconsent)



head(o)
t1
s<-o%*%t1
s

#The values retrived here can be pasted in an excel sheet


#Retrieve the ongoing data with year more than 2013
dim(merged)
head(merged)
ongoing<-filter(merged, consent_year > 2008 & ongoing==1)
head(ongoing)
dim(ongoing)


t4 <- with(ongoing, table(ongoing$consent_quarter, ongoing$ongoing))
t4
#t4 displays number of ongoing projects. Add t4 to the completed dwellings derived below

comp<-filter(merged, consent_year > 2008,completed==1)
comp$newconsent <- as.yearqtr(comp$consent_date, format = "%Y-%m-%d")
comp$newconsent<-format(comp$newconsent, format = "%y/0%q")
head(comp)
comp.consentedtable<-table(comp$newconsent) 
comp.consentedtable


#Will need the next line of code to get the probability, 

t <- with(a, table(P$newconsent, P$newcompleted))
t <- prop.table(t, margin = 0)
t

head(comp)
comp$date_completed_quarter <- as.yearqtr(comp$date_completed, format = "%Y-%m-%d")
comp$date_completed_quarter<-format(comp$date_completed_quarter, format = "%y/0%q")
head(comp)
#Now both consented and completed quarter are in the same format

#tp and tq are derived to aggregate dwellings consented and completed consents with their respective dates
tp<-aggregate(dwellings_consented~date_completed_quarter,data=comp,FUN=sum)
tp
tp[-c(1), ] #Remove the unwanted row 
tq<-aggregate(completedderived~newconsent,data=comp,FUN=sum)
tq 

#Changing the name of the variable to merge tp and tq, the below set of steps is for reference purpose, can be skipped
library(reshape)
names(tq)[1] <- "quarter"
names(tp)[1] <- "quarter"


u<-merge(tp,tq)
u



####to create a new dataset where no completion dates are available, which are the ones with NA's
summary(merged)
noncomp<-filter(merged, consent_year > 2008,is.na(merged$completed))
head(noncomp)
summary(noncomp)

#create a new column called consent  quarter which will be later used to summarise
noncomp$date_consented_quarter <- as.yearqtr(noncomp$consent_date, format = "%Y-%m-%d")
noncomp$date_consented_quarter<-format(noncomp$date_consented_quarter, format = "%y/0%q")

tr<-aggregate(dwellings_consented~date_consented_quarter,data=noncomp,FUN=sum)
tr
#tr gives number of dwellings consented from 2009 to 2017 

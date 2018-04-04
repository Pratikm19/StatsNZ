#Part1 Apartments with bigger values and lower value
library(dplyr)
t<-filter(survival_data, type == 1121)
dim(t)
head(t)
summary(t)
t1<-subset( t,dwellings_consented > 0)
dim(t1)
class(t1$dwellings_consented)
summary(t1)

t1<-mutate(t1,newvar=ifelse(dwellings_consented<=10,"smaller","bigger"))
head(t1)
summary(t1)
t1$newvar<-as.factor(t1$newvar)
class(survival_data$house)

#Creating survival probability and plotting
survival.objopo<-Surv(t1$completion_lag,t1$completed)
survdistopo<-survfit((survival.objopo)~newvar,data=t1)
summary(survdistopo,times = c(6,9,12*(1:10)))
plot(survdistopo,col=c("red","blue","green","yellow"), xlim = c(0,30),,ylab="completion probability",xlab="lag in months")




#Part 2 Kaplan Meier Analysis on various types of dwellings
summary(survival_data)
survival.obj<-Surv(survival_data$completion_lag,survival_data$completed)
survdist<-survfit((survival.obj)~1)
summary(survdist)
plot(survdist,xlab="lag period",ylab="Survival Probability")
survdist_type<-survfit((survival.obj)~type,data=survival_data)
summary(survdist_type,times = c(6,9,12*(1:10)))

plot(survdist,col=c("red","blue","green","yellow"), xlim = c(0,30))


#Part 3 Average time completion like by like, only high valued consents
t3<-subset( survival_data,log_deflated_project_value > 5.65)

##Average comopletion lag overall

tapply(t3$completion_lag, t3$type, mean)

#Average completion for 2011 - 2014
#filter
summary(t3)


avg1<-t3%>%
  select(completion_lag,date_completed,type)%>%
  filter(date_completed > "2011-1-1" & date_completed <"2013-12-31")
tapply(avg1$completion_lag, avg1$type, mean)


#Average completion for 2014-2017
avg2<-t3%>%
  select(completion_lag,date_completed,type)%>%
  filter(date_completed > "2014-1-1" & date_completed <"2017-12-31")
tapply(avg2$completion_lag, avg2$type, mean)

#Average completion for 2002-2005
avg4<-t3%>%
  select(completion_lag,date_completed,type)%>%
  filter(date_completed > "2002-1-1" & date_completed <"2004-12-31")
tapply(avg4$completion_lag, avg4$type, mean)



#Average completion for 2005-2008
avg5<-t3%>%
  select(completion_lag,date_completed,type)%>%
  filter(date_completed > "2005-1-1" & date_completed <"2007-12-31")
tapply(avg5$completion_lag, avg5$type, mean)





#Average completion for 2008-2011
avg6<-t3%>%
  select(completion_lag,date_completed,type)%>%
  filter(date_completed > "2008-1-1" & date_completed <"2010-12-31")
tapply(avg6$completion_lag, avg6$type, mean)





#PART 4 model based on past 3 years - 2010 - 2013

head(survival_data)
recent<-filter(survival_data,date_completed > "2010-1-1" & date_completed <"2013-12-31")

#Modelling
survival.obj_recent<-Surv(recent$completion_lag,recent$completed)
survdist_recent<-survfit((survival.obj_recent)~house,data=recent)
summary(survdist_recent,times = c(6,9,12*(1:10)))
plot(survdist_recent,col=c("red","blue","green","yellow"), xlim = c(0,30),,ylab="completion probability",xlab="lag in months")


#Chisq test
table(merged$completed,merged$type)

chisq.test(merged$completed,merged$type, correct=FALSE)
#There is a strong relation between type of dwelling and completion 




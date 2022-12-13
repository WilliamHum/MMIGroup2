library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(AER)
library(stargazer)
#should we do probit/logit?
#data cleaning
bigdataCase <- read_csv("shcwep-5362-E-2022_F1.csv")
dat <- filter(bigdataCase,(complete.cases(bigdataCase)))
dat <- filter(dat,GEN_20<=5) #mental health compared to before the pandemic
dat <- filter(dat,ICJ_05E<=2)#workload
dat <- filter(dat,ICJ_05L<=2)#unpaid leave
dat <- filter(dat,IM_10D<=2)#loneliness
dat <- filter(dat,GDRDVGRP<=2)#gender(m/f)
dat <- filter(dat,IM_10C<=2)#knows someone who died from covid
dat <- filter(dat,OCCDVGYW<=3)#the numbers of years in current occupation.
dat <- filter(dat, AGEDVGRP<=4)#age in four groups, youngest to oldest
dat <- filter(dat,ICJ_05D<=2)#different work task? potential instrument
dat <- filter(dat,ICJ_05I<=2)#loss of income 1 = yes 2 = no
dat <- filter(dat,ICJ_05C<=2)#more stressed at work due to COVID 19? 1 = yes 2 = no
dat <- filter(dat,ICJ_05B<=2)#more conflict 1 = yes 2 = no

#Modicification of variables (mostly changing things to binary/bernoulli)
dat <- mutate(dat,mental_health = ifelse(GEN_20 <= 3,0,1)) #changed to Bernoulli, 0 means the same or better
dat <- mutate(dat, workload = ifelse(ICJ_05E == 2,0,1)) # changed workload to Bernoulli
dat <- mutate(dat, unpaid_leave = ifelse(ICJ_05L == 1,1,0)) #unpaid leave is 1, didnt take is 0
dat <- mutate(dat, loneliness = ifelse(IM_10D == 1,1,0)) #1 is experienced loneliness due to covid, 0 is didnt
dat <- mutate(dat, male = ifelse(GDRDVGRP == 1,1,0)) #1 if male, 0 if female
dat <- mutate(dat, closedeath = ifelse(IM_10C == 2,0,1)) #0 if no, 1 if yes ; no significant effect
dat <- mutate(dat, NumberYearFac = as.factor(OCCDVGYW)) # Less than 10 years / 10 to 19 years / 20 years or more
dat <- mutate(dat, AgeFac = as.factor(AGEDVGRP)) # divide age into different segmentation
dat <- mutate(dat, diff_task = ifelse(ICJ_05D == 1,0,1)) #1 is different task
dat <- mutate(dat, incomeloss = ifelse(ICJ_05I == 2,0,1)) # 1 is loss of income due to COVID-19 pandemic
dat <- mutate(dat, more_stress = ifelse(ICJ_05C == 2,0,1)) #1 is more stressed from COVID-19 pandemic
dat <- mutate(dat, more_conflict = ifelse(ICJ_05B == 2,0,1))# 1 is more conflict between employees and management due to covid

#THIS DOESNT WORK FOR SOME REASON
#dat["LMAGNOC"][dat["LMAGNOC"] == "2"] <- "Nurse" #LMAGNOC is occupation within healthcare industry
#dat["LMAGNOC"][dat["LMAGNOC"] == "3"] <- "Supportworker" #this is personal support worker or care aides
#dat["LMAGNOC"][dat["LMAGNOC"] == "4"] <- "Other"
#colnames(dat)[colnames(dat) == "LMAGNOC"] ="occupation"
#dat <- mutate(dat, occupation=as.factor(occupation)) #1 was physicians, which are dummy variables

#initial testing for effect of workload on mental health
graphdat <- dat %>% group_by(workload) %>% summarize(mental_health = mean(mental_health))
ggplot(graphdat) + geom_bar(aes(x = workload, y = mental_health, fill = workload), stat = 'identity') + 
  #annotate("text", x = 1.5, y = 1.2, label = "p-value < 2.2e-16", size = 5, col='red') + 
  xlab('Impact of COVID-19 on workload (0 = Did not have increased workload, 1 = increased workload') + ylab('Mental Health: 0 = same or better, 1 = worse') + theme_classic() + 
  theme(legend.position = '')

t.test(mental_health ~ workload, data=dat) #checking if workload affects mental health significantly (it does)
ln0 <- lm(mental_health ~ workload, data=dat) #single variable OLS
summary(ln0) #so far increased workload increases probability of having worse mental health by 22.15%

#Finding our model
full_model <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac, data=dat) #unpaid leave probably related to workload
summary(full_model)
#Interactions: male is not an interaction, occupation may not work
#Notes: maybe remove NumberYearFac

# instrument: Age group of respondents in increments of 10
#             the number of years that the respondent has worked in their current occupation.
#             change method of delivery of health care
#             different work task

#instrument testing
iv1 <- lm(workload ~ diff_task + unpaid_leave + loneliness + male + AgeFac + NumberYearFac, data=dat) #testing an instrument (diffTask)
summary(iv1)
iv2 <- ivreg(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac | diff_task + unpaid_leave + loneliness + male + AgeFac + NumberYearFac, data=dat)
summary(iv2)



#"preliminary regressions"
ln1 <- lm(mental_health ~ workload, data=dat)
ln2 <- lm(mental_health ~ workload + unpaid_leave, data=dat)
ln3 <- lm(mental_health ~ workload + unpaid_leave + loneliness, data=dat)
ln4 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male, data=dat)
ln5 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac, data=dat)
ln6 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac, data=dat)
ln7 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss, data=dat)
ln8 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress, data=dat)
ln9 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)

#summary(ln9)

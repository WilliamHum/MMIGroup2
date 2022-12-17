# detach("package:stargazer",unload=T)
# # Delete it
# remove.packages("stargazer")
# # Download the source
# download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# # Unpack
# untar("stargazer_5.2.3.tar.gz")
# # Read the sourcefile with .inside.bracket fun
# stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# # Move the length check 5 lines up so it precedes is.na(.)
# stargazer_src[1990] <- stargazer_src[1995]
# stargazer_src[1995] <- ""
# # Save back
# writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# # Compile and install the patched package
# install.packages("stargazer", repos = NULL, type="source")

library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(AER)
library(stargazer)

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
dat <- filter(dat,LMAGNOC<=4)#occupation from 1 to 4

#Modification of variables (mostly changing things to binary/bernoulli)
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
dat <- mutate(dat, occupation = as.factor(LMAGNOC))

#initial testing for effect of workload on mental health
graphdat <- dat %>% group_by(workload) %>% summarize(mental_health = mean(mental_health))
ggplot(graphdat) + geom_bar(aes(x = workload, y = mental_health, fill = workload), stat = 'identity') + 
  #annotate("text", x = 1.5, y = 1.2, label = "p-value < 2.2e-16", size = 5, col='red') + 
  xlab('Impact of COVID-19 on workload (0 = Did not have increased workload, 1 = increased workload') + ylab('Mental Health: 0 = same or better, 1 = worse') + theme_classic() + 
  theme(legend.position = '')

t.test(mental_health ~ workload, data=dat) #checking if workload affects mental health significantly (it does)
ln0 <- lm(mental_health ~ workload, data=dat) #single variable OLS
summary(ln0) #so far increased workload increases probability of having worse mental health by 22.15%

#instrument testing (diff_task)
iv1 <- lm(workload ~ diff_task + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat) #testing an instrument (diffTask)
summary(iv1)
iv2 <- ivreg(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict| diff_task + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
summary(iv2)

regtest <-lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
summary(regtest)

#regressions
ln1 <- lm(mental_health ~ workload, data=dat)
ln2 <- lm(mental_health ~ workload + unpaid_leave, data=dat)
ln3 <- lm(mental_health ~ workload + unpaid_leave + loneliness, data=dat)
ln4 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male, data=dat)
ln5 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac, data=dat)
ln6 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac, data=dat)
ln7 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss, data=dat)
ln8 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress, data=dat)
ln9 <- lm(mental_health ~ workload + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
#ln9 is the full regression
ln10 <- lm(mental_health ~ workload*male + unpaid_leave + loneliness + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
#ln10 shows there is no interaction between workload and male
ln11 <- lm(mental_health ~ workload*AgeFac + unpaid_leave + loneliness + male + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
#ln11 shows there is no interaction between workload and AgeFac
ln12 <- lm(mental_health ~ workload*occupation + unpaid_leave + loneliness + male + AgeFac + NumberYearFac + incomeloss + more_stress + more_conflict, data=dat)
#ln12 shows that there is an interaction with occupation (physicians tend to be the most negatively affected by increased workload)
stargazer(ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8, ln9, iv2, ln10, ln11, ln12, type = 'text', 
          add.lines = list(c('added variable','workload','unpaid leave','loneliness','male','AgeFac','NumberYearFac','incomeloss','more stress','more conflict','IV','male interaction','age group interaction','occupation interaction')),
          out="table1.txt")

#WALD TEST
reg_restricted <- lm(mental_health ~ 1,data=dat)
waldtest(ln9, reg_restricted, vcov=vcovHC(ln9))
#Wald Test showed that all of the coefficients are significant
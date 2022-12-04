library(tidyverse)
library(ggplot2)

dat_unclean <- read.csv("shcwep-5362-E-2022_F1.csv")
dat <- filter(dat_unclean,(complete.cases(dat_unclean)))
dat <- filter(dat,GEN_20<=5)
dat <- filter(dat,ICJ_05E<=2)#2 = no
dat <- filter(dat,ICJ_05L<=2)#1 is took unpaid leave, 2 is did not take unpaid leave
dat <- filter(dat,IM_10D<=2)#1 is loneliness due to covid, 2 is none
dat <- mutate(dat,mental_health = ifelse(GEN_20 <= 3,0,1)) #changed to Bernoulli, 0 means the same or better
dat <- mutate(dat, workload = ifelse(ICJ_05E == 2,0,1)) # changed workload to Bernoulli
dat <- mutate(dat, unpaidleave = ifelse(ICJ_05L == 1,1,0)) #unpaid leave is 1, didnt take is 0
dat <- mutate(dat, loneliness = ifelse(IM_10D == 1,1,0)) #1 is experienced loneliness due to covid, 0 is didnt

graphdat <- dat %>% group_by(workload) %>% summarize(mental_health = mean(mental_health))
ggplot(graphdat) + geom_bar(aes(x = workload, y = mental_health, fill = workload), stat = 'identity') + 
  #annotate("text", x = 1.5, y = 1.2, label = "p-value < 2.2e-16", size = 5, col='red') + 
  xlab('Impact of COVID-19 on workload (0 = Did not have increased workload, 1 = increased workload') + ylab('Mental Health: 0 = same or better, 1 = worse') + theme_classic() + 
  theme(legend.position = '')

t.test(mental_health ~ workload, data=dat) #checking if workload affects mental health significantly (it does)
ln1 <- lm(mental_health ~ workload, data=dat) #single variable OLS
summary(ln1) #so far increased workload increases probability of having worse mental health by 22.15%

#adding other variables
ln2 <- lm(mental_health ~ workload + unpaidleave + loneliness, data=dat) #unpaid leave probably related to workload
summary(ln2)

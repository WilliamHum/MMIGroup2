## R lesson demo 2


library(tidyverse)
library(ggplot2)

BRFSS_2018 <- read_csv("D:/Dropbox/Teaching/2021/MMI1020/data/brfss health survey/BRFSS_2018.csv")

## clean the data

dat <- filter(BRFSS_2018, complete.cases(BRFSS_2018))
dat <- filter(dat, alcday5 <= 7)

## t test
?t.test

t.test(alcday5 ~ sex1, data=dat)


## t test by hand
dat %>% group_by(sex1) %>% summarize(mean(alcday5), sd(alcday5), n())
abs((0.513 - 0.932)/sqrt((1/140990)*1.36^2 + (1/99773)*1.84^2))

## plotting the conditional mean difference

graphdat <- dat %>% group_by(sex1) %>% summarize(alcday5 = mean(alcday5))

graphdat <- mutate(graphdat, female = ifelse(sex1 == 1, 'Female', 'Male'))


ggplot(graphdat) + geom_bar(aes(x = female, y = alcday5, fill = female), stat = 'identity') + 
  annotate("text", x = 1.5, y = 1.2, label = "p-value < 2.2e-16", size = 5, col='red') + 
  xlab('') + ylab('Days per week where alcohol was had') + theme_classic() + 
  theme(legend.position = '')










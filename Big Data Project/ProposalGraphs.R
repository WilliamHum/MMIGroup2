library(tidyverse)
library(ggplot2)

dat_unclean <- read.csv("shcwep-5362-E-2022_F1.csv")
dat <- filter(dat_unclean,(complete.cases(dat_unclean)))
dat <- filter(dat,GEN_20<=5)
dat <- filter(dat,ICJ_05E<=2)#2 = no
graphdat <- dat %>% group_by(ICJ_05E) %>% summarize(GEN_20 = mean(GEN_20))
ggplot(graphdat) + geom_bar(aes(x = ICJ_05E, y = GEN_20, fill = ICJ_05E), stat = 'identity') + 
  #annotate("text", x = 1.5, y = 1.2, label = "p-value < 2.2e-16", size = 5, col='red') + 
  xlab('Impact of COVID-19 on workload (1 = increased workload, 2 = Did not have increased workload') + ylab('Perceived Mental Health Compared to Before the Pandemic (1-5)') + theme_classic() + 
  theme(legend.position = '')

t.test(GEN_20 ~ ICJ_05E, data=dat)
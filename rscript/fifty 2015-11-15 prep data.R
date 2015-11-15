library(plyr)
library(dplyr)
library(reshape2)
library(mtusRlocal)
library(ggplot2)

library(foreign)
atus65 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1965hfep.SAV', to.data.frame = T)
atus75 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1975hfep.sav', to.data.frame = T)

atus65$id = paste(atus65$hhid, atus65$pid, sep = '_') 
atus75$id = paste(atus75$hhid, atus75$pid, sep = '_') 

head(atus75)

n_distinct(atus65$diaryday)
n_distinct(atus75$diaryday)

atus75 = atus75 %>% left_join(., distinct(atus75, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )
atus75$n_days

# summarise 
atsum65 = atus65 %>% group_by(id, main) %>% summarise(stime = sum(time)) %>% mutate(st = sum(stime))
atsum75 = atus75 %>% group_by(id, main, n_days) %>% summarise(stime = sum(time)) %>% group_by(id) %>% mutate(st = sum(stime) / n_days)

n_distinct(atsum65$id)
n_distinct(atsum75$id)

atsum65 %>% group_by(main) %>% mutate(sum(stime) / n_distinct(id))
atsum75 %>% group_by(main) %>% mutate(sum(stime) / n_distinct(id))

TimeClock(841)

atus65 %>% group_by(id, alone) %>% summarise(stime = sum(time)) %>% group_by(alone) %>% summarise(sum(stime) / n_distinct(id))

atus65 %>% group_by(id, alone, diaryday) %>%
  summarise(stime = sum(time)) %>% 
  group_by(alone, diaryday) %>% summarise(meanalone = sum(stime) / n_distinct(id)) %>%
  filter(alone == 'yes') %>% 
  ggplot(aes(x=diaryday, y=meanalone, fill=diaryday)) +
  geom_bar(stat="identity")

ggplot(mtc,aes(x=factor(gear), y=wt)) + stat_summary(fun.y=mean, geom="bar")

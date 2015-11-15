library(plyr)
library(dplyr)
library(reshape2)
library(mtusRlocal)
library(ggplot2)
library(tidyr)
library(foreign)

###########
# Notes : This file explore the possibility of comparing 60s with the 2000s with whom files 
###########

atus65 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1965hfep.SAV', to.data.frame = T)
atus75 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1975hfep.sav', to.data.frame = T)
atus85 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1985hfep.sav', to.data.frame = T)
atus93 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA1993hfep.SAV', to.data.frame = T)
atus03_12 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA0312hfsum.sav', to.data.frame = T)

head(atus03)

# personal id ! 
atus03_12$id = paste(atus03_12$hhid, atus03_12$pid, sep = '_') 

atus65$id = paste(atus65$hhid, atus65$pid, sep = '_') 
atus75$id = paste(atus75$hhid, atus75$pid, sep = '_') 
atus85$id = paste(atus85$hhid, atus85$pid, sep = '_') 
atus93$id = paste(atus93$hhid, atus93$pid, sep = '_') 
atus03$id = paste(atus03$hhid, atus03$pid, sep = '_') 

head(atus75)

n_distinct(atus65$id)
n_distinct(atus75$id)
n_distinct(atus85$id)
n_distinct(atus93$id)
n_distinct(atus03$id)

# creating the numbers of days 
atus65 = atus65 %>% left_join(., distinct(atus65, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )
atus75 = atus75 %>% left_join(., distinct(atus75, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )
atus85 = atus85 %>% left_join(., distinct(atus85, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )
atus93 = atus93 %>% left_join(., distinct(atus93, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )
atus03 = atus03 %>% left_join(., distinct(atus03, id, diaryday) %>% group_by(id) %>% summarise(n_days = n()) )

atus65$n_days
atus75$n_days
atus85$n_days
atus93$n_days
atus03$n_days %>% table

# summarise 
atsum65 = atus65 %>% group_by(id, main, n_days) %>% summarise(stime = sum(time)) %>% group_by(id) %>% mutate(st = sum(stime) / n_days) 
atsum75 = atus75 %>% group_by(id, main, n_days) %>% summarise(stime = sum(time)) %>% group_by(id) %>% mutate(st = sum(stime) / n_days) 
atsum85 = atus85 %>% group_by(id, main, n_days) %>% summarise(stime = sum(time)) %>% group_by(id) %>% mutate(st = sum(stime) / n_days) 

atsum65 = atsum65 %>% group_by(id, main, n_days) %>% mutate(meanid = stime / n_days)
atsum75 = atsum75 %>% group_by(id, main, n_days) %>% mutate(meanid = stime / n_days)
atsum85 = atsum85 %>% group_by(id, main, n_days) %>% mutate(meanid = stime / n_days)

n_distinct(atsum65$id)
n_distinct(atsum75$id)
n_distinct(atsum85$id)

######
# summarise act 
######

### 65 
atsum65_mean = atsum65 %>% group_by(main) %>% summarise(meantime = sum(meanid) / n_distinct(id))
atsum65_mean$nrow = 1
atsum65_meansp = atsum65_mean %>% spread(main, meantime) %>% as.data.frame()
# ggplot2 
atsum65_mean %>% ggplot(aes(x=main, y=meantime, fill=main)) + geom_bar(stat="identity") + theme_minimal()

### 75 
atsum75_mean = atsum75 %>% group_by(main) %>% summarise(meantime = sum(meanid) / n_distinct(id))
atsum75_mean$nrow = 1
atsum75_meansp = atsum75_mean %>% spread(main, meantime) %>% as.data.frame()
# ggplot2 
atsum75_mean %>% ggplot(aes(x=main, y=meantime, fill=main)) + geom_bar(stat="identity") + theme_minimal()

### 85 
atsum85_mean = atsum85 %>% group_by(main) %>% summarise(meantime = sum(meanid) / n_distinct(id))
atsum85_mean$nrow = 1
atsum85_meansp = atsum85_mean %>% spread(main, meantime) %>% as.data.frame()
# ggplot2 
atsum85_mean %>% ggplot(aes(x=main, y=meantime, fill=main)) + geom_bar(stat="identity") + theme_minimal()

#
atsum65_meansp
atsum75_meansp
atsum85_meansp

atsum65_mean[grepl('sleep', atsum65_mean$main), ] 
atsum75_mean[grepl('sleep', atsum75_mean$main), ] 
atsum85_mean[grepl('sleep', atsum85_mean$main), ] 

atsum65_mean[grepl('child', atsum65_mean$main), ] 
atsum75_mean[grepl('child', atsum75_mean$main), ] 
atsum85_mean[grepl('child', atsum85_mean$main), ] 

# alone time 
atus65 %>% group_by(id, alone, n_days) %>% summarise(stime = sum(time)) %>% 
  group_by(id) %>% mutate(st = sum(stime) / n_days) %>% group_by(id, alone, n_days) %>% mutate(meanid = stime / n_days) %>% 
  group_by(alone) %>% summarise(sum(meanid) / n_distinct(id))

atus75 %>% group_by(id, alone, n_days) %>% summarise(stime = sum(time)) %>% 
  group_by(id) %>% mutate(st = sum(stime) / n_days) %>% group_by(id, alone, n_days) %>% mutate(meanid = stime / n_days) %>% 
  group_by(alone) %>% summarise(sum(meanid) / n_distinct(id))

# 
ggplot(aes(x=diaryday, y=meanalone, fill=diaryday)) +
geom_bar(stat="identity")


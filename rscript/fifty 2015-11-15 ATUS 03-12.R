library(plyr)
library(dplyr)
library(reshape2)
library(mtusRlocal)
library(ggplot2)
library(tidyr)
library(foreign)

###########
# Notes : This file explore the 03-12 series 
###########

# two main files to use 

# this one has been uploaded from the CTUR website 
atus03_12 = read.spss(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/atus_diaries/USA0312hfsum.sav', to.data.frame = T)

# this one from the AHTUS - it seems more complete 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/ATUS/ATUS2013/dataAtusGoodWithWhom.RData')
hatus_03_12 = dataAtusGoodWithWhom

#########
#########
# some checks 

# alone  
atus03_12_alone = atus03_12 %>% group_by(year) %>% summarise(meantimealone = sum(walone) / n_distinct(id))
plot(atus03_12_alone)

# partner 
atus03_12_partner = atus03_12 %>% group_by(year) %>% summarise(meantimealone = sum(wsppart) / n_distinct(id))
plot(atus03_12_partner)

# child 
atus03_12_child = atus03_12 %>% group_by(year) %>% summarise(meantimealone = sum(wchild) / n_distinct(id))
plot(atus03_12_child)

head(hatus_03_12) 

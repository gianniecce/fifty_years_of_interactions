
load('/Users/giacomovagni/Documents/Data/MTUS/Aggregate/MtusAggregate.RData')

table(MtusAggregate$countrya)
mtusUS = filter(MtusAggregate, countrya == 38)
mtusUS = filter(MtusAggregate, countrya == 38)

mtusUS$id = paste(mtusUS$hldid, mtusUS$persid, sep = '_') 

# working pop
mtusUSwpop = filter(mtusUS, age > 18 & age < 65)
# SUNDAY 
mtusUSwpop = filter(mtusUSwpop, day == 7)

table(mtusUSwpop$day) 
table(mtusUSwpop$n_days) 

mtusUSwpop = mtusUSwpop %>% left_join(., distinct(mtusUSwpop, id, diary) %>% group_by(id) %>% summarise(n_days = n()) )
mtusUSwpop = mtusUSwpop %>% left_join(., mtusUSwpop %>% group_by(survey, n_days) %>% summarise(nid = n_distinct(id) ) ) 

mtusUSwpop %>% select(survey, n_days, av7, nid) %>% group_by(survey, n_days, nid) %>% 
  summarise(sact = sum(av7)) %>% mutate(sact / nid) 

mtusUSwpop %>% select(survey, n_days, av7, nid) %>% group_by(survey, n_days, nid) %>% 
  summarise(mean = mean(av7), sd = sd(av7)) %>% filter(n_days == 1)

mtusUSwpop %>% select(survey, n_days, av1, nid) %>% group_by(survey, n_days, nid) %>% 
  summarise(mean = mean(av1), sd = sd(av1)) %>% filter(n_days == 1)

# gender 
mtusUSwpop %>% select(survey, n_days, av7, nid, sex) %>% group_by(survey, n_days, nid, sex) %>% 
  summarise(mean = mean(av7), sd = sd(av7)) %>% filter(n_days == 1) %>% 
  ggplot(aes(x=survey, y=mean, group = factor(sex), colour = factor(sex))) + geom_line(size=1.5) + theme_minimal()

# gender 
mtusUSwpop %>% select(survey, n_days, av11, nid, sex) %>% group_by(survey, n_days, nid, sex) %>% 
  summarise(mean = mean(av11), sd = sd(av11)) %>% filter(n_days == 1) %>% 
  ggplot(aes(x=survey, y=mean, group = factor(sex), colour = factor(sex))) + geom_line(size=1.5) + theme_minimal()

# 11 childcare
# 1 paid
# 16 sleep 
# 7 housework 

####


# working pop
mtusUSwpop = filter(mtusUS, age > 18 & age < 30)
# SUNDAY 
mtusUSwpop = filter(mtusUSwpop, day == 7)

mtusUSwpop = mtusUSwpop %>% left_join(., distinct(mtusUSwpop, id, diary) %>% group_by(id) %>% summarise(n_days = n()) )
mtusUSwpop = mtusUSwpop %>% left_join(., mtusUSwpop %>% group_by(survey, n_days) %>% summarise(nid = n_distinct(id) ) ) 

head(mtusUSwpop)
mtusUSwpop %>% group_by(survey) %>% summarise(n())
mtusUSwpop %>% group_by(survey) %>% summarise(mean(av1), sd(av1))

mtusUSwpop %>% group_by(survey) %>% summarise(mean(sppart), sd(av1))



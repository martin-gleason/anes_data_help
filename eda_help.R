library("tidyverse")

library(tidyverse)
library(data.table)


rawanes <- fread("anes_timeseries_2016_rawdata.txt")

dim(rawanes)

crackpots <- rawanes %>% select ("school_vac" = V162146, 
                                 "vac_bene" = V162161,
                                 "nine_el"= V162254,
                                 "obama_muslim" = V162255, 
                                 "trust_wash" = V161215, 
                                 "trust_others" = V161219,
                                 "gun_access" = V161188,
                                 "gun_limits" = V161187) %>% 
  filter(school_vac >= 1 & vac_bene>= 1 & nine_el>= 1 &
           obama_muslim>= 1 & trust_wash >= 1 & trust_others >= 1 & 
           gun_access >= 1 & gun_limits>= 1)


# Obersations, rows, col, etc
sum(crackpots$school_vac)

ncol(crackpots)
nrow(crackpots)

# crackpots$school_vac <- as.factor(crackpots$school_vac)

crackpots <- crackpots %>% map(as.factor)
crackpots_int <- crackpots %>% map(as.integer)

crackpot_table <- table(crackpots) #table function

crackpot_prob <- prop.table(crackpot_table) #probabitly table

vector <- c(1, 2, 3)


maybelater <- crackpots %>% 
  group_by(gun_access) %>%
  summarise(counts = n(),
            vac_OK =mean(school_vac == 1), 
            vac_DUNO=mean(school_vac == 3),
            vac_BAD = mean(school_vac == 2))

crackpots %>%
  ggplot(aes(x=nine_el)) + geom_histogram()


ggplot(crackpots, aes(x=gun_access, fill = as.factor(school_vac))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)

ggplot(data = crackpots) +
  geom_bar(mapping = aes(x = gun_access, fill = as.factor(school_vac)))


#Trying to get the wickham plot
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

#linear model formula

trust_v_gun_ownership <- 
class(trust_v_gun_ownership)

crackpots %>% ggplot(aes(x = trust_others, fill = as.factor(obama_muslim))) + 
  geom_bar(position = "fill")

glm(formula = , family = "gaussian", data = crackpots)

lm(formula = ~ trust_others, data = crackpots)

lm(~ trust_others,
   data = crackpots_int)

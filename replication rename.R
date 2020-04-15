
# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)

# Load data ---------------------------------------------------------------

dat <- haven::read_dta(here::here("Replication Data.dta"))

# Data Wrangling ----------------------------------------------------------

#Setting variables as numeric in order to be able to treat them as numeric values.
dat <- dat %>% 
  mutate(Q20 = as.numeric(Q20)) %>%
  drop_na(Q20) %>% 
  mutate(Q43 = as.numeric(Q43)) %>% 
  drop_na(Q43)
#Creating groups for different experimental conditions

#Group A - control group
A_control <- dat %>% 
  filter(control == 1) %>% # Selecting only observations from the control group
  # Recoding the Likert scale from 0 to 1. This will be applied to each of the study groups.
  mutate(anger = ifelse(Q20 == 1, 0,
                        ifelse(Q20 == 2, 0.25,
                               ifelse(Q20 == 3, 0.50,
                                      ifelse(Q20 == 4, 0.75,
                                             ifelse(Q20 == 5, 1, 0)))))) %>%
  mutate(shame = ifelse(Q43 == 1, 0,
                        ifelse(Q43 == 2, 0.25,
                               ifelse(Q43 == 3, 0.50,
                                      ifelse(Q43 == 4, 0.75,
                                             ifelse(Q43 == 5, 1, 0)))))) %>% 
  mutate(punitive = ifelse(Q28 == 1, 1,
                        ifelse(Q28 == 2, 0.75,
                               ifelse(Q28 == 3, 0.50,
                                      ifelse(Q28 == 4, 0.25,
                                             ifelse(Q28 == 5, 0, 0)))))) #This scale has to be recoded other way round
  
#Group B - White-on-Black
B_wob <- dat %>% 
  filter(wob == 1) %>% 
  mutate(anger = ifelse(Q20 == 1, 0,
                        ifelse(Q20 == 2, 0.25,
                               ifelse(Q20 == 3, 0.50,
                                      ifelse(Q20 == 4, 0.75,
                                             ifelse(Q20 == 5, 1, 0)))))) %>%
  mutate(shame = ifelse(Q43 == 1, 0,
                        ifelse(Q43 == 2, 0.25,
                               ifelse(Q43 == 3, 0.50,
                                      ifelse(Q43 == 4, 0.75,
                                             ifelse(Q43 == 5, 1, 0)))))) %>% 
  mutate(punitive = ifelse(Q28 == 1, 1,
                           ifelse(Q28 == 2, 0.75,
                                  ifelse(Q28 == 3, 0.50,
                                         ifelse(Q28 == 4, 0.25,
                                                ifelse(Q28 == 5, 0, 0))))))
  
#Group C - White-on-White
C_wow <- dat %>% 
  filter(wow == 1) %>% 
  mutate(anger = ifelse(Q20 == 1, 0,
                        ifelse(Q20 == 2, 0.25,
                               ifelse(Q20 == 3, 0.50,
                                      ifelse(Q20 == 4, 0.75,
                                             ifelse(Q20 == 5, 1, 0)))))) %>% 
  mutate(shame = ifelse(Q43 == 1, 0,
                        ifelse(Q43 == 2, 0.25,
                               ifelse(Q43 == 3, 0.50,
                                      ifelse(Q43 == 4, 0.75,
                                             ifelse(Q43 == 5, 1, 0)))))) %>% 
  mutate(punitive = ifelse(Q28 == 1, 1,
                           ifelse(Q28 == 2, 0.75,
                                  ifelse(Q28 == 3, 0.50,
                                         ifelse(Q28 == 4, 0.25,
                                                ifelse(Q28 == 5, 0, 0))))))

#Group D - Black-on-White
D_bow <- dat %>% 
  filter(bow == 1) %>% 
  mutate(anger = ifelse(Q20 == 1, 0,
                        ifelse(Q20 == 2, 0.25,
                               ifelse(Q20 == 3, 0.50,
                                      ifelse(Q20 == 4, 0.75,
                                             ifelse(Q20 == 5, 1, 0)))))) %>% 
  mutate(shame = ifelse(Q43 == 1, 0,
                        ifelse(Q43 == 2, 0.25,
                               ifelse(Q43 == 3, 0.50,
                                      ifelse(Q43 == 4, 0.75,
                                             ifelse(Q43 == 5, 1, 0)))))) %>% 
  mutate(punitive = ifelse(Q28 == 1, 1,
                           ifelse(Q28 == 2, 0.75,
                                  ifelse(Q28 == 3, 0.50,
                                         ifelse(Q28 == 4, 0.25,
                                                ifelse(Q28 == 5, 0, 0))))))

#Group E - Black-on-Black
E_bob <- dat %>% 
  filter(bob == 1) %>% 
  mutate(anger = ifelse(Q20 == 1, 0,
                        ifelse(Q20 == 2, 0.25,
                               ifelse(Q20 == 3, 0.50,
                                      ifelse(Q20 == 4, 0.75,
                                             ifelse(Q20 == 5, 1, 0)))))) %>% 
  mutate(shame = ifelse(Q43 == 1, 0,
                        ifelse(Q43 == 2, 0.25,
                               ifelse(Q43 == 3, 0.50,
                                      ifelse(Q43 == 4, 0.75,
                                             ifelse(Q43 == 5, 1, 0)))))) %>% 
  mutate(punitive = ifelse(Q28 == 1, 1,
                           ifelse(Q28 == 2, 0.75,
                                  ifelse(Q28 == 3, 0.50,
                                         ifelse(Q28 == 4, 0.25,
                                                ifelse(Q28 == 5, 0, 0))))))


## T-Test to set p-value to 0-10 or is it set when doing the table?
## Jak udelam ten test

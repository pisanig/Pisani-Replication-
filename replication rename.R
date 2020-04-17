
# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(stargazer)

# Load data ---------------------------------------------------------------

dat <- haven::read_dta(here::here("Replication Data.dta"))

# Data Wrangling ----------------------------------------------------------

#Setting variables as numeric in order to be able to treat them as numeric values.
dat <- dat %>% 
  mutate(Q20 = as.numeric(Q20)) %>%
  drop_na(Q20) %>% 
  mutate(Q43 = as.numeric(Q43)) %>% 
  drop_na(Q43) %>% 
  mutate(Q28 = as.numeric(Q28)) %>% 
  drop_na(Q28)
  
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
  
#Creating reduced objects via selecting needed dependent variables

A <- summarize(A_control, anger_mean = mean(anger), shame_mean = mean(shame), punitive_mean = mean(punitive))

B <- summarize(B_wob, anger_mean = mean(anger), shame_mean = mean(shame), punitive_mean = mean(punitive))

C <- summarize(C_wow, anger_mean = mean(anger), shame_mean = mean(shame), punitive_mean = mean(punitive))

D <- summarize(D_bow, anger_mean = mean(anger), shame_mean = mean(shame), punitive_mean = mean(punitive))

E <- summarize(E_bob, anger_mean = mean(anger), shame_mean = mean(shame), punitive_mean = mean(punitive))

# dat (tgroup2) > vytvoreni nove promenne angry, shame, punitive a jeji recode > vytvoreni pěti nových variables A B C D E > olabelovani techto novych variables


install.packages("margins")
library(margins)  

test <- lm(A_control$anger ~ A_control)
  

anger_model_1 <- A$anger_mean - B$anger_mean

anger_model_2 <- B$anger_mean - E$anger_mean

anger_model_3 <- B$anger_mean - C$anger_mean


shame_model_1 <- A$shame_mean - B$shame_mean

shame_model_2 <- B$shame_mean - E$shame_mean

shame_model_3 <- B$shame_mean - C$shame_mean


punitive_model_1 <- A$punitive_mean - B$punitive_mean

punitive_model_2 <- B$punitive_mean - E$punitive_mean

punitive_model_3 <- C$punitive_mean - D$punitive_mean

punitive_model_4 <- B$punitive_mean - C$punitive_mean


# T-Test -------------------------------------------------------

# We have to run 4 T-Tests. Each one comparing one group with the control one. We are looking for difference between the mean of the group adn the control group. 

# ANGER

#Control and B
t.test(x = A_control$anger, y = B_wob$anger, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE) # The p-value for the Table 1 in the article is set to 10 % so I set the confidence interval for 90 %.

#Control and C
t.test(x = A_control$anger, y = C_wow$anger, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and D
t.test(x = A_control$anger, y = D_bow$anger, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and E
t.test(x = A_control$anger, y = E_bob$anger, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)


# SHAME

#Control and B
t.test(x = A_control$shame, y = B_wob$shame, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE) # The p-value for the Table 1 in the article is set to 10 % so I set the confidence interval for 90 %.

#Control and C
t.test(x = A_control$shame, y = C_wow$shame, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and D
t.test(x = A_control$shame, y = D_bow$shame, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and E
t.test(x = A_control$shame, y = E_bob$shame, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)


# PUNITIVE

#Control and B
t.test(x = A_control$punitive, y = B_wob$punitive, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and C
t.test(x = A_control$punitive, y = C_wow$punitive, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and D
t.test(x = A_control$punitive, y = D_bow$punitive, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)

#Control and E
t.test(x = A_control$punitive, y = E_bob$punitive, mu = 0, alternative = "two.sided", conf.level = 0.90, var.equal = TRUE, paired = FALSE)


# Making tables -----------------------------------------------------------

# Plotting ----------------------------------------------------------------

# MARGINS PLOT

figure_1 <- ggplot() +
  geom_histogram(aes(dat_figure_1))

figure_1


####TO DO
#### Stargazer type PŘEDĚLAT NA LATEX, TO UDELA V MARKDOWN DO PDF PEKNOU TABULKU.
#### # Jak ale nastavit tu p-value, co se bude odvijet od control group?

##PROBLEMS
## number of individuals is different than in the original study (difference in one observation) in group three and four





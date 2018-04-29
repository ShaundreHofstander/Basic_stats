# Day 6
# Confidence Intervals
# 26 April 2018
# Shaundre'


# Confidence intervals ----------------------------------------------------


# Range the estimates of the mean 
# 95% fall in that particular range and 5% will fall outside of this

# Load library
library(rcompanion)
library(ggplot2)

# dataset
Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
# calculates number of steps in the entire data set e.g. mean steps 7690 for whole class

# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95, digits = 3)

# two-way data
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3)

# plot mean as point and around this the CI, and 
# the effect of teachers and sex on the mean value and CI


# Create a graph ----------------------------------------------------------

ggplot(data = dat1, aes(y = Mean, x = Sex))+
  geom_point(aes(colour = Teacher))+
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax =  Mean + Trad.upper, 
                    colour = Teacher))+
  facet_wrap(~ Teacher)

# by bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

groupwiseMean(Steps ~ Teacher + Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)


# Testing assumptions (Chapter 11) -----------------------------------------------------

# Normally distributed
# Variance homoscedasticity
# Data independant from each other
# Dependant variable must be continuous 

# Load library
library(tidyverse)
library(ggpubr)
# load data
chicks <- as_tibble(ChickWeight)

# Shapiro-Wilk normality test
shapiro.test(chicks$weight)
# below p value it is not normal, if it larger than p value it is normal
# if data is not normal perform Wilcoxon test


# summarise data
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# test homoscedasticity
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(var_wt = var(weight))

# log transform the data do natural log and log base 10
# the cube root and the square root

stuff <- data %>%
  mutate(ln_steps = log(Steps), 
         log_steps = log10(Steps),
         squared = sqrt(Steps),
         cubed = Steps^(1/3)) %>% 
  select(-Student, -Rating)


# create a panel of histograms

hist1 <- ggplot(data = stuff, aes( x = log_steps, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist2 <- ggplot(data = stuff, aes( x = ln_steps, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist3 <- ggplot(data = stuff, aes( x = squared, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist4 <- ggplot(data = stuff, aes( x = cubed, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

ggarrange(hist1, hist2, hist3, hist4)



# Iris data ---------------------------------------------------------------


iris_dat <- as.tibble(iris)

iris %>% 
  group_by(Species) %>% 
  summarise(none_dat = as.numeric(shapiro.test(Petal.Width)[2]))

# will find that of the species have non normal data
# do a kruskal-wallus test instead of an ANOVA
kruskal.test(Petal.Width ~ Species, data = iris)



# Exercises ---------------------------------------------------------------


# Find one of the days of measurement where the chicken weights do not pass 
# the assumptions of normality, and another day (not day 21!) in which they do.

# Load libraries
library(tidyverse)


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 20)

# Check assumptions
# The test rejects the hypothesis of normality when the p-value is less than or equal to 0.05. 
# Failing the normality test allows you to state with 95% confidence the data does not fit the 
# normal distribution. Passing the normality test only allows you to state no significant 
# departure from normality was found

# Test Normality
shapiro.test(chicks_sub$weight)

# But that is testing all of the data together
  group_by(Diet) %>% 
  summarise(c = as.numeric(shapiro.test(weight)[2]))
# therefore the data is normally distributed

chicks_sub1 <- chicks %>% 
    filter(Diet %in% c(1, 2), Time == 2)
  
# Test Normality
  shapiro.test(chicks_sub1$weight)
  
# p = 0.001, thus the data is not normally distributed

  
  
# Transform the data so that they may pass the assumptions ----------------

dat <- chicks_sub1 %>%
    mutate(ln_steps = log(weight), 
           log_steps = log10(weight),
           squared = sqrt(weight),
           cubed = weight^(1/3)) 

  

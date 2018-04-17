# github day and stats
# Day 1
# Shauny
# 12 April 2018
# To practice some of thr concepts that we will encounter


# load packages -----------------------------------------------------------

library(tidyverse)

# Integers ----------------------------------------------------------------

# Generate some integer data
integer_r <- as.integer(seq(5, 14, by = 1)) #this tells the computer thait is integer values in that specific seq(sequence), 
# by = the difference between the numners should be 1

# Look at a brief summary of them
summary(integer_r)


# Continuous --------------------------------------------------------------

# Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10) # length.out gives the desired length of the sequence)

# Dates -------------------------------------------------------------------
# one may perform some arithmetic with dates
as.Date("2005-12-16") - as.Date("2005-12-25")
# or for example
dates_r <- seq(as.Date("2005-12-16"),  as.Date("2005-12-25"), by = "day")
# There is much more
summary(dates_r)


# Data frames ----------------------------------------------------------

# create the base dataframe # must be the same length for it all to run
df_r <- data_frame(integers = integer_r, 
                   numeric = numeric_r,
                  dates = dates_r)

# Then tibbles
df_r <- as_tibble(df_r)
summary(df_r)


# Categories --------------------------------------------------------------

# Electronics
elec_r <- as.factor(c("laptops", # these are the different levels or categories
                      "desktops", 
                      "cell phones")) # as.factor = coerces its argument to a factor

# People
people_r <- as.factor(c("funny", 
                      "beautiful", 
                      "beanies"))

# Colours
colour_r <- as.factor(c("red", 
                        "blue"))


# Ordinal data ------------------------------------------------------------

# Here we still have qualitative data
# but with some sort of order
colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"), 
                       levels = c("blue", "green", "yellow", "orange", "red")) # ordered = this orders the data into a 
#specic format


# Binary data -------------------------------------------------------------

# These are generally represented as: TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)

# Characters --------------------------------------------------------------

sites_r <- c("Yzetrvarkpunt", "Betty's Bay", "Gansbaai", "Sea Point")


# Missing values -----------------------------------------------------------
# Number of eggs recorded in a nest
chicks_nes <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
# A summary
summary(chicks_nes)
# The mean
mean(chicks_nes)
# the standard deviation
sd(chicks_nes)


# Chap 3: Descriptive statistics ---------------------------------------------------------

# First create a dataframe
chicks <- as_tibble(ChickWeight)
chicks %>%
  summarise(chicken_count = n())#n gives the number of things
# or
nrow(chicks)#number of rows


# Measures of central tendacy ---------------------------------------------

# Calculate mean weight
chicks %>% 
  summarize(mean_wt = mean(weight))

#  Be more specific
chicks %>% 
  filter(Time == 21) %>% #== means equivalent to
  group_by(Diet) %>% 
  summarize(mean_wt = mean(weight),
            median_wt = median(weight))

# Visualize the density of the data
ggplot(data = filter(chicks, Time == 21), 
       aes(x = weight, fill = Diet))+
  geom_density(alpha = 0.4)
#creates vertical lines, shows the comparisons between the median and mean
             
# visualize the data by showing the density of the different diets based on its weight
# median and mean should be more or less the same , but sometimes not so
# but with the chickens, the mean is to the left of the median so its right skewed

# Skewness ----------------------------------------------------------------

# Calculate the numeric value


# compare difference in mean and median agaisnt skewness, right skewed is positive and left is negative (opposites)
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))


# Kurtosis ----------------------------------------------------------------
# First load libraries
library(e1071)

# best shape is bell curve
# Calculate the kurtosis of the tails of a distribution
chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))

exp_r <- data.frame(dat =rexp(n = 500),
                    sample = "A")

ggplot(data = exp_r, aes(x = dat))+
  geom_density()

kurtosis(exp_r$dat)


# Measures of Variability -------------------------------------------------------------

# below is a summmary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
                         wt_sd = sd(weight),
                         wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.50),
            wt_quart3 = quantile(weight, 0.75))

# visualise
wt_summary
